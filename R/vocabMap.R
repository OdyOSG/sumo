mapConceptToMesh <- function(conceptIds, cdm) {

  #check that cdm is a cdm_reference class
  checkmate::assertClass(cdm, "cdm_reference")

  res <- cdm$concept_relationship %>%
    dplyr::filter(.data$concept_id_1 %in% conceptIds,
                  .data$relationship_id == "Mapped from") %>%
    dplyr::left_join(cdm$concept, by = c("concept_id_2" = "concept_id")) %>%
    dplyr::filter(.data$vocabulary_id == "MeSH") %>%
    dplyr::select(concept_name) %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(res)
}

#' Function to build a search query based on omop concept ids
#' @param conceptIds a integer string of concept ids from OMOP vocabulary
#' @param cdm a cdm_reference object created using the CDMConnector package
#' @param explosion a toggle on whether or not to apply MeSH explosion to a query
#' @export
searchOmop <- function(conceptIds, cdm, explosion = TRUE) {
  mapConceptToMesh(conceptIds = conceptIds, cdm = cdm) %>%
    meshSearch(explosion = explosion)
}

mapMeshToConcept <- function(meshTerms, cdm) {

  #check that cdm is a cdm_reference class
  checkmate::assertClass(cdm, "cdm_reference")

  res <- cdm$concept %>%
    dplyr::filter(.data$concept_name %in% meshTerms, .data$vocabulary_id == "MeSH") %>%
    dplyr::left_join(cdm$concept_relationship, by = c("concept_id" = "concept_id_2")) %>%
    dplyr::left_join(cdm$concept, by = c("concept_id_1" = "concept_id")) %>%
    dplyr::select(concept_id_1, concept_name.y, concept_name.x, domain_id.y) %>%
    dplyr::collect() %>%
    dplyr::rename(MeSH_term = concept_name.x) %>%
    dplyr::rename(concept_name = concept_name.y) %>%
    dplyr::rename(domain_id = domain_id.y)

  return(res)
}

#' Function to add concept IDs/names/domains to a results dictionary
#' @param resDict A dictionary of keywords created via makeDict
#' @param cdm a cdm_reference object created using the CDMConnector package
#' @export
addConceptsToDict <- function(resDict, cdm){

  meshTerms <- resDict$MeSH_term

  conceptDict <- mapMeshToConcept(meshTerms, cdm) %>%
    dplyr::full_join(resDict,
                     by = c("MeSH_term" = "MeSH_term"))

  return(conceptDict)

}

#' Function that maps condition codes to icd 10 chapters
#' @param resDict A dictionary of keywords created via makeDict
#' @param cdm a cdm_reference object created using the CDMConnector package
#' @export
icd10Map <- function(resDict, cdm) {
  #add icd10 chapters to connection
  icd10 <-   tibble::tribble(
    ~precedence, ~category_name, ~category_id,
    1L, "Blood disease", 440371L,
    1L, "Blood disease", 443723L,
    2L, 'Injury and poisoning', 432795L,
    2L, 'Injury and poisoning', 442562L,
    2L, 'Injury and poisoning', 444363L,
    3L, 'Congenital disease', 440508L,
    4L, 'Pregnancy or childbirth disease', 435875L,
    4L, 'Pregnancy or childbirth disease', 4088927L,
    4L, 'Pregnancy or childbirth disease', 4154314L,
    4L, 'Pregnancy or childbirth disease', 4136529L,
    5L, 'Perinatal disease', 441406L,
    6L, 'Infection', 432250L,
    7L, 'Neoplasm', 438112L,
    8L, 'Endocrine or metabolic disease', 31821L,
    8L, 'Endocrine or metabolic disease', 4090739L,
    8L, 'Endocrine or metabolic disease', 436670L,
    9L, 'Mental disease', 432586L,
    10L, 'Nerve disease and pain', 376337L,
    10L, 'Nerve disease and pain', 4011630L,
    11L, 'Eye disease', 4038502L,
    12L, 'ENT disease', 4042836L,
    13L, 'Cardiovascular disease', 134057L,
    14L, 'Respiratory disease', 320136L,
    15L, 'Digestive disease', 4302537L,
    16L, 'Skin disease', 4028387L,
    17L, 'Soft tissue or bone disease', 4244662L,
    17L, 'Soft tissue or bone disease', 433595L,
    17L, 'Soft tissue or bone disease', 4344497L,
    17L, 'Soft tissue or bone disease', 40482430L,
    17L, 'Soft tissue or bone disease', 4027384L,
    18L, 'Genitourinary disease', 4041285L,
    19L, 'Iatrogenic condition', 4105886L,
    19L, 'Iatrogenic condition', 4053838L
  )
  #upload to src
  icd10 <- dplyr::copy_to(dest = attributes(cdm)$dbcon, df = icd10,
                          name = "icd10",
                          overwrite = TRUE)


  conceptIds <- resDict %>%
    dplyr::filter(domain_id == "Condition") %>%
    dplyr::pull(concept_id_1)

  #make a temp table on the concept ancestor table
  tmp <- cdm$concept_ancestor %>%
    dplyr::inner_join(icd10, by = c("ancestor_concept_id" = "category_id")) %>%
    dplyr::select(ancestor_concept_id, descendant_concept_id, category_name, precedence) %>%
    dplyr::rename(category_id = ancestor_concept_id)

  qq <- cdm$concept %>%
    dplyr::filter(concept_id %in% conceptIds) %>%
    dplyr::left_join(tmp, by = c("concept_id" = "descendant_concept_id")) %>%
    dplyr::mutate(
      category_id = dplyr::coalesce(category_id, 0L),
      category_name = dplyr::coalesce(category_name, 'Other Condition')
    ) %>%
    dplyr::group_by(concept_id) %>%
    dplyr::mutate(
      category_id = dplyr::first(category_id, order_by = precedence),
      category_name = dplyr::first(category_name, order_by = precedence)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(concept_id, concept_name, category_id, category_name) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  newResDict <- resDict %>%
    dplyr::left_join(qq %>% select(-concept_name), by = c("concept_id_1"= "concept_id"))

  return(newResDict)
}


#' Function that maps drug codes to atc2 class
#' @param resDict A dictionary of keywords created via makeDict
#' @param cdm a cdm_reference object created using the CDMConnector package
#' @export
atc2Map <- function(resDict, cdm) {

  #get atc2 vocab
  atc2 <- cdm$concept %>%
    dplyr::filter(
      vocabulary_id == 'ATC',
      concept_class_id == 'ATC 2nd'
      ) %>%
    mutate(
      precedence = dplyr::row_number(concept_code),
      category_id = concept_id,
      category_name = concept_name
    ) %>%
    dplyr::select(precedence, category_id, category_name)

  # concept ids to use
  conceptIds <- resDict %>%
    dplyr::filter(domain_id == "Drug") %>%
    dplyr::pull(concept_id_1)

  #make a temp table on the concept ancestor table
  tmp <- cdm$concept_ancestor %>%
    dplyr::inner_join(atc2, by = c("ancestor_concept_id" = "category_id")) %>%
    dplyr::select(ancestor_concept_id, descendant_concept_id, category_name, precedence) %>%
    dplyr::rename(category_id = ancestor_concept_id)

  qq <- cdm$concept %>%
    dplyr::filter(concept_id %in% conceptIds) %>%
    dplyr::left_join(tmp, by = c("concept_id" = "descendant_concept_id")) %>%
    dplyr::mutate(
      category_id = dplyr::coalesce(category_id, 0L),
      category_name = dplyr::coalesce(category_name, 'Other Drug')
    ) %>%
    dplyr::group_by(concept_id) %>%
    dplyr::mutate(
      category_id = dplyr::first(category_id, order_by = precedence),
      category_name = dplyr::first(category_name, order_by = precedence)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(concept_id, concept_name, category_id, category_name) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  newResDict <- resDict %>%
    dplyr::left_join(qq %>% select(-concept_name), by = c("concept_id_1"= "concept_id"))

  return(newResDict)
}

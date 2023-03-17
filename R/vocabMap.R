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



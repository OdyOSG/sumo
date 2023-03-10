mapConceptToMesh <- function(conceptIds, cdm) {

  #check that cdm is a cdm_reference class
  checkmate::assertClass(cdm, "cdm_reference")

  res <- cdm$concept_relationship %>%
    dplyr::filter(concept_id_1 %in% conceptIds,
                  relationship_id == "Mapped from") %>%
    dplyr::left_join(
      cdm$concept,
      by = c("concept_id_2" = "concept_id")
    ) %>%
    dplyr::filter(.data$vocabulary_id == "MeSH") %>%
    dplyr::select(concept_name) %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(res)
}

#' Function to build a search query based on omop concept ids
#' @param conceptIds a integer string of concept ids from OMOP vocabulary
#' @param cdm a cdm_reference object created using the CDMConnector package
#' @export
searchOmop <- function(conceptIds, cdm) {
  mapConceptToMesh(conceptIds = conceptIds, cdm = cdm) %>%
    meshSearch()
}

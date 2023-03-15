#' Function to build a dictionary of all the MeSH terms in a result
#' Prototype function used before MeSH mapping is complete
#' @param res A fetched sumo object containing PMIDs and key words
#' @param cdm a cdm_reference object created using the CDMConnector package
#' @param removeCommon A toggle on whether or not to remove the most common keywords
#' @export
makeDict <- function(res, cdm, removeCommon = TRUE){
  dict <- as.data.frame(table(unlist(strsplit(res$key_words, "; "))))
  colnames(dict) <- c("MeSH_term","count")
  dict <- dict[!dict$MeSH_term %in% commonKeywords(),]

  resDict <- addConceptsToDict(dict, cdm)

  return(resDict)
}

#' Function to pretty print Abstracts from a res object
#' @param res A fetched sumo object containing PMIDs and key words
#' @export
printAbstract <- function(res){

  resPrint <- res %>%
    dplyr::mutate(
      pmid = paste("PMID: ", .data$pmid, sep=""),
      paperInfo = paste(paste("<b>", .data$title, "</b>", sep = ""),
                        paste(.data$journal, ", ", .data$epubdate, sep = ""),
                        paste("<b>DOI:</b> ", .data$doi, sep=""), sep = "<br>"),
      abstract = paste(.data$abstract,"",sep="<br><br>"),
      key_words = paste("<b>MeSH terms:</b> ", .data$key_words, sep=""))

  if("concepts" %in% colnames(res)) {
    resPrint <- resPrint %>%
      dplyr::select(pmid, paperInfo, abstract, key_words, concepts) %>%
      dplyr::mutate(concepts = paste("<b>OMOP Concepts (ID):</b> ", .data$concepts, "<br>", sep="")) %>%
      tidyr::pivot_longer(cols = c("paperInfo","abstract","key_words","concepts"))

  } else {
    resPrint <- resPrint %>%
      dplyr::select(pmid, paperInfo, abstract, key_words) %>%
      tidyr::pivot_longer(cols = c("paperInfo","abstract","key_words"))
  }

  resPrint[,c(3)] %>%
    knitr::kable("html", escape = F, col.names = NULL) %>%
    kableExtra::pack_rows(
      index = setNames(
        rep(4,length(unique(resPrint$pmid))),
        unique(resPrint$pmid)
      )
    ) %>%
    kableExtra::kable_paper("hover", full_width = F)

}

conceptID_Map <- function(keywords, conceptDict) {

  conceptIds <- conceptDict[conceptDict$MeSH_term %in%
                              c(strsplit(keywords,";|; ")[[1]]) &
                          !is.na(conceptDict$concept_id_1),]$concept_id_1

  conceptIds <- paste(unlist(conceptIds),collapse = "; ")

  return(conceptIds)

}

conceptName_Map <- function(keywords, conceptDict) {

  conceptNames <- conceptDict[conceptDict$MeSH_term %in%
                              c(strsplit(keywords,";|; ")[[1]]) &
                              !is.na(conceptDict$concept_id_1),]$concept_name

  conceptNames <- paste(unlist(conceptNames),collapse = "; ")

  return(conceptNames)

}

#' Add dictionary information directly to the res file
#' @param conceptDict A dictionary of keywords created via makeDict with
#' attached OMOP concept name and ID information
#' @param res a cdm_reference object created using the CDMConnector package
#' @export
addDictToRes <- function(res, conceptDict){

  res$conceptIds <- unlist(lapply(res$key_words,FUN = conceptID_Map, conceptDict = conceptDict))
  res$conceptNames <- unlist(lapply(res$key_words,FUN = conceptName_Map, conceptDict = conceptDict))

  res <- res %>%
    dplyr::relocate(.data$conceptIds, .after = key_words) %>%
    dplyr::relocate(.data$conceptNames, .after = conceptIds) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(concepts = paste(unlist(strsplit(.data$conceptNames, ";|; "))," (",
                            unlist(strsplit(.data$conceptIds, ";|; ")),"); ",
                            collapse="",sep=""))

  return(res)

}

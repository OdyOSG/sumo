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

  # Comment: wondering if you could functionalize this a bit more? The concept select throws it off
  # a bit. Maybe functionalize the doi, paperInfo, and abstract mutates before the second select?

  if("concepts" %in% colnames(res)) {
    resPrint <- res %>%
      dplyr::select(pmid,title,abstract,journal,year,doi,key_words, concepts) %>%
      dplyr::mutate(doi = paste("doi: ", .data$doi, sep="")) %>%
      dplyr::mutate(paperInfo = paste(
        paste(.data$title, .data$journal, .data$year, sep = ",  "),
        "",.data$doi,"", sep="<br>")
        ) %>%
      dplyr::mutate(abstract = paste(.data$abstract,"",sep="<br><br>")) %>%
      dplyr::ungroup() %>% #why is there an ungroup, where is the groupby
      dplyr::select(pmid, paperInfo, abstract, key_words, concepts) %>%
      dplyr::mutate(pmid = paste("PMID: ", .data$pmid, sep="")) %>%
      dplyr::mutate(key_words = paste("MeSH terms: ", .data$key_words, sep="")) %>%
      dplyr::mutate(concepts = paste("OMOP Concepts (id): ", .data$concepts, sep="")) %>%
      tidyr::pivot_longer(cols = c("paperInfo","abstract","key_words","concepts"))

    resPrint[,c(3)] %>%
      knitr::kable("html", escape = F, col.names = NULL) %>%
      #this chunk is a bit awk via pipe
      #important you scope this, thought it was dplyr::group_rows at first
      kableExtra::group_rows(
        index = setNames(
          rle(resPrint$pmid)[[1]],
          rle(resPrint$pmid)[[2]]
          )
        ) %>%
      kableExtra::kable_paper("hover", full_width = F)

  } else {
    resPrint <- res %>%
      dplyr::select(pmid,title,abstract,journal,year,doi,key_words) %>%
      dplyr::mutate(doi = paste("doi: ", .data$doi, sep="")) %>%
      dplyr::mutate(paperInfo = paste(
        paste(.data$title, .data$journal, .data$year, sep = ",  "),
        "",.data$doi,"", sep="<br>")
      ) %>%
      dplyr::mutate(abstract = paste(abstract,"",sep="<br><br>")) %>%
      dplyr::ungroup() %>%
      dplyr::select(pmid, paperInfo, abstract, key_words) %>%
      dplyr::mutate(pmid = paste("PMID: ", pmid, sep="")) %>%
      dplyr::mutate(key_words = paste("MeSH terms: ", key_words, sep="")) %>%
      tidyr::pivot_longer(cols = c("paperInfo","abstract","key_words"))

    resPrint[,c(3)] %>%
      knitr::kable("html", escape = F, col.names = NULL) %>%
      #this chunk is a bit awk to read via pipe
      kableExtra::group_rows(
        index = setNames(
          rle(resPrint$pmid)[[1]],
          rle(resPrint$pmid)[[2]]
        )
      ) %>%
      kableExtra::kable_paper("hover", full_width = F)
  }

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
    dplyr::relocate(conceptIds, .after = key_words) %>%
    dplyr::relocate(conceptNames, .after = conceptIds) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(concepts = paste(unlist(strsplit(.data$conceptNames, ";|; "))," (",
                            unlist(strsplit(.data$conceptIds, ";|; ")),"); ",
                            collapse="",sep=""))

  return(res)

}

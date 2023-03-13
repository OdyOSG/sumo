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

  if("concepts" %in% colnames(res)) {
    resPrint <- res %>%
      select(pmid,title,abstract,journal,year,doi,key_words, concepts) %>%
      mutate(doi = paste("doi: ",doi,sep="")) %>%
      mutate(paperInfo = paste(paste(title, journal, year, sep = ",  "),"",doi,"",sep="<br>")) %>%
      mutate(abstract = paste(abstract,"",sep="<br><br>")) %>%
      ungroup() %>%
      select(pmid, paperInfo, abstract, key_words, concepts) %>%
      mutate(pmid = paste("PMID: ", pmid, sep="")) %>%
      mutate(key_words = paste("MeSH terms: ", key_words, sep="")) %>%
      pivot_longer(cols = c("paperInfo","abstract","key_words","concepts"))

    resPrint[,c(3)] %>%
      kable("html", escape = F, col.names = NULL) %>%
      group_rows(index = setNames(rle(resPrint$pmid)[[1]],
                                  rle(resPrint$pmid)[[2]])) %>%
      kable_paper("hover", full_width = F)

  } else {
    resPrint <- res %>%
      select(pmid,title,abstract,journal,year,doi,key_words) %>%
      mutate(doi = paste("doi: ",doi,sep="")) %>%
      mutate(paperInfo = paste(paste(title, journal, year, sep = ",  "),"",doi,"",sep="<br>")) %>%
      mutate(abstract = paste(abstract,"",sep="<br><br>")) %>%
      ungroup() %>%
      select(pmid, paperInfo, abstract, key_words) %>%
      mutate(pmid = paste("PMID: ", pmid, sep="")) %>%
      mutate(key_words = paste("MeSH terms: ", key_words, sep="")) %>%
      pivot_longer(cols = c("paperInfo","abstract","key_words"))

    resPrint[,c(3)] %>%
      kable("html", escape = F, col.names = NULL) %>%
      group_rows(index = setNames(rle(resPrint$pmid)[[1]],
                                rle(resPrint$pmid)[[2]])) %>%
      kable_paper("hover", full_width = F)
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
    rowwise() %>%
    mutate(concepts = paste(unlist(strsplit(conceptNames, ";|; "))," (",
                            unlist(strsplit(conceptIds, ";|; ")),"); ",
                            collapse="",sep=""))

  return(res)

}

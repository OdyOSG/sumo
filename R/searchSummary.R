#' Function to build a dictionary of all the MeSH terms in a result
#' Prototype function used before MeSH mapping is complete
#' @param res A fetched sumo object containing PMIDs and key words
#' @param removeCommon logical toggle to remove common keywords from dictionary
#' @export
makeDict <- function(res, removeCommon = TRUE){
  # TODO you ok switching to tibbles? Not pressing
  dict <- as.data.frame(table(unlist(strsplit(res$key_words, "; "))))
  colnames(dict) <- c("keyword","count")
  dict <- dict[!dict$keyword %in% commonKeywords(),]
  return(dict)
}

#' Function to generate summary plots of key_words from a dict object which
#' contains MeSH terms mapped to OMOP concept codes via SnoMED and RxNorm
#' @param res A fetched sumo object
#' @export
plotKeywords <- function(res){
  dict <- makeDict(res)
  #Needs MeSH mapping for domain-specific dictionary
  #Need domain-specific info for plotting

}

#' Function to pretty print Abstracts from a res object
#' @param res A fetched sumo object
#' @export
printAbstract <- function(res){
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

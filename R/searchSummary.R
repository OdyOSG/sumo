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

  #Remove concepts which have an ID but no name
  #What do these even refer to?
  resDict <- resDict %>%
    dplyr::filter(!(!is.na(.data$concept_id_1) & is.na(.data$concept_name == "")))

  #Add on extra domains for studies and stats
  resDict[grepl("Studies|Survey",resDict$MeSH_term,ignore.case = T) &
            is.na(resDict$domain_id),]$domain_id <- "Study"

  resDict[grepl("Index|Model|Estimate|Ratio",resDict$MeSH_term,ignore.case = T) &
            is.na(resDict$domain_id),]$domain_id <- "Statistic"

  #Mapping the few exceptions to the ggplot2 world_coordinates data and MeSH terms
  world_coordinates <- ggplot2::map_data("world")

  resDict[resDict$MeSH_term == "Republic of Korea",]$MeSH_term <- "South Korea"
  resDict[resDict$MeSH_term == "United States",]$MeSH_term <- "USA"

  resDict[grepl(paste(unique(world_coordinates$region),collapse="|"),
                resDict$MeSH_term,ignore.case = T) &
                is.na(resDict$domain_id),]$domain_id <- "Country"

  return(resDict)
}

#' Function to pretty print Abstracts from a res object
#' @param res A fetched sumo object containing PMIDs and key words
#' @export
printAbstract <- function(res){

  resPrint <- res %>%
    dplyr::mutate(
      pmid = paste("<b>PMID:</b> ", .data$pmid, sep=""),
      title = paste("<b>", .data$title, "</b>", sep = ""),
      key_words = paste("<b>MeSH terms:</b> ", .data$key_words, sep=""),
      abstract = gsub("<br><br>","<br><br>&emsp;",.data$abstract))

  if("concepts" %in% colnames(res)){
    resPrint <- resPrint %>%
      dplyr::mutate(concepts = paste("<b>OMOP Concepts (IDs):</b> ", .data$concepts))

    resPrint2 <- resPrint %>%
      dplyr::mutate(display =
                      paste(pmid,"<br><br>",
                            "&emsp;",title,"<br><br>",
                            "&emsp;",journal,", ",year,"<br><br>",
                            "&emsp;",abstract,"<br><br>",
                            "&emsp;",key_words,"<br><br>",
                            "&emsp;",concepts,"<br><br><br>",sep=""))

    resPrint2$display %>%
      knitr::kable("html", escape = F, col.names = NULL) %>%
      kableExtra::kable_paper(full_width = F)


  } else {
    resPrint2 <- resPrint %>%
      dplyr::mutate(display =
                      paste(pmid,"<br><br>",
                            "&emsp;",title,"<br><br>",
                            "&emsp;",journal,", ",year,"<br><br>",
                            "&emsp;",abstract,"<br><br>",
                            "&emsp;",key_words,"<br><br><br>",sep=""))

    resPrint2$display %>%
      knitr::kable("html", escape = F, col.names = NULL) %>%
      kableExtra::kable_paper(full_width = F)
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
    dplyr::relocate(.data$conceptIds, .after = key_words) %>%
    dplyr::relocate(.data$conceptNames, .after = conceptIds) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(concepts = paste(unlist(strsplit(.data$conceptNames, ";|; "))," (",
                            unlist(strsplit(.data$conceptIds, ";|; ")),"); ",
                            collapse="",sep=""))

  return(res)

}

#' Creates a cumulative sum matrix with dates as rows and key words as columns
#' @param res a cdm_reference object created using the CDMConnector package
#' @return cumuDate A matrix of the cumulative sum of key words by date
#' @export
cumuDate <- function(res){

  #Setup matrix with correct row/col names and lengths
  cumuDate <- as.data.frame(matrix(nrow = length(unique(res$epubdate)),
                                   ncol = length(unique(unlist(strsplit(unlist(res$key_words),";|; "))))))

  rownames(cumuDate) <- unique(res$epubdate)
  colnames(cumuDate) <- unique(unlist(strsplit(unlist(res$key_words),";|; ")))

  #Order both matrix and res by date
  cumuDate <- cumuDate[order(rownames(cumuDate)),]
  res <- res[order(res$epubdate),]

  #Set the first row to 0 occurences
  cumuDate[as.character(res[1,]$epubdate),] <- 0

  #Handle first row by adding one to each key word column where found
  for(term in unlist(strsplit(res[1,]$key_words,";|; "))) {
    cumuDate[as.character(res[1,]$epubdate),term] <- 1
  }

  #Iterate over each term found in each result row, first setting each row to the same
  #as the previous row, and then adding one to each key word column where found
  for(i in c(2:length(res$epubdate))){
    cumuDate[as.character(res[i,]$epubdate),] <-
      cumuDate[as.character(res[i-1,]$epubdate),]

    for(term in unlist(strsplit(res[i,]$key_words,";|; "))) {
      cumuDate[as.character(res[i,]$epubdate),term] <-
        cumuDate[as.character(res[i-1,]$epubdate),term] + 1
    }
  }
  return(cumuDate)
}

#' Outputs a bib file containing all result BibTeXs, derived via doi
#' @param res a cdm_reference object created using the CDMConnector package
#' @param outfile a string file name to store bib output
#' @export
exportBib <- function(res,outfile = "bibs.bib"){

  DOIlist <- res$doi

  h <- curl::new_handle()
  curl::handle_setheaders(h, "accept" = "application/x-bibtex")

  urls <- c()
  for (i in 1:length(DOIlist)) {
    urls <- c(urls,paste0("https://doi.org/", DOIlist[i]))
  }

  purrr::walk(urls, ~ {
    curl::curl(., handle = h) %>%
        readLines(warn = FALSE) %>%
        write(file = outfile, append = TRUE)
  })

}

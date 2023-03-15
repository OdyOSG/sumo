#' Search pubmed using defined search strategy
#' @param searchStrategy a search strategy built using sumo
#' @return an esearch list from the 'rentrez' package specifying the pmids resulting
#' from the search and count of hits
#' @export
searchPubmed <- function(searchStrategy) {
  #seach pubmed and get pmids
  hits <- rentrez::entrez_search(
    db = "pubmed",
    term = searchStrategy,
    use_history = TRUE,
    retmax = 10000
  )
  return(hits)
}


#' Fetch the results of a pubmed search
#' @param hits the ouptu of searchPubmed or an esearch list from the 'rentrez' package
#' @export
fetchPubmed <- function(hits) {
  # fetch the pmids and return xml
  res <- rentrez::entrez_fetch(
    db = "pubmed",
    web_history = hits$web_history,
    rettype = "xml"
  ) %>%
    rentrez::parse_pubmed_xml()

  tbl <- tibble::tibble(
    pmid = purrr::map_chr(res, ~.x$pmid),
    title = purrr::map_chr(res, ~.x$title),
    journal = purrr::map_chr(res, ~.x$journal),
    year = purrr::map_chr(res, ~.x$year),
    doi = purrr::map_chr(res, ~.x$doi),
    abstract = purrr::map(res, ~.x$abstract),
    key_words = purrr::map(res, ~.x$key_words)
  ) %>%
    dplyr::group_by(.data$pmid,.data$title,.data$journal,
                    .data$year,.data$doi,.data$key_words) %>%
    dplyr::summarise(abstract = paste(unlist(.data$abstract), collapse = "<br><br>")) %>%
    dplyr::group_by(.data$pmid,.data$title,.data$journal,
                    .data$year,.data$doi,.data$abstract) %>%
    dplyr::summarise(key_words = paste(unlist(.data$key_words),collapse = "; "))

  #Get denominator of search without keyword
  denom <- rentrez::entrez_search(
    db = "pubmed",
    term = removeKeywords(searchStrategy),
    retmax = 10000
  )$count

  tbl$abstract <- paste(tbl$abstract, "\n", sep="")
  tbl$count <- dim(tbl)[1]
  tbl$totalCount <- denom

  #Add date information
  dates <- rentrez::entrez_summary(
    db = "pubmed",
    web_history = hits$web_history) %>%
    rentrez::extract_from_esummary(c("uid","epubdate")) %>%
    as.data.frame() %>% t() %>% as.data.frame() %>%
    dplyr::rename(pmid=.data$uid)

  tbl <- merge(tbl,dates,by="pmid") %>%
    dplyr::mutate(epubdate = ifelse(.data$epubdate == "",
                                    paste(year," Jan 1",sep=""),.data$epubdate))

  tbl$epubdate <- lubridate::as_date(as.character(tbl$epubdate))

  return(tbl)
}

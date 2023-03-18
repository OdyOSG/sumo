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
#' @param searchStrategy a search strategy built using sumo
#' @export
fetchPubmed <- function(hits, searchStrategy) {
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
    dplyr::group_by(
      .data$pmid, .data$title, .data$journal,
      .data$year, .data$doi, .data$key_words
      ) %>%
    dplyr::summarise(
      abstract = paste(unlist(.data$abstract), collapse = "<br><br>")
      ) %>%
    dplyr::group_by(
      .data$pmid, .data$title, .data$journal,
      .data$year, .data$doi, .data$abstract
      ) %>%
    dplyr::summarise(
      key_words = paste(unlist(.data$key_words),collapse = "; ")
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      abstract = paste(.data$abstract, "\n", sep = ""),
      count = dplyr::n_distinct(pmid)
    )

  #Get denominator of search without keyword
  denom <- rentrez::entrez_search(
    db = "pubmed",
    term = removeKeywords(searchStrategy),
    retmax = 10000
  )$count

  #tbl$abstract <- paste(tbl$abstract, "\n", sep="")
  #tbl$count <-  nrow(tbl)
  tbl$totalCount <- denom

  #Add date information
  dates <- rentrez::entrez_summary(
    db = "pubmed",
    web_history = hits$web_history, retmode = "xml") %>%
    rentrez::extract_from_esummary(c("PubDate","EPubDate")) %>%
    as.data.frame() %>% t() %>% as.data.frame()

  dates[dates$EPubDate=="NULL",]$EPubDate <- dates[dates$EPubDate=="NULL",]$PubDate

  dates <- dates %>%
    dplyr::mutate(pmid = rownames(dates)) %>%
    dplyr::rename(epubdate=.data$EPubDate,) %>%
    dplyr::select(pmid,epubdate)

  quiet_date <- purrr::quietly(lubridate::as_date)

  tbl2 <- tbl %>%
    dplyr::left_join(dates, by = "pmid") %>%
    dplyr::mutate(
      epubdate = quiet_date(as.character(.data$epubdate))$result,
      epubdate2 = lubridate::ymd(.data$year, truncated = 2L),
      epubdate = dplyr::coalesce(epubdate, epubdate2)
    ) %>%
    dplyr::select(-epubdate2)


  return(tbl2)
}

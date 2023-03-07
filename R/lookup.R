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
    group_by(pmid,title,journal,year,doi,key_words) %>%
    summarise(abstract = paste(unlist(abstract), collapse = " ")) %>%
    group_by(pmid,title,journal,year,doi,abstract) %>%
    summarise(key_words = paste(unlist(key_words),collapse = "; "))

  return(tbl)
}

#' Search pubmed using defined search strategy
#' @param searchStrategy a search strategy built using sumo
#' @return an esearch list from the 'rentrez' package specifying the pmids resulting
#' from the search and count of hits
#' @export
searchPubmed <- function(searchStrategy) {
  #seach pubmed and get pmids
  hits <- rentrez::entrez_search(
    db = "pubmed",
    term = searchStrategy
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
    id = hits$ids,
    rettype = "xml"
  ) %>%
    rentrez::parse_pubmed_xml()

  #TODO deal with abstract that has four sections: purpose, methods, results, conclusion

  tbl <- tibble::tibble(
    pmid = purrr::map_chr(res, ~.x$pmid),
    title = purrr::map_chr(res, ~.x$title),
    journal = purrr::map_chr(res, ~.x$journal),
    year = purrr::map_chr(res, ~.x$year),
    doi = purrr::map_chr(res, ~.x$doi)
  )

  return(tbl)
}

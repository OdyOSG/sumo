# Internal ----------------
subSearch <- function(tag = c("journal", "mesh", "tiab", "keyword", "text"), items) {
  tag <- checkmate::assertChoice(
    tag,
    choices = c("journal", "mesh", "tiab", "keyword", "text")
  )
  searchTag <- switch(tag,
                      journal = "ta",
                      mesh = "mh",
                      tiab = "tiab",
                      keyword = "ot",
                      text = "tw"
  )
  jj <- purrr::map_chr(items, ~glue::glue("'{.x}'[{searchTag}]")) %>%
    paste(collapse = " OR ")
  txt <- paste0("(", jj, ")")
  return(txt)
}

#UI -----------------------
#' Function to build a search query based on journals
#' @param journals a character vector of journals to use in the search
#' @export
journalSearch <- function(journals) {
  subSearch(tag = "journal", items = journals)
}

#' Function to build a search query based on mesh terms
#' @param mesh a character vector of mesh terms to use in the search
#' @export
meshSearch <- function(mesh) {
  subSearch(tag = "mesh", items = mesh)
}

#' Function to build a search query based on terms in the title and abstract
#' @param mesh a character vector of terms in the title and abstract to use in the search
#' @export
tiabSearch <- function(tiab) {
  subSearch(tag = "tiab", items = tiab)
}

#' Function to build a search query based on keywords
#' @param keyword a character vector of keywords to use in the search
#' @export
keywordSearch <- function(keyword) {
  subSearch(tag = "keyword", items = keyword)
}

#' Function to build a search query based on text
#' @param text a character vector of text to use in the search
#' @export
textSearch <- function(text) {
  subSearch(tag = "text", items = text)
}

#' Function to add a date range to the search query
#' @param start the start date, must have format 'yyyy/mm/dd' as a character string
#' @param end the end date, must have format 'yyyy/mm/dd' as a character string
#' @export
dateRange <- function(start, end) {
  glue::glue("('{start}':'{end}'[dp])") %>%
    as.character()
}

#' Function that build the pubmed search strategy
#' @param ... a list of different field tags to add to the search
#' @export
combineSearchStrategy <- function(...) {
  dots <- rlang::list2(...)
  txt <- purrr::map_chr(dots, ~.x) %>%
    paste(collapse = " AND ")
  return(txt)
}

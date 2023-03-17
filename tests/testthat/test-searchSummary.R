test_that("abstract print works", {

  journals <- journalSearch(highImpactJournals())
  timeFrame <- dateRange("2018/01/01", "2022/12/31")
  terms <- textSearch(text = observationalStudy())
  meshTerm <- meshSearch("anemia")
  searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, meshTerm)
  hits <- searchPubmed(searchStrategy)
  res <- fetchPubmed(hits, searchStrategy)

  tst <- printAbstract(res, plot = FALSE)
  expect_type(tst[1], "character")
  expect_match(tst[1], "29020197")

  tst <- printAbstract(res, plot = TRUE)
  expect_s3_class(tst, c("kableExtra", "knitr_kable"))

})


test_that("Can create cumulative date matrix", {

  journals <- journalSearch(highImpactJournals())
  timeFrame <- dateRange("2018/01/01", "2022/12/31")
  terms <- textSearch(text = observationalStudy())
  meshTerm <- meshSearch("anemia")
  searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, meshTerm)
  hits <- searchPubmed(searchStrategy)
  res <- fetchPubmed(hits, searchStrategy)
  tst <- cumuDate(res)

  rr <- as.character(unique(res$epubdate)) %>% sort()
  dd <- unique(unlist(strsplit(unlist(res$key_words),";|; ")))

  expect_equal(rownames(tst), rr)
  expect_equal(colnames(tst), dd)

})


test_that("Run bib functions", {
  journals <- journalSearch(highImpactJournals())
  timeFrame <- dateRange("2018/01/01", "2022/12/31")
  terms <- textSearch(text = observationalStudy())
  meshTerm <- meshSearch("anemia")
  searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, meshTerm)
  hits <- searchPubmed(searchStrategy)
  res <- fetchPubmed(hits, searchStrategy) %>%
    dplyr::slice(1:4)

  #create bibfile
  tmp <- tempfile()
  fs::dir_create(tmp)
  ff <- fs::path(tmp, "tst.bib")
  exportBib(res, outfile = ff)

  tst <- readr::read_file(ff)
  expect_match(tst, "Nelson_2017")
  expect_match(tst, "10.1093/aje/kwx285")

})

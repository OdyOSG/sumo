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

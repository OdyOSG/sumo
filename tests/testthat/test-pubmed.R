test_that("Can search pubmed", {

  journals <- journalSearch(highImpactJournals())

  timeFrame <- dateRange("2018/01/01", "2022/12/31")

  terms <- textSearch(text = observationalStudy())

  meshTerm <- meshSearch("anemia")

  searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, meshTerm)

  hits <- searchPubmed(searchStrategy)

  expect_equal(hits$count, 101L)
  expect_equal(hits$ids[1], "36525434")
})


test_that("Fetch pubmed", {

  journals <- journalSearch(highImpactJournals())

  timeFrame <- dateRange("2018/01/01", "2022/12/31")

  terms <- textSearch(text = observationalStudy())

  meshTerm <- meshSearch("anemia")

  searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, meshTerm)

  hits <- searchPubmed(searchStrategy)

  res <- fetchPubmed(hits, searchStrategy)

  expect_equal(nrow(res), 101)
  expect_equal(ncol(res), 10)
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_equal(res$pmid[1], "29020197")

})

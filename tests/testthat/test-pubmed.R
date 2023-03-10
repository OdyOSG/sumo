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

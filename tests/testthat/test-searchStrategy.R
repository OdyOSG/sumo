test_that("can build search strategy on journals", {

  #trivial test for helper function
  listOfJournals <- highImpactJournals()
  expect_vector(listOfJournals, ptype = character(), size = 43)

  journals <- journalSearch(highImpactJournals())
  expect_type(journals, "character")

  journals2 <- journalSearch(c("Science", "Nature"))
  expect_true(grepl("Science", journals2))
  expect_true(grepl("Nature", journals2))
})


test_that("can build search strategy on date range", {

  timeFrame <- dateRange("2020/01/01", "2021/12/31")
  expect_type(timeFrame, "character")
  expect_true(grepl("dp", timeFrame))

})



test_that("can build search strategy on terms", {

  os <- observationalStudy()
  expect_vector(os, ptype = character(), size = 6)
  terms <- textSearch(text = observationalStudy())
  expect_type(terms, "character")
  expect_true(grepl("cohort", terms))
})

test_that("Combine search strategy", {

  journals <- journalSearch(highImpactJournals())
  timeFrame <- dateRange("2020/01/01", "2021/12/31")
  terms <- textSearch(text = observationalStudy())

  searchStrategy <- combineSearchStrategy(terms, timeFrame, journals)
  expect_type(searchStrategy, "character")
  expect_true(grepl("Science", searchStrategy))
  expect_true(grepl("dp", searchStrategy))
  expect_true(grepl("cohort", searchStrategy))

})

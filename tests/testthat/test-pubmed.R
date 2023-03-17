test_that("Can search pubmed", {

  hits <- searchPubmed(searchStrategy)

  expect_equal(hits$count, 101L)
  expect_equal(hits$ids[1], "36525434")
})


test_that("Fetch pubmed", {

  res <- fetchPubmed(hits, searchStrategy)

  expect_equal(nrow(res), 101)
  expect_equal(ncol(res), 10)
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_equal(res$pmid[1], "29020197")

})

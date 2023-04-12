test_that("search omop works", {
  conceptIds <- c("439777")
  keyword_tst <- searchOmop(conceptIds = conceptIds, cdm = cdm, explosion = TRUE)
  expect_type(keyword_tst, "character")
  expect_match(keyword_tst, "MeSH")
  expect_match(keyword_tst, "Anemia")
})



test_that("make dict works", {
  resDict <- makeDict(res, cdm)
  expect_s3_class(resDict, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(resDict), 5)
  expect_type(resDict$concept_id_1, "integer")
})


test_that("add dictionary back to res", {
  resDict <- makeDict(res, cdm)
  res <- addDictToRes(res, resDict)
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(res), 13)
  expect_type(res$concepts, "character")
})

test_that("icd10 map works", {
  resDict <- makeDict(res, cdm)
  icd10_map <- icd10Map(resDict, cdm)
  expect_type(icd10_map$category_id, "integer")
  expect_type(icd10_map$category_name, "character")
})

test_that("atc2 map works", {
  resDict <- makeDict(res, cdm)
  atc2_map <- atc2Map(resDict, cdm)
  expect_type(atc2_map$category_id, "integer")
  expect_type(atc2_map$category_name, "character")
})

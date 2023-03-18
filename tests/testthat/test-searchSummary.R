
test_that("abstract print works", {

  tst <- printAbstract(res, view = FALSE)
  expect_type(tst[1], "character")
  expect_match(tst[1], "29020197")

  tst <- printAbstract(res, view = TRUE)
  expect_s3_class(tst, c("kableExtra", "knitr_kable"))

})


test_that("Can create cumulative date matrix", {

  tst <- cumuDate(res)

  rr <- as.character(unique(res$epubdate)) %>% sort()
  dd <- unique(unlist(strsplit(unlist(res$key_words),";|; ")))

  expect_equal(rownames(tst), rr)
  expect_equal(colnames(tst), dd)

})


test_that("Run bib functions", {

  res2 <- res %>%
    dplyr::slice(1:2)

  #create bibfile
  tmp <- tempfile()
  fs::dir_create(tmp)
  ff <- fs::path(tmp, "tst.bib")
  exportBib(res2, outfile = ff)

  tst <- readr::read_file(ff)
  expect_match(tst, "Nelson_2017")
  expect_match(tst, "10.1093/aje/kwx285")
  expect_true(file.exists(ff))
  unlink(ff)

})

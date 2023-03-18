

test_that("Results bar", {
  resDict <- makeDict(res, cdm)
  res2 <- addDictToRes(res, resDict)
  gg <- plotResultsBar(res2, domain = "Journal", N = 10, conceptDict = resDict)
  expect_true(ggplot2::is.ggplot(gg))
  expect_s3_class(gg$layers[[1]]$geom, c("GeomBar", "GeomRect", "Geom", "ggproto", "gg"))
  expect_named(gg$data, c("concepts", "count"))
  expect_equal(gg$labels$x, "Count")
})


test_that("Results Line", {
  resDict <- makeDict(res, cdm)
  res2 <- addDictToRes(res, resDict)
  gg <- plotCumulative(res, domain = "Condition", N = 10, conceptDict = resDict)
  expect_true(ggplot2::is.ggplot(gg))
  expect_s3_class(gg$layers[[1]]$geom, c("GeomLine", "GeomPath", "Geom", "ggproto", "gg"))
  expect_s3_class(gg$layers[[2]]$geom, c("GeomText", "Geom", "ggproto", "gg"))
  expect_s3_class(gg$layers[[3]]$geom, c("GeomPoint", "Geom", "ggproto", "gg"))
  expect_equal(gg$labels$colour, "concepts")
})


test_that("Results Map", {
  resDict <- makeDict(res, cdm)
  res2 <- addDictToRes(res, resDict)
  gg <- plotMap(resDict)
  expect_true(ggplot2::is.ggplot(gg))
  expect_s3_class(gg$layers[[1]]$geom, c("GeomMap", "GeomPolygon", "Geom", "ggproto", "gg"))
  expect_equal(gg$labels$fill, "Results")
  expect_equal(gg$labels$x, "long")
  expect_equal(gg$labels$y, "lat")
})

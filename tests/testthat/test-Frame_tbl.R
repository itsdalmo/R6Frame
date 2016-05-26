context("dplyr::tbl Frame")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)

# R6 tbl_df --------------------------------------------------------------------
test_that("[<- works with R6 tbl_df", {
  skip_if_not_installed("dplyr")

  r6tbl <- R6Frame$new(dplyr::tbl_df(org))
  r6tbl[, "test"] <- "test"

  expect_s3_class(r6tbl, "R6Frame")
  expect_s3_class(r6tbl$data, c("tbl_df", "tbl"))
  expect_identical(r6tbl$data$test, rep("test", 2L))

})

test_that("[[<- works with R6 tbl_df", {
  skip_if_not_installed("dplyr")

  r6tbl <- R6Frame$new(dplyr::tbl_df(org))
  r6tbl[["test"]] <- "test"

  expect_s3_class(r6tbl, "R6Frame")
  expect_s3_class(r6tbl$data, c("tbl_df", "tbl"))
  expect_identical(r6tbl$data$test, rep("test", 2L))

})

test_that("[ works with R6 tbl_df", {

  r6tbl <- R6Frame$new(dplyr::tbl_df(org))
  r6tbl <- r6tbl[, "Q1", drop = FALSE]

  expect_s3_class(r6tbl, "R6Frame")
  expect_s3_class(r6tbl$data, c("tbl_df", "tbl"))
  expect_identical(names(r6tbl), "Q1")

})

test_that("[[ works with R6 tbl_df", {

  r6tbl <- R6Frame$new(dplyr::tbl_df(org))
  expect_identical(r6tbl[["Score"]], c(9, 8))

})

test_that("names<- works with R6 tbl_df", {
  skip_if_not_installed("dplyr")

  r6tbl <- R6Frame$new(dplyr::tbl_df(org))
  names(r6tbl) <- c("entity", "score")

  expect_s3_class(r6tbl, "R6Frame")
  expect_s3_class(r6tbl$data, c("tbl_df", "tbl"))
  expect_identical(names(r6tbl), c("entity", "score"))

})

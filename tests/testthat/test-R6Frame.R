context("R6Frame descriptives")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)

# Descriptives -----------------------------------------------------------------
test_that("head/tail works for R6 data.frame", {
  df <- R6Frame$new(org)
  expect_s3_class(head(df, 1), "R6Frame")
  expect_equal(head(df, 1), df[1, ])

  expect_s3_class(tail(df, 1), "R6Frame")
  expect_equal(tail(df, 1), df[2, ])
})

test_that("head/tail works for R6 data.table", {
  dt <- R6Frame$new(data.table::as.data.table(org))
  expect_s3_class(head(dt, 1), "R6Frame")
  expect_equal(head(dt, 1), dt[1, ])

  expect_s3_class(tail(dt, 1), "R6Frame")
  expect_equal(tail(dt, 1), dt[2, ])
})

test_that("head/tail works for R6 tbl_df", {
  skip_if_not_installed("dplyr")
  tbl <- R6Frame$new(dplyr::as.tbl(org))
  expect_s3_class(head(tbl, 1), "R6Frame")
  expect_equal(head(tbl, 1), tbl[1, ])

  expect_s3_class(tail(tbl, 1), "R6Frame")
  expect_equal(tail(tbl, 1), tbl[2, ])
})

test_that("length/dim work for R6 data.frame.", {

  df <- R6Frame$new(org)
  expect_identical(dim(df), c(2L, 2L))
  expect_identical(length(df), 2L)

})

test_that("length/dim work for R6 data.table.", {

  dt <- R6Frame$new(data.table::as.data.table(org))
  expect_identical(dim(dt), c(2L, 2L))
  expect_identical(length(dt), 2L)

})

test_that("length/dim work for R6 tbl_df.", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  expect_identical(dim(tbl), c(2L, 2L))
  expect_identical(length(tbl), 2L)

})

test_that("names/dimnames work for R6 data.frame.", {

  df <- R6Frame$new(org)
  expect_identical(dimnames(df), list(c("1", "2"), c("Q1", "Score")))
  expect_identical(names(df), c("Q1", "Score"))

})

test_that("names/dimnames work for R6 data.table.", {

  dt <- R6Frame$new(data.table::as.data.table(org))
  expect_identical(dimnames(dt), list(NULL, c("Q1", "Score")))
  expect_identical(names(dt), c("Q1", "Score"))

})

test_that("names/dimnames work for R6 tbl_df.", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  expect_identical(dimnames(tbl), list(c("1", "2"), c("Q1", "Score")))
  expect_identical(names(tbl), c("Q1", "Score"))

})




context("melt/gather and dcast/spread")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score1 = c(9, 8), Score2 = c(80, 70), stringsAsFactors = FALSE)

# Gather -----------------------------------------------------------------------
test_that("gather works with R6 data.frame", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  df <- R6Frame$new(org)
  df <- tidyr::gather(df, var, score, -Q1)

  expect_s3_class(df, "R6Frame")
  expect_identical(names(df), c("Q1", "var", "score"))
  expect_identical(df$data$score, c(9, 8, 80, 70))

})

test_that("gather works with R6 data.table", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  # tidyr does not have data.table methods. Returns a data.frame.
  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- tidyr::gather(dt, var, score, -Q1)

  expect_s3_class(dt, "R6Frame")
  expect_identical(names(dt), c("Q1", "var", "score"))
  expect_identical(dt$data$score, c(9, 8, 80, 70))

})

test_that("gather works with R6 tbl_df", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- tidyr::gather(tbl, var, score, -Q1)

  expect_s3_class(tbl, "R6Frame")
  expect_identical(names(tbl), c("Q1", "var", "score"))
  expect_identical(tbl$data$score, c(9, 8, 80, 70))

})

# Spread -----------------------------------------------------------------------
test_that("spread works with R6 data.frame", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  df <- R6Frame$new(org)
  df <- tidyr::gather(df, var, score, -Q1)
  df <- tidyr::spread(df, var, score)

  expect_s3_class(df, "R6Frame")
  expect_identical(names(df), c("Q1", "Score1", "Score2"))
  expect_equal(df$data, org)

})

test_that("spread works with R6 data.table", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  # tidyr does not have data.table methods. Returns a data.frame.
  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- tidyr::gather(dt, var, score, -Q1)
  dt <- tidyr::spread(dt, var, score)

  expect_s3_class(dt, "R6Frame")
  expect_identical(names(dt), c("Q1", "Score1", "Score2"))
  expect_equal(dt$data, org)

})

test_that("spread works with R6 tbl_df", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- tidyr::gather(tbl, var, score, -Q1)
  tbl <- tidyr::spread(tbl, var, score)

  expect_s3_class(tbl, "R6Frame")
  expect_identical(names(tbl), c("Q1", "Score1", "Score2"))
  expect_equal(tbl$data, org)

})

# melt -------------------------------------------------------------------------
test_that("melt works with R6 data.table", {
  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- melt(dt, "Q1", c("Score1", "Score2"))

  expect_s3_class(dt, "R6Frame")
  expect_identical(names(dt), c("Q1", "variable", "value"))
  expect_identical(dt$data$value, c(9, 8, 80, 70))

})

# dcast ------------------------------------------------------------------------
test_that("dcast works with R6 data.table", {
  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- melt(dt, "Q1", c("Score1", "Score2"))
  dt <- dcast(dt, "Q1 ~ variable", value.var = "value")

  expect_s3_class(dt, "R6Frame")
  expect_identical(names(dt), c("Q1", "Score1", "Score2"))
  expect_equivalent(dt$data, data.table::as.data.table(org))

})

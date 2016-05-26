context("dplyr verbs")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)

# Mutate -----------------------------------------------------------------------
test_that("mutate works with R6 data.frame", {
  skip_if_not_installed("dplyr")

  df <- R6Frame$new(org)
  df <- dplyr::mutate(df, test = "test")

  expect_s3_class(df, "R6Frame")
  expect_identical(df$data$test, rep("test", 2))

})

test_that("mutate works with R6 data.table", {
  skip_if_not_installed("dplyr")

  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- dplyr::mutate(dt, test = "test")

  expect_s3_class(dt, "R6Frame")
  expect_identical(dt$data$test, rep("test", 2))

})

test_that("mutate works with R6 tbl_df", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- dplyr::mutate(tbl, test = "test")

  expect_s3_class(tbl, "R6Frame")
  expect_identical(tbl$data$test, rep("test", 2))

})

# Select -----------------------------------------------------------------------
test_that("select works with R6 data.frame", {
  skip_if_not_installed("dplyr")

  df <- R6Frame$new(org)
  df <- dplyr::select(df, Q1)

  expect_s3_class(df, "R6Frame")
  expect_true(ncol(df) == 1L)
  expect_identical(names(df), "Q1")

})

test_that("select works with R6 data.table", {
  skip_if_not_installed("dplyr")

  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- dplyr::select(dt, Q1)

  expect_s3_class(dt, "R6Frame")
  expect_true(ncol(dt) == 1L)
  expect_identical(names(dt), "Q1")

})

test_that("select works with R6 tbl_df", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- dplyr::select(tbl, Q1)

  expect_s3_class(tbl, "R6Frame")
  expect_true(ncol(tbl) == 1L)
  expect_identical(names(tbl), "Q1")

})

# Other verbs ------------------------------------------------------------------
# Note: Only checking R6 tbl_df for remaining joins. Assuming _df and _dt work.
test_that("filter works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(org)
  tbl <- dplyr::filter(tbl, Score > 8)

  expect_s3_class(tbl, "R6Frame")
  expect_true(nrow(tbl) == 1L)

})

test_that("summarise works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(org)
  tbl <- dplyr::summarise(tbl, Score = mean(Score))

  expect_s3_class(tbl, "R6Frame")
  expect_equal(tbl$data$Score, 8.5)

})

test_that("arrange works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(org)
  tbl <- dplyr::arrange(tbl, Score)

  expect_s3_class(tbl, "R6Frame")
  expect_identical(tbl$data$Q1, c("Example 2", "Example 1"))

})

test_that("tbl_vars works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(org)
  expect_identical(dplyr::tbl_vars(tbl), names(tbl))

})

test_that("group_by works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(org)
  tbl <- dplyr::group_by(tbl, Q1)

  expect_s3_class(tbl, "R6Frame")
  expect_identical(as.character(dplyr::groups(tbl)), "Q1")

})

test_that("ungroup works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(org)
  tbl <- dplyr::group_by(tbl, Q1)
  tbl <- dplyr::ungroup(tbl)

  expect_s3_class(tbl, "R6Frame")
  expect_identical(dplyr::groups(tbl), NULL)

})

test_that("rename works with R6 tbl_df (and _df)", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(org)
  tbl <- dplyr::rename(tbl, entity = Q1)

  expect_s3_class(tbl, "R6Frame")
  expect_identical(names(tbl)[1], "entity")

})

test_that("rename works with R6 data.table", {
  skip_if_not_installed("dplyr")

  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- dplyr::rename(dt, entity = Q1)

  expect_s3_class(dt, "R6Frame")
  expect_identical(names(dt)[1], "entity")

})

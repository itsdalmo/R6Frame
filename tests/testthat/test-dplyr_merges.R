context("binds/merges/joins")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)

# Bind rows --------------------------------------------------------------------
test_that("bind_rows work with R6 data.frame", {
  skip_if_not_installed("dplyr")

  df <- R6Frame$new(org)
  df <- bind_rows(df, dplyr::mutate(df, test = "test"))

  expect_s3_class(df, "R6Frame")
  expect_identical(names(df), c("Q1", "Score", "test"))
  expect_identical(df$data$test, c(NA, NA, "test", "test"))

})

test_that("bind_rows work with R6 data.table", {
  skip_if_not_installed("dplyr")

  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- bind_rows(dt, dplyr::mutate(dt, test = "test"))

  expect_s3_class(dt, "R6Frame")
  expect_identical(names(dt), c("Q1", "Score", "test"))
  expect_identical(dt$data$test, c(NA, NA, "test", "test"))

})

test_that("bind_rows work with R6 tbl_df", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- bind_rows(tbl, dplyr::mutate(tbl, test = "test"))

  expect_s3_class(tbl, "R6Frame")
  expect_identical(names(tbl), c("Q1", "Score", "test"))
  expect_identical(tbl$data$test, c(NA, NA, "test", "test"))

})

# Bind cols --------------------------------------------------------------------
test_that("bind_cols work with R6 data.frame", {
  skip_if_not_installed("dplyr")

  df <- R6Frame$new(org)
  df <- bind_cols(df, dplyr::mutate(df, test = "test"))

  expect_s3_class(df, "R6Frame")

})

test_that("bind_cols work with R6 data.table", {
  skip_if_not_installed("dplyr")

  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- bind_cols(dt, dplyr::mutate(dt, test = "test"))

  expect_s3_class(dt, "R6Frame")

})

test_that("bind_cols work with R6 tbl_df", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- bind_cols(tbl, dplyr::mutate(tbl, test = "test"))

  expect_s3_class(tbl, "R6Frame")

})

# Left join --------------------------------------------------------------------
test_that("left_join work with R6 data.frame", {
  skip_if_not_installed("dplyr")

  df <- R6Frame$new(org)
  df <- dplyr::left_join(df, dplyr::mutate(df[1, ], test = "test"))

  expect_s3_class(df, "R6Frame")
  expect_identical(df$data$test, c("test", NA))

})

test_that("left_join work with R6 data.table", {
  skip_if_not_installed("dplyr")

  dt <- R6Frame$new(data.table::as.data.table(org))
  dt <- dplyr::left_join(dt, dplyr::mutate(dt[1, ], test = "test"))


  expect_s3_class(dt, "R6Frame")
  expect_identical(dt$data$test, c("test", NA))

})

test_that("left_join work with R6 tbl_df", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- dplyr::left_join(tbl, dplyr::mutate(tbl[1, ], test = "test"))

  expect_s3_class(tbl, "R6Frame")
  expect_identical(tbl$data$test, c("test", NA))

})

# Other joins ------------------------------------------------------------------
# Note: Only checking R6 tbl_df for remaining joins. Assuming _df and _dt work.
test_that("right_join work with R6Frame", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- dplyr::right_join(tbl, dplyr::mutate(tbl[1, ], test = "test"))

  expect_s3_class(tbl, "R6Frame")
  expect_identical(tbl$data$test, "test")
  expect_identical(tbl$data$Score, 9)

})

test_that("full_join work with R6Frame", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- dplyr::full_join(tbl, dplyr::mutate(tbl[1, ], Q1 = "Example 3", test = "test"))

  expect_s3_class(tbl, "R6Frame")
  expect_identical(tbl$data$test, c(NA, NA, "test"))
  expect_identical(tbl$data$Q1, c("Example 1", "Example 2", "Example 3"))

})

test_that("semi_join work with R6Frame", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- dplyr::semi_join(tbl, dplyr::mutate(tbl[1, ], test = "test"))

  expect_s3_class(tbl, "R6Frame")
  expect_identical(names(tbl), c("Q1", "Score"))
  expect_identical(nrow(tbl), 1L)

})

test_that("anti_join work with R6Frame", {
  skip_if_not_installed("dplyr")

  tbl <- R6Frame$new(dplyr::as.tbl(org))
  tbl <- dplyr::anti_join(tbl, dplyr::mutate(tbl[1, ], test = "test"))

  expect_s3_class(tbl, "R6Frame")
  expect_identical(names(tbl), c("Q1", "Score"))
  expect_identical(tbl$data$Q1, "Example 2")

})

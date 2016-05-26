context("data.table Frame")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)
org <- data.table::as.data.table(org)

test_that(":= works with R6 data.table", {

  r6dt <- R6Frame$new(org)
  r6dt[, test := "test"]

  expect_s3_class(r6dt, "R6Frame")
  expect_s3_class(r6dt$data, "data.table")
  expect_identical(r6dt$data$test, rep("test", 2L))

})

test_that("[<- works with R6 data.table", {

  r6dt <- R6Frame$new(org)
  r6dt[, "test"] <- "test"

  expect_s3_class(r6dt, "R6Frame")
  expect_s3_class(r6dt$data, "data.table")
  expect_identical(r6dt$data$test, rep("test", 2L))

})

test_that("[[<- works with R6 data.table", {

  r6dt <- R6Frame$new(org)
  r6dt[["test"]] <- "test"

  expect_s3_class(r6dt, "R6Frame")
  expect_s3_class(r6dt$data, "data.table")
  expect_identical(r6dt$data$test, rep("test", 2L))

})

test_that("[ works with R6 data.table", {

  r6dt <- R6Frame$new(org)
  r6dt <- r6dt[, .(Q1)]

  expect_s3_class(r6dt, "R6Frame")
  expect_s3_class(r6dt$data, "data.table")
  expect_identical(names(r6dt), "Q1")

})

test_that("[[ works with R6 data.table", {

  r6dt <- R6Frame$new(org)

  expect_identical(r6dt[["Score"]], c(9, 8))
  expect_identical(r6dt[, Score], c(9, 8))

})

test_that("names<- works with R6 data.table", {

  r6dt <- R6Frame$new(org)
  names(r6dt) <- c("entity", "score")

  expect_s3_class(r6dt, "R6Frame")
  expect_s3_class(r6dt$data, "data.table")
  expect_identical(names(r6dt), c("entity", "score"))

})

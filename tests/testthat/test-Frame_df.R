context("data.frame Frame")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)

test_that("[<- works with R6 data.frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df[, "test"] <- "test"
  df[, "test"] <- "test"

  expect_s3_class(r6df, "R6Frame")
  expect_identical(df, r6df$data)

})

test_that("[[<- works with R6 data.frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df[["test"]] <- "test"
  df[["test"]] <- "test"

  expect_s3_class(r6df, "R6Frame")
  expect_identical(df, r6df$data)

})

test_that("[ works with R6 data.frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df <- r6df[, "Q1", drop = FALSE]
  df <- df[, "Q1", drop = FALSE]

  expect_s3_class(r6df, "R6Frame")
  expect_identical(df, r6df$data)

})

test_that("[[ works with R6 data.frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df <- r6df[["Score"]]
  df <- df[["Score"]]

  expect_is(r6df, "numeric")
  expect_identical(df, r6df)

})


test_that("names<- works with R6 data.frame", {

  r6df <- R6Frame$new(org); df <- org

  names(r6df) <- c("entity", "score")
  names(df) <- c("entity", "score")

  expect_s3_class(r6df, "R6Frame")
  expect_identical(df, r6df$data)

})

test_that("rbind works with R6 data.frame", {

  r6df <- R6Frame$new(org); df <- org

  r6 <- rbind(r6df, r6df)
  rf <- rbind(df, df)
  mix <- rbind(r6df, df)

  expect_s3_class(r6, "R6Frame")
  expect_identical(rf, r6$data)
  expect_s3_class(mix, "R6Frame")
  expect_identical(rf, mix$data)

})

test_that("cbind works with R6 data.frame", {

  r6df <- R6Frame$new(org); df <- org

  r6 <- cbind(r6df, r6df)
  rf <- cbind(df, df)
  mix <- cbind(r6df, df)

  expect_identical(rf, r6$data)
  expect_identical(rf, mix$data)

})

test_that("Merge works with R6 data.frame", {

  # TODO

})

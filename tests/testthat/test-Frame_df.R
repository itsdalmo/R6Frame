context("Frame_df")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)

test_that("[<- works with R6Frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df[, "test"] <- "test"
  df[, "test"] <- "test"

  expect_identical(df, r6df$data)

})

df <- R6Frame$new(org)

test_that("[[<- works with R6Frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df[["test"]] <- "test"
  df[["test"]] <- "test"

  expect_identical(df, r6df$data)

})

test_that("[ works with R6Frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df <- r6df[, "Q1", drop = FALSE]
  df <- df[, "Q1", drop = FALSE]

  expect_identical(df, r6df$data)

})

test_that("[[ works with R6Frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df <- r6df[["Score"]]
  df <- df[["Score"]]

  expect_is(r6df, "numeric")
  expect_identical(df, r6df)

})


test_that("names<- works with R6Frame", {

  r6df <- R6Frame$new(org); df <- org

  names(r6df) <- c("entity", "score")
  names(df) <- c("entity", "score")

  expect_identical(df, r6df$data)

})

test_that("rbind works with R6Frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df_r <- rbind(r6df, r6df)
  df <- rbind(df, df)
  # mix <- rbind(r6df, df)

  expect_identical(df, r6df_r$data)
  # expect_identical(df, mix$data)

})

test_that("cbind works with R6Frame", {

  r6df <- R6Frame$new(org); df <- org

  r6df <- cbind(r6df, r6df)
  df <- cbind(df, df)
  # mix <- cbind(r6df, df)

  expect_identical(df, r6df$data)
  # expect_identical(df, mix$data)

})

test_that("Merge works with R6Frame", {

  # TODO
  #   y <- R6Frame(x)
  #   y <- set_association(y, mainentity = "Q1")
  #   z <- merge(y, x, by = "Q1")
  #
  #   check(z)

})

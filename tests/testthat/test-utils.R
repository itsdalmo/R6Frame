context("Utility functions")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)

test_that("capture_dots can be used inside functions", {
  df <- R6Frame$new(org)
  variable <- "Score"
  f <- function(x) {
    env <- list2env(list(srv = x, parent = environment()))
    variable <- "Q1"
    x[[variable]]
  }
  expect_identical(f(df$data), f(df))
})

test_that("capture_dots uses regular scoping", {
  df <- R6Frame$new(org)
  f <- function(x) {
    variable <- "Q1"
    x[[variable]]
  }
  f2 <- function(x) {
    f(x)
  }
  expect_identical(f2(df$data), f2(df))
})

#' R6Frame: rbind
#'
#' \code{\link[base]{rbind}} method for \code{R6Frame} objects. Labels and associations
#' are also merged if you are binding other R6Frames.
#'
#' @param x A \code{R6Frame}.
#' @param ... Additional parameters passed to \code{rbind}.
#' @inheritParams base::rbind
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

rbind.R6Frame <- function(x, ...) {
  x$do_merge("rbind", list(...))
}

#' R6Frame: cbind
#'
#' \code{\link[base]{cbind}} method for \code{R6Frame} objects. Labels and associations
#' are also merged if you are binding other R6Frames.
#'
#' @param x A \code{R6Frame}.
#' @param ... Additional parameters passed to \code{cbind}.
#' @inheritParams base::rbind
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

cbind.R6Frame <- function(x, ...) {
  x$do_merge("cbind", list(...))
}

# S3 methods -------------------------------------------------------------------
#' @export
`[.R6Frame` <- function(x, ...) {
  x$do("[", capture_dots(...))
}

#' @export
`[[.R6Frame` <- function(x, ...) {
  x$do("[[", capture_dots(...))
}

#' @export
`[<-.R6Frame` <- function(x, ...) {
  x$do("[<-", capture_dots(...))
}

#' @export
`[[<-.R6Frame` <- function(x, ...) {
  x$do("[[<-", capture_dots(...))
}

#' @export
names.R6Frame <- function(x) {
  x$names()
}

#' @export
`names<-.R6Frame` <- function(x, value) {
  x$set_names(value)
}

#' @importFrom utils head
#' @export
head.R6Frame <- function(x, ...) {
  f <- get("head", asNamespace("utils"))
  x$do(f, list(...))
}

#' @importFrom utils tail
#' @export
tail.R6Frame <- function(x, ...) {
  f <- get("tail", asNamespace("utils"))
  x$do(f, list(...))
}

#' @export
dimnames.R6Frame <- function(x) {
  # NOTE: Might need to use data.table::copy() here.
  dimnames(x$data)
}

#' @export
dim.R6Frame <- function(x) {
  dim(x$data)
}

#' @export
length.R6Frame <- function(x) {
  length(x$data)
}

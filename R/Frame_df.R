#' R6Frame: rbind
#'
#' A new generic for \code{\link[base]{rbind}} which includes a default method, and
#' a method for \code{R6Frame}. For all other objects, it defaults to the base method.
#'
#' @param x A \code{R6Frame}.
#' @param ... Additional parameters passed to \code{rbind}.
#' @inheritParams base::rbind
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

rbind <- function(x, ...) UseMethod("rbind")

#' @export
rbind.default <- function(x, ...) rbind(x, ...)

#' @export
rbind.R6Frame <- function(x, ...) {
  x$do_merge("rbind", list(...))
}

#' R6Frame: cbind
#'
#' A new generic for \code{\link[base]{cbind}} which includes a default method, and
#' a method for \code{R6Frame}. For all other objects, it defaults to the base method.
#'
#' @param x A \code{R6Frame}.
#' @param ... Additional parameters passed to \code{cbind}.
#' @inheritParams base::rbind
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

cbind <- function(x, ...) UseMethod("cbind")

#' @export
cbind.default <- function(x, ...) rbind(x, ...)

#' @export
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
  x$do(utils::head, list(...))
}

#' @importFrom utils tail
#' @export
tail.R6Frame <- function(x, ...) {
  x$do(utils::tail, list(...))
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

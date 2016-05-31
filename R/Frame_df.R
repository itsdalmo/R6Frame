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
#' @seealso \code{\link[base]{rbind}} in \code{base} for more information and examples.

rbind <- function(x, ...) UseMethod("rbind")

#' @export
rbind.default <- function(x, ...) rbind(x, ...)

#' @export
rbind.R6Frame <- function(x, ...) {
  x$do_merge("rbind", list(...), env = parent.frame())
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
#' @seealso \code{\link[base]{cbind}} in \code{base} for more information and examples.

cbind <- function(x, ...) UseMethod("cbind")

#' @export
cbind.default <- function(x, ...) rbind(x, ...)

#' @export
cbind.R6Frame <- function(x, ...) {
  x$do_merge("cbind", list(...), env = parent.frame())
}

# S3 methods -------------------------------------------------------------------
#' @export
`[.R6Frame` <- function(x, ...) {
  x$do("[", capture_dots(...), env = parent.frame())
}

#' @export
`[[.R6Frame` <- function(x, ...) {
  x$do("[[", capture_dots(...), env = parent.frame())
}

#' @export
`[<-.R6Frame` <- function(x, i, j, value) {
  args <- list(
    if (missing(i)) quote(expr = ) else i,
    if (missing(j)) quote(expr = ) else j,
    value = value
    )
  x$do("[<-", args, env = parent.frame())
}

#' @export
`[[<-.R6Frame` <- function(x, i, j, value) {
  if (nargs() < 4L) {
    args <- list(i, value = value)
  } else {
    args <- list(i, j, value = value)
  }
  x$do("[[<-", args, env = parent.frame())
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
  x$do(utils::head, list(...), env = parent.frame())
}

#' @importFrom utils tail
#' @export
tail.R6Frame <- function(x, ...) {
  x$do(utils::tail, list(...), env = parent.frame())
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

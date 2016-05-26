#' tidyr: Methods for R6Frame.
#'
#' R6Frames include support for \code{tidyr}. See \code{tidyr} documentation
#' for further information.
#'
#' NOTE: In order for these verbs to function properly, tidyr must be attached
#' with \code{library} or \code{require} before \code{reporttoolDT}. (I think).
#'
#' @param x A \code{R6Frame} object.
#' @param ... Further arguments passed to \code{tidyr}.
#' @author Kristian D. Olsen
#' @name tidyr_verbs

#' @rdname tidyr_verbs
#' @export
gather_.R6Frame <- function(x, ...) {
  x$do(tidyr::gather_, list(...), env = parent.frame())
}

#' @rdname tidyr_verbs
#' @export
spread_.R6Frame <- function(x, ...) {
  x$do(tidyr::spread_, list(...), env = parent.frame())
}

#' @rdname tidyr_verbs
#' @export
complete_.R6Frame <- function(x, ...) {
  x$do(tidyr::complete_, list(...), env = parent.frame())
}

#' @rdname tidyr_verbs
#' @export
expand_.R6Frame <- function(x, ...) {
  x$do(tidyr::expand_, list(...), env = parent.frame())
}

#' @rdname tidyr_verbs
#' @export
fill_.R6Frame <- function(x, ...) {
  x$do(tidyr::fill_, list(...), env = parent.frame())
}


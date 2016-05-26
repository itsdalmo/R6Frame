#' data.table: melt
#'
#' Same as the \code{data.table} function \code{melt}. This is a new generic,
#' because the \code{data.table} version calls \code{reshape2} unless the input
#' is a \code{data.table}.
#'
#' @param data A \code{R6Frame} or \code{data.frame}.
#' @param ... Additional parameters passed to \code{melt}.
#' @inheritParams data.table::melt
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

melt <- function(data, ...) UseMethod("melt")

#' @rdname melt
#' @export
melt.default <- data.table::melt

#' @rdname melt
#' @export
melt.R6Frame <- function(data, ...) {
  data$do(data.table::melt, capture_dots(...))
}

#' data.table: dcast
#'
#' Same as the \code{data.table} function \code{dcast}. This is a new generic,
#' because the \code{data.table} version calls \code{reshape2} unless the input
#' is a \code{data.table}.
#'
#' @param data A \code{R6Frame} or \code{data.frame}.
#' @param ... Additional parameters passed to \code{dcast}.
#' @inheritParams data.table::dcast
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

dcast <- function(data, ...) UseMethod("dcast")

#' @rdname dcast
#' @export
dcast.default <- data.table::dcast

#' @rdname dcast
#' @export
dcast.R6Frame <- function(data, ...) {
  data$do(data.table::dcast, capture_dots(...))
}

#' @export
.datatable.aware <- TRUE

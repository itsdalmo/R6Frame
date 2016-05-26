#' dplyr: Methods for R6Frame.
#'
#' R6Frames include support for \code{dplyr}. See \code{dplyr} documentation
#' for further information.
#'
#' NOTE: In order for these verbs to function properly, dplyr must be attached
#' with \code{library} or \code{require} before \code{reporttoolDT}. (I think).
#'
#' @param x A R6Frame object.
#' @param ... Further arguments passed to \code{dplyr}.
#' @author Kristian D. Olsen
#' @name dplyr_verbs

#' @rdname dplyr_verbs
#' @export
tbl_vars.R6Frame <- function(x) x$names()

#' @rdname dplyr_verbs
#' @export
select_.R6Frame <- function(x, ...) {
  # dplyr::select also allows renaming variables when called.
  # e.g., select(x, new_name = old_var)
  dots <- lazyeval::all_dots(...)
  vars <- renamed_vars(names(x), dots)
  x$do(dplyr::select_, dots, env = parent.frame(), renamed = vars)
}

#' @rdname dplyr_verbs
#' @export
rename_.R6Frame <- function(x, ...) {
  dots <- lazyeval::all_dots(...)
  vars <- renamed_vars(names(x), dots)
  x$do(dplyr::rename_, dots, env = parent.frame(), renamed = vars)
}

#' @rdname dplyr_verbs
#' @export
filter_.R6Frame <- function(x, ...) {
  x$do(dplyr::filter_, lazyeval::all_dots(...), env = parent.frame())
}

#' @rdname dplyr_verbs
#' @export
arrange_.R6Frame <- function(x, ...) {
  x$do(dplyr::arrange_, lazyeval::all_dots(...), env = parent.frame())
}

#' @rdname dplyr_verbs
#' @export
group_by_.R6Frame <- function(x, ...) {
  # TODO: "add" is not passed to next call. Error message.
  x$do(dplyr::group_by_, list(...), env = parent.frame())
}

#' @rdname dplyr_verbs
#' @export
groups.R6Frame <- function(x, ...) {
  x$do(dplyr::groups, lazyeval::all_dots(...), env = parent.frame())
}

#' @rdname dplyr_verbs
#' @export
ungroup.R6Frame <- function(x, ...) {
  x$do(dplyr::ungroup, lazyeval::all_dots(...), env = parent.frame())
}

#' @rdname dplyr_verbs
#' @export
mutate_.R6Frame <- function(x, ...) {
  x$do(dplyr::mutate_, lazyeval::all_dots(...), env = parent.frame())
}

#' @rdname dplyr_verbs
#' @export
summarise_.R6Frame <- function(x, ...) {
  x$do(dplyr::summarise_, lazyeval::all_dots(...), env = parent.frame())
}

#' dplyr: Methods for R6Frame.
#'
#' \code{dplyr} bind methods for R6Frame.
#'
#' NOTE: In order for these verbs to function properly, dplyr must be attached
#' with \code{library} or \code{require} before \code{reporttoolDT}. (I think).
#'
#' @param x A R6Frame object.
#' @param ... Further arguments passed to \code{dplyr}.
#' @author Kristian D. Olsen
#' @name dplyr_binds

#' @rdname dplyr_binds
#' @export
bind_rows <- function(x, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package required to use bind_rows.")
  }
  UseMethod("bind_rows")
}

#' @export
bind_rows.default <- function(x, ...) {
  dplyr::bind_rows(x, ...)
}

#' @rdname dplyr_binds
#' @export
bind_rows.R6Frame <- function(x, ...) {
  x$do_merge(dplyr::bind_rows, list(...), env = parent.frame())
}


#' @rdname dplyr_binds
#' @export
bind_cols <- function(x, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package required to use bind_rows.", call. = FALSE)
  }
  UseMethod("bind_cols")
}

#' @export
bind_cols.default <- function(x, ...) {
  dplyr::bind_cols(x, ...)
}

#' @rdname dplyr_binds
#' @export
bind_cols.R6Frame <- function(x, ...) {
  x$do_merge(dplyr::bind_cols, list(...), env = parent.frame())
}

# Joins ------------------------------------------------------------------------

#' dplyr: Methods for R6Frame.
#'
#' \code{dplyr} join methods for R6Frame.
#'
#' NOTE: In order for these verbs to function properly, dplyr must be attached
#' with \code{library} or \code{require} before \code{reporttoolDT}. (I think).
#'
#' @param x A \code{R6Frame} object.
#' @param y A \code{R6Frame}, regular \code{data.frame}, \code{data.table} or \code{tbl}.
#' @param ... Further arguments passed to \code{dplyr}.
#' @author Kristian D. Olsen
#' @name dplyr_joins

#' @rdname dplyr_joins
#' @export
left_join.R6Frame <- function(x, y, ...) {
  x$do_merge(dplyr::left_join, list(y, ...), env = parent.frame())
}

#' @rdname dplyr_joins
#' @export
right_join.R6Frame <- function(x, y, ...) {
  x$do_merge(dplyr::right_join, list(y, ...), env = parent.frame())
}

#' @rdname dplyr_joins
#' @export
full_join.R6Frame <- function(x, y, ...) {
  x$do_merge(dplyr::full_join, list(y, ...), env = parent.frame())
}

#' @rdname dplyr_joins
#' @export
semi_join.R6Frame <- function(x, y, ...) {
  x$do_merge(dplyr::semi_join, list(y, ...), env = parent.frame())
}

#' @rdname dplyr_joins
#' @export
anti_join.R6Frame <- function(x, y, ...) {
  x$do_merge(dplyr::anti_join, list(y, ...), env = parent.frame())
}

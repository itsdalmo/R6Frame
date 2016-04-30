#' Create a new R6Frame
#'
#' Create a new R6Frame from a \code{data.frame}, \code{data.table} or \code{tbl}.
#'
#' @param x A \code{data.frame}, \code{data.table} or \code{tbl} (requires dplyr).
#' @author Kristian D. Olsen
#' @note Under the hood, the \code{R6Frame} is a \code{\link[R6]{R6Class}}.
#' @export
#' @examples
#' NULL

R6Frame <- function(x) R6Frame$new()

#' @importFrom R6 R6Class
R6Frame <- R6::R6Class("R6Frame",
  private = list(
    deep_clone = function(name, value) {
      if (name == "data" && data.table::is.data.table(value)) {
        data.table::copy(value)
      } else {
        value
      }
    }
  ),
  public = list(
    data = NULL,
    initialize = function(x) {
      if (missing(x) || !is.data.frame(x))
        stop("Expecting a data.frame or data.table.", call. = FALSE)
      self$data <- x
      self$update()
    },

    do = function(f, dots, renamed = NULL) {
      "Perform operations on the R6Frame."
      # Original call is 2 layers up at this point. parent.frame(n = 2L)
      res <- do.call(f, c(list(self$data), dots), envir = parent.frame(n = 2L))

      if (identical(data.table::address(res), data.table::address(self$data))) {
        invisible(self$update(renamed))
      } else {
        if (is.data.frame(res)) {
          self$initialize_subset(res)$update(renamed)
        } else {
          res
        }
      }
    },

    do_merge = function(f, dots) {
      "Do merging operations on a R6Frame."
      # Get labels and associations
      lbl <- lapply(dots, function(x) { if (is.R6Frame(x)) x$get_label() })
      aso <- lapply(dots, function(x) { if (is.R6Frame(x)) x$get_association(invert = FALSE) })

      # Unlist and assign to private fields (self$do will remove duplicates)
      private$.labels <- merge_vectors(private$.labels, lbl)
      private$.associations <- merge_vectors(private$.associations, aso)

      # Extract data and apply function
      dots <- lapply(dots, function(x) { if (is.R6Frame(x)) x$get_data() else x })
      self$do(f, dots)
    },

    initialize_subset = function(x) {
      "(Re)Initialize R6Frame after slicing or subsetting."
      slice <- self$clone(deep = TRUE)
      slice$data <- x
      slice
    },

    update = function(renamed = NULL) {
      "Update function. Called after each operation on the R6Frame."
      self
    },

    set_names = function(new_names) {
      "Set colnames/named vectors with new names."
      if (data.table::is.data.table(self$data)) {
        data.table::setnames(self$data, new_names)
      } else {
        names(self$data) <- new_names
      }
      invisible(self)
    },

    names = function() {
      names(self$data)
    },

    print = function(...) {
      print(self$data)
    }
  )

)

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

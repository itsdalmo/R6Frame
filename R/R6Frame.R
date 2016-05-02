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

#' @rdname R6Frame
#' @export
is.R6Frame <- function(x) inherits(x, "R6Frame")

#' @importFrom R6 R6Class
#' @export
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
    get_data = function(copy = TRUE) {
      if (data.table::is.data.table(self$data)) {
        if (copy)
          data.table::copy(self$data)
      } else {
        self$data
      }

    },
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

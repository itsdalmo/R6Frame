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

    initialize = function(x) {
      if (missing(x) || !is.data.frame(x))
        stop("Expecting a data.frame or data.table.", call. = FALSE)
      if (data.table::is.data.table(x)) {
        self$data <- data.table::copy(x)
      } else {
        self$data <- x
      }
      self$update()
    },

    get_data = function(copy = TRUE) {
      if (data.table::is.data.table(self$data)) {
        if (copy)
          data.table::copy(self$data)
      } else {
        self$data
      }

    },

    do = function(f, dots, env, renamed = NULL) {
      "Perform operations on the R6Frame."
      # Original call is 2 layers up at this point. parent.frame(n = 2L)
      res <- do.call(f, c(list(self$data), dots), envir = env)

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

    do_merge = function(f, dots, env) {
      "Do merging operations on a R6Frame."
      # Extract data and apply function
      dots <- lapply(dots, function(x) { if (is.R6Frame(x)) x$get_data() else x })
      self$do(f, dots, env = env)
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

    as_df = function(...) {
      self$data <- as.data.frame(self$data)
      invisible(self)
    },

    as_dt = function(...) {
      if (!data.table::is.data.table(self$data))
        self$data <- data.table::as.data.table(self$data)
      invisible(self)
    },

    as_tbl = function(...) {
      if (!is_tbl(self$data) && requireNamespace("dplyr", quietly = TRUE))
        self$data <- dplyr::as.tbl(self$data)
      invisible(self)
    },

    as_tbldf = function(...) {
      if (!inherits(self$data, "tbl_df") && requireNamespace("dplyr", quietly = TRUE))
        self$data <- dplyr::tbl_df(self$data)
      invisible(self)
    },

    as_tbldt = function(...) {
      if (!inherits(self$data, "tbl_dt") && requireNamespace("dplyr", quietly = TRUE))
        self$data <- dplyr::tbl_dt(self$data)
      invisible(self)
    },

    names = function() {
      if (data.table::is.data.table(self$data)) {
        data.table::copy(names(self$data))
      } else {
        names(self$data)
      }
    },

    print = function(...) {
      print(self$data)
    }
  )

)

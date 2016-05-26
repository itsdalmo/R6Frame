# Capture dots. Primarily used to pass calls to R6 methods ---------------------
capture_dots <- function(...) {
  eval(substitute(alist(...)))
}

# Get renamed columns from lazy_dots in dplyr::select and dplyr::rename.
# Return is equivalent to: setNames(old_name, new_name) ------------------------
renamed_vars <- function(vars, dots) {
  renamed <- dplyr::select_vars_(vars, dots)
  renamed <- renamed[names(renamed) != renamed]

  # Return NULL early if nothing has been renamed.
  if (!length(renamed)) return()

  # If renamed. Return a renamed vars vector.
  # (has to be done in a loop to support referencing newly created vars.)
  for (i in seq_along(renamed)) {
    vars[vars == renamed[i]] <- names(renamed)[i]
  }
  vars

}

# Check whether data is a tbl --------------------------------------------------
is_tbl <- function(x) {
  inherits(x, c("tbl", "tbl_df", "tbl_dt"))
}

# Hadley's %||% ----------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

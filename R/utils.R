# Capture dots. Primarily used to pass calls to R6 methods ---------------------
capture_dots <- function(...) {
  eval(substitute(alist(...)))
}

# Get renamed columns from lazy_dots in dplyr::select and dplyr::rename.
# Return is equivalent to: setNames(old_name, new_name) ------------------------
renamed_vars <- function(dots) {
  expr <- dots[!is.na(names(dots)) & names(dots) != ""]
  if (length(expr)) {
    nms <- lapply(names(expr), function(nm) { x <- expr[[nm]]$expr; if (x != nm) x })
    nms <- setNames(as.character(unlist(nms)), names(expr))
  } else {
    nms <- NULL
  }
  nms
}

# Check whether data is a tbl --------------------------------------------------
is_tbl <- function(x) {
  inherits(x, c("tbl", "tbl_df", "tbl_dt"))
}

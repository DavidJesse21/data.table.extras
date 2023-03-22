#' Conditional column manipulation by reference
#'
#' @description
#' These functions manipulate existing columns of your `data.table` by reference
#' based on a selection of columns, a predicate function or a regex pattern that the
#' column name must match.
#'
#' @details
#' `setj_at()` takes an integer (!) vector of column indices or a character vector of
#' column names as input for `cols` and applies `.f` on the corresponding columns.
#' If you supply indices or column names which don't exist in your `data.table`
#' you will get an informative message.
#'
#' `setj_if()` takes a predicate function as input for `.p`.
#' The function `.f` will then be applied to those columns for which
#' `.p` returns `TRUE`.
#' If the predicate function returns `FALSE` for all columns you will get an
#' informative message.
#'
#' `setj_grep()` manipulates columns based on regular expression pattern matches.
#' For that you need to pass a single regex pattern to the `pattern` argument.
#' If the pattern doesn't match any of your `data.table` columns you will get
#' an informative message.
#'
#' @param DT (`data.table::data.table()`)\cr
#'   A `data.table`.
#' @param .f (`function()`)\cr
#'   A function that will be applied by reference to the matching columns
#' @param ... Additional function arguments you want to supply to `.f`.
#' @param cols (`numeric()` or `character()`)\cr
#'   A vector of integer column indices or character column names.
#' @param .p (`function()`)\cr
#'   A predicate function that will be applied to all columns and returns
#'   a logical vector.
#' @param pattern (`character(1)`)\cr
#'   A regular expression pattern as used by `grep()`.
#'
#' @importFrom data.table set
#' @importFrom checkmate assert_data_table assert_function
#'
#' @name setj
NULL



#' @rdname setj
#'
#' @importFrom checkmate test_names test_integerish
#'
#' @export
setj_at = function(DT, cols, .f, ...) {
  assert_data_table(DT)

  if (!(
    test_names(cols, type =  "unique", subset.of = colnames(DT)) ||
    test_integerish(cols, lower = 1L, upper = ncol(DT), unique = TRUE)
  )) {
    stop(paste0(
      "\n",
      "`cols` must be either a character vector of column names ",
      "or an integerish vector of valid column indices."
    ))
  }

  # TODO: further safety checks on .f (?)
  assert_function(.f)

  # TODO: create checks for `...`

  if (is.numeric(cols)) {
    cols = as.integer(cols)
  }

  for (j in cols) {
    set(DT, j = j, value = .f(DT[[j]], ...))
  }

  invisible()
}




#' @rdname setj
#'
#' @export
setj_if = function(DT, .p, .f, ...) {
  assert_data_table(DT)
  assert_function(.p)
  assert_function(.f)

  # TODO: safety checks on .p (?)
  ptrue = vapply(DT, .p, logical(1L))
  cols = names(which(ptrue))

  if (is_empty(cols)) {
    message(
      "`.p` returns FALSE for all columns. ",
      "Your data.table remains unchanged."
    )
  } else {
    for (j in cols) {
      set(DT, j = j, value = .f(DT[[j]], ...))
    }
  }

  invisible()
}



#' @rdname setj
#'
#' @importFrom checkmate assert_string
#'
#' @export
setj_grep = function(DT, pattern, .f, ...) {
  assert_data_table(DT)
  assert_string(pattern)
  assert_function(.f)

  cols = grep(pattern, colnames(DT), value = TRUE)

  if (is_empty(cols)) {
    message(
      "The supplied pattern does not match any column names. ",
      "Your data.table remains unchanged."
    )
  } else {
    for (j in cols) {
      set(DT, j = j, value = .f(DT[[j]], ...))
    }
  }

  invisible()
}



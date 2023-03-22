#' Drop columns conditionally by reference
#'
#' @description
#' These functions drop columns of your `data.table` by reference
#' based on a selection of columns, a predicate function or a regex pattern that the
#' column name must match.
#'
#' @details
#' `dropj_at()` drops column based on a vector of column indices or column names.
#'
#' `dropj_if()` takes predicate function `.p` as an input that will
#' be applied to each of the columns and returns a logical vector.
#' It will then drop those columns for which the function returns `TRUE`.
#'
#' `dropj_grep()` takes a regex pattern as input and will check if the
#' column names match that pattern. If so, they will be dropped.
#'
#' @param DT (`data.table::data.table()`)\cr
#'   A `data.table`.
#' @param cols (`character()` or `numeric()`)\cr
#'   A vector of integer column indices or character column names.
#' @param .p (`function()`)\cr
#'   A predicate function that will be applied to all columns and returns
#'   a logical vector.
#' @param pattern (`character(1)`)\cr
#'   A regular expression pattern as used by `grep()`.
#'
#' @importFrom checkmate assert_data_table test_names test_integerish
#'    assert_function assert_string
#' @importFrom data.table set
#'
#'
#'
#' @name dropj
NULL



#' @rdname dropj
#' @export
dropj_at = function(DT, cols) {
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

  if (is.numeric(cols)) {
    # For the `set()` function, columns should be actual integers and not only integerish.
    # Drop columns with the larger index first.
    cols = sort(
      as.integer(cols),
      decreasing = TRUE
    )
  }

  for (j in cols) {
    set(DT, j = j, value = NULL)
  }

  invisible()
}



#' @rdname dropj
#' @export
dropj_if = function(DT, .p) {
  assert_data_table(DT)
  assert_function(.p)

  # TODO: safety checks on .p (?)
  ptrue = vapply(DT, .p, logical(1L))
  cols = names(which(ptrue))

  if (is_empty(cols)) {
    message(
      "`.p` returns FALSE for all columns. ",
      "No columns have been dropped."
    )
  } else {
    for (j in cols) {
      set(DT, j = j, value = NULL)
    }
  }

  invisible()
}



#' @rdname dropj
#' @export
dropj_grep = function(DT, pattern) {
  assert_data_table(DT)
  assert_string(pattern)

  cols = grep(pattern, colnames(DT), value = TRUE)

  if (is_empty(cols)) {
    message(
      "The supplied pattern does not match any column names. ",
      "No columns have been dropped."
    )
  } else {
    for (j in cols) {
      set(DT, j = j, value = NULL)
    }
  }

  invisible()
}


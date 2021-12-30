#' Conditional column manipulation by reference
#'
#' @description
#' These functions manipulate existing columns of your \code{data.table} by reference
#' based on a selection of columns, a predicate function or a regex pattern that the
#' column name must match.
#'
#' @details
#' \code{setj_at()} takes an integer (!) vector of column indices or a character vector of
#' column names as input for \code{cols} and applies \code{.f} on the corresponding columns.
#' If you supply indices or column names which don't exist in your \code{data.table}
#' you will get an informative message.
#'
#' \code{setj_if()} takes a predicate function as input for \code{.p}.
#' The function \code{.f} will then be applied to those columns for which
#' \code{.p} returns TRUE.
#' If the predicate function returns \code{FALSE} for all columns you will get an
#' informative message.
#'
#' \code{setj_grep()} manipulates columns based on regular expression pattern matches.
#' For that you need to pass a single regex pattern to the \code{pattern} argument.
#' If the pattern doesn't match any of your \code{data.table} columns you will get
#' an informative message.
#'
#' @param DT A \code{data.table}
#' @param .f A function that will be applied by reference to the matching columns
#' @param ... Additional function arguments you want to supply to \code{.f}
#' @param cols A vector of integer column indices or character column names
#' @param .p A predicate function that will be applied to all columns and returns
#'    a logical vector
#' @param pattern A regular expression pattern as used by \code{grep}
#'
#' @name setj
NULL



#' @rdname setj
#'
#' @importFrom checkmate assert_data_table test_character test_integer assert_function
#' @importFrom data.table set
#'
#' @export
setj_at = function(DT, cols, .f, ...) {
  assert_data_table(DT)

  if (!(test_character(cols) | test_integer(cols, lower = 1L))) {
    stop(paste0(
      "\n",
      "`cols` must be either a character vector of column names ",
      "or an integer vector of valid (positive) column indices."
    ))
  }

  # TODO: further safety checks on .f (?)
  assert_function(.f)

  # TODO: create checks for `...`

  if (is.integer(cols)) {
    type = "int"
    col_idx = 1:ncol(DT)
    invalid = cols[!(cols %in% col_idx)]
    cols = setdiff(cols, invalid)
  }

  if (is.character(cols)) {
    type = "chr"
    col_names = colnames(DT)
    invalid = cols[!(cols %in% col_names)]
    cols = setdiff(cols, invalid)
  }

  for (j in cols) {
    set(DT, j = j, value = .f(DT[[j]], ...))
  }

  if (!is_empty(invalid)) {
    switch(
      type,
      int = message(
        "Columns with indices ",
        "[", paste0(invalid, collapse = ", "), "]",
        " don't exist and have been ignored."
      ),
      chr = message(
        "Columns ",
        "[", paste0(sprintf("`%s`", invalid), collapse = ", "), "]",
        " don't exist and have been ignored."
      )
    )
  }

  invisible()
}




#' @rdname setj
#'
#' @importFrom checkmate assert_data_table assert_function
#' @importFrom data.table set
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
#' @importFrom checkmate assert_data_table assert_function assert_string
#' @importFrom data.table set
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



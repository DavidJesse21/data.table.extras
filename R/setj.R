#' Conditional column manipulation by reference
#'
#' @description
#' These functions manipulate existing columns of your \code{data.table} by reference
#' based on a selection of columns, a predicate function or a regex pattern that the
#' column name must match.
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

  for (j in cols) {
    set(DT, j = j, value = .f(DT[[j]], ...))
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
      "The supplied pattern does not match any column name in your data.table."
    )
  } else {
    for (j in cols) {
      set(DT, j = j, value = .f(DT[[j]], ...))
    }
  }

  invisible()
}



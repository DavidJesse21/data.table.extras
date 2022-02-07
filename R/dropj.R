#' Drop columns conditionally by reference
#'
#' @description
#' These functions drop columns of your \code{data.table} by reference
#' based on a selection of columns, a predicate function or a regex pattern that the
#' column name must match.
#'
#' @details
#' \code{dropj_at()} drops column based on a vector of column indices or column names.
#'
#' \code{dropj_if()} takes predicate function \code{.p} as an input that will
#' be applied to each of the columns and returns a logical vector.
#' It will then drop those columns for which the function returns \code{TRUE}.
#'
#' \code{dropj_grep()} takes a regex pattern as input and will check if the
#' column names match that pattern. If so, they will be dropped.
#'
#' @param DT A \code{data.table}.
#' @param cols A vector of integer column indices or character column names.
#' @param .p A predicate function that will be applied to all columns and returns
#'    a logical vector.
#' @param pattern A regular expression pattern as used by \code{grep}.
#'
#' @importFrom checkmate assert_data_table test_character test_integer
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

  if (!(test_character(cols) | test_integer(cols, lower = 1L))) {
    stop(paste0(
      "\n",
      "`cols` must be either a character vector of column names ",
      "or an integer vector of valid (positive) column indices."
    ))
  }

  if (is.integer(cols)) {
    type = "int"
    col_idx = 1:ncol(DT)
    invalid = cols[!(cols %in% col_idx)]
    cols = setdiff(cols, invalid)
    cols = sort(cols, decreasing = TRUE)
  }

  if (is.character(cols)) {
    type = "chr"
    col_names = colnames(DT)
    invalid = cols[!(cols %in% col_names)]
    cols = setdiff(cols, invalid)
  }

  for (j in cols) {
    set(DT, j = j, value = NULL)
  }

  if (!is_empty(invalid)) {
    switch(
      type,
      int = message(
        "Columns with indices ",
        "[", paste0(invalid, collapse = ", "), "]",
        " didn't exist and have been ignored."
      ),
      chr = message(
        "Columns ",
        "[", paste0(sprintf("`%s`", invalid), collapse = ", "), "]",
        " didn't exist and have been ignored."
      )
    )
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
      "No columns will be dropped."
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
      "Your data.table remains unchanged."
    )
  } else {
    for (j in cols) {
      set(DT, j = j, value = NULL)
    }
  }

  invisible()
}


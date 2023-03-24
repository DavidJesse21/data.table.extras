#' Reshape a contingency table like matrix to a long data.table
#'
#' @description
#' `melt_matrix()` takes a matrix as input, which has the shape of a contingency table.
#' Hence, the row and column names/indices are considered as unique values of a
#' (categorical) variable and the cells represent values associated with the
#' respective row-column combinations.
#' The function then transforms the data to a long data.table representation.
#'
#' @param x (`matrix()`)\cr
#'   A matrix.
#' @param name_row (`character(1)`)\cr
#'   A string specifying the column name associated with the rows of the matrix.
#' @param name_col (`character(1)`)\cr
#'   A string specifying the column name associated with the columns of the matrix.
#' @param name_val (`character(1)`)\cr
#'   A string specifying the column name associated with the cells/values of the matrix.
#'
#' @return (`data.table::data.table()`)\cr
#'   A `data.table` representing `x` in a long format.
#'
#' @note
#' If the supplied matrix does not have row and/or column names, it will be supplied
#' with the matching indices.\cr
#' The columns of the data.table created from the row and column names are always
#' of type "character".
#' If a type conversion is desired, this has to be done by the
#' user afterwards manually.
#'
#' @importFrom checkmate assert_matrix assert_string
#' @importFrom data.table setDT setnames
#'
#' @export
melt_matrix = function(x,
                       name_row = "row",
                       name_col = "column",
                       name_val = "value") {

  assert_matrix(x)
  assert_string(name_row)
  assert_string(name_col)
  assert_string(name_val)

  if (any(duplicated(c(name_row, name_col, name_val)))) {
    stop(
      "\n",
      "`name_row`, `name_col` and/or `name_val` must not be duplicated."
    )
  }

  # Assign row and column names if not provided
  if (is.null(colnames(x))) {
    colnames(x) = 1:ncol(x)
  }
  if (is.null(rownames(x))) {
    rownames(x) = 1:nrow(x)
  }

  # Melt the matrix to a data.table
  class(x) = "table"
  dt = as.data.frame(x, stringsAsFactors = FALSE, responseName = name_val)
  setDT(dt)

  # Assign correct names
  setnames(dt, old = paste0("Var", 1:2), new = c(name_row, name_col))


  return(dt)
}

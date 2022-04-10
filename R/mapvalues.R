#' Replace values of a vector based on a mapping scheme.
#'
#' @description This is basically a copy or reimplementation of the
#'    \code{plyr::mapvalues()} function.
#'    It takes an input vector \code{x} as well as two vectors \code{from} and
#'    \code{to} that act as a dictionary for the mapping.
#'
#' @param x A vector.
#' @param from A vector specifying the values that should be mapped.
#' @param to A vector specifying the values to map to.
#'
#' @return A vector. It may be of a different type than the input vector \code{x},
#'    depending on your inputs for \code{from} and \code{to}.
#'
#' @importFrom checkmate assert_atomic_vector
#'
#' @export
mapvalues = function(x, from, to) {
  assert_atomic_vector(x, min.len = 1L)
  assert_atomic_vector(from, min.len = 1L, unique = TRUE)
  assert_atomic_vector(to, min.len = 1L)

  if (!same_length(from, to)) {
    stop("Vectors `from` and `to` must have the same length.")
  }

  if (!setequal(unique(x), from)) {
    stop("`from` must cover all unique values in `x`.")
  }


  if (is.factor(x)) {
    levels(x) = mapvalues(levels(x), from, to)
    return(x)
  }

  mapidx = match(x, from)
  to[mapidx]
}

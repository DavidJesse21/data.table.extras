dt_check = data.table::as.data.table(iris)

# Test sanity checks
expect_error(dropj_at(dt_check, c(-1L, -2L, 5L)))
expect_error(dropj_at(dt_check, c(1, 2, 3)))
expect_error(
  dropj_at(list(a = 1:3, b = 1:3, c = 1:3), c("a", "b"))
)

# Test normal functionality
dt1 = data.table::copy(dt_check)
dropj_at(dt1, c(3L, 5L))
expect_false(any(c("Petal.Length", "Species") %in% colnames(dt1)))
expect_true(all(
  c("Sepal.Length", "Sepal.Width", "Petal.Width") %in% colnames(dt1)
))

dt2 = data.table::copy(dt_check)
dropj_at(dt2, c("Sepal.Length", "Sepal.Width"))
expect_false(any(c("Sepal.Length", "Sepal.Width") %in% colnames(dt2)))
expect_true(all(
  c("Petal.Length", "Petal.Width", "Species") %in% colnames(dt2)
))

dt3 = data.table::copy(dt_check)
expect_message(dropj_at(dt3, c("sepal_length", "petal_length")))

rm(dt1, dt2, dt3, dt_check)

dt_check = data.table::as.data.table(iris)

# Test sanity checks ----

# `DT` needs to be a data.table
df_test = iris
expect_error(dropj_at(df_test, c(1, 3)))
expect_error(
  dropj_at(list(a = 1:3, b = 1:3, c = 1:3), c("a", "b"))
)

# `cols` argument must be valid, i.e.
# - valid indices or column names
# - unique entries
expect_error(dropj_at(dt_check, c(-1, -2, 5)))
expect_error(dropj_at(dt_check, c("Sepal.Length", "sepal_width")))
expect_error(dropj_at(dt_check, c(1, 3, 1)))
expect_error(dropj_at(dt_check, c("Sepal.Length", "Sepal.Width", "Sepal.Width")))


# Test normal functionality ----

dt1 = data.table::copy(dt_check)
dropj_at(dt1, c(3, 5))
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

rm(dt1, dt2, dt_check, df_test)

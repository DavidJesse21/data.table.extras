dt_check = data.table::as.data.table(iris)

# Test sanity checks ----

# `DT` needs to be a data.table
df_test = iris
expect_error(dropj_grep(df_test, "^Sepal"))
expect_error(
  dropj_grep(list(a = 1, b = 2,), "a")
)

# `pattern` must be a string (i.e. character vector of length one)
expect_error(dropj_grep(dt_check, c("^Sepal", "^Petal")))
expect_error(dropj_grep(dt_check, 3L))


# Test normal functionality ----

dt1 = data.table::copy(dt_check)
dropj_grep(dt1, "^Sepal")
expect_false(any(
  c("Sepal.Length", "Sepal.Width") %in% colnames(dt1)
))
expect_true(all(
  c("Petal.Length", "Petal.Width", "Species") %in% colnames(dt1)
))

# Case: no pattern matching
dt2 = data.table::copy(dt_check)
expect_message(dropj_grep(dt2, "^sepal_"))

rm(dt1, dt2, df_test)

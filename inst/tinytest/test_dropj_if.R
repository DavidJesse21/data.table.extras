dt_check = data.table::as.data.table(iris)
dt_check[, Petal.Width := as.character(Petal.Width)]

# Test sanity checks ----

# `DT` needs to be a data.table
df_test = iris
expect_error(dropj_if(df_test, is.numeric))
expect_error(
  dropj_if(list(a = 1:3, b = letters[1:3]), is.character)
)

# `.p` needs to be a function object
expect_error(dropj_if(dt_check, "is.character"))


# Test normal functionality ----

dt1 = data.table::copy(dt_check)
dropj_if(dt1, is.character)
expect_false("Petal.Width" %in% colnames(dt1))
expect_true(all(
  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Species") %in% colnames(dt1)
))

dt2 = data.table::copy(dt_check)
dropj_if(dt2, is.factor)
expect_false("Species" %in% colnames(dt2))
expect_true(all(
  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width") %in% colnames(dt2)
))

dt3 = data.table::copy(dt_check)
dropj_if(dt3, is.numeric)
expect_true("Species" %in% colnames(dt3))
expect_false(any(
  c("Sepal.Length", "Sepal.Width", "Petal.Length") %in% colnames(dt3)
))

dt4 = data.table::copy(dt_check)
dropj_if(dt4, is.factor)
# Now there should be no more column of type factor and therefore we should only
# receive a message
expect_message(dropj_if(dt4, is.factor))

rm(dt1, dt2, dt3, dt4, dt_check, df_test)

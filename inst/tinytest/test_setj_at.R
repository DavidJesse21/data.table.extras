dt_check = data.table::data.table(
  a = 1:5,
  b = 1:5,
  c = 1:5,
  d = 1:5
)


# Test sanity checks ----

# `DT` must be a data.table
df_test = iris
expect_error(
  setj_at(df_test, 1:4, function(x) x + 1)
)
expect_error(
  setj_at(data.frame(a = 1:3), 1:2, function(x) x + 1)
)

# `cols` argument must be valid, i.e.
# - valid indices or column names
# - unique entries
expect_error(
  setj_at(dt_check, c(-1, 1, 3), as.character)
)
expect_error(
  setj_at(dt_check, c(1, 1, 3), as.character)
)
expect_error(
  setj_at(dt_check, c(1, 3, 5), as.character)
)
expect_error(
  setj_at(dt_check, c("a", "d", "f"), as.character)
)
expect_error(
  setj_at(dt_check, c("a", "a", "b"), as.character)
)

# `.f` must be a function object
expect_error(
  setj_at(dt_check, c("a", "b", "c"), "as.character")
)


# Test functionality ----

dt1 = data.table::copy(dt_check)
setj_at(dt1, 1:2, function(x) x + 1)
expect_equal(dt1[[1]], 2:6)
expect_equal(dt1[[2]], 2:6)
expect_equal(dt1[[3]], 1:5)
expect_equal(dt1[[4]], 1:5)
rm(dt1)

dt2 = data.table::copy(dt_check)
setj_at(dt2, c("a", "c"), function(x) x + 1)
expect_equal(dt2[["a"]], 2:6)
expect_equal(dt2[["c"]], 2:6)
expect_equal(dt2[["b"]], 1:5)
expect_equal(dt2[["b"]], 1:5)
rm(dt2)


rm(dt_check)

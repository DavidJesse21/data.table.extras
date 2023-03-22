dt_check = data.table::data.table(
  aa = 1:5,
  ab = 1:5,
  bd = 1:5,
  cd = 1:5
)


# Test sanity checks ----

# `DT` must be a data.table
df_test = iris
expect_error(
  setj_grep(df_test, "^Sepal", function(x) x / 2)
)
expect_error(
  setj_grep(data.frame(a = 1), "^a", as.character)
)

# `pattern` must be a string
expect_error(
  setj_grep(dt_check, c("^a", "a$"), as.character)
)
expect_error(
  setj_grep(dt_check, 1L, as.character)
)

# `.f` must be a function object
expect_error(
  setj_grep(dt_check, "^a", "as.character")
)


# Test functionality ----
dt1 = data.table::copy(dt_check)

setj_grep(dt1, "^a", function(x) x + 10L)
expect_equal(dt1[, aa], 11:15)
expect_equal(dt1[, ab], 11:15)
expect_equal(dt1[, bd], 1:5)
expect_equal(dt1[, cd], 1:5)

setj_grep(dt1, "d$", as.character)
expect_true(dt1[, is.character(bd)])
expect_true(dt1[, is.character(cd)])
expect_false(dt1[, is.character(aa)])
expect_false(dt1[, is.character(ab)])

# Scenario: Pattern doesn't match any column name
dt2 = data.table::copy(dt1)
expect_message(
  setj_grep(dt2, "^z", as.factor)
)
expect_equal(dt2, dt1)

# Invisible return and chaining works
dt3 = data.table::copy(dt_check)
expect_true(
  inherits(setj_grep(dt1, "^a", function(x) x + 10L), "data.table")
)

dt4 = data.table::copy(dt_check)
dt4 = setj_grep(dt1, "^a", function(x) x + 10L)[1:3]
expect_equal(nrow(dt4), 3)


rm(dt_check, df_test, dt1, dt2, dt3, dt4)


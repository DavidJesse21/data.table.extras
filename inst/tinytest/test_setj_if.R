dt_check = data.table::data.table(
  a = 1:5,
  b = 6:10,
  c = letters[1:5]
)

# Test sanity checks ----

# `DT` must be a data.table
expect_error(
  setj_if(data.frame(a = 1:3), is.integer, function(x) x + 1)
)

# `.p` must be a function object
expect_error(
  setj_if(dt_check, "is.integer", function(x) x + 1)
)

# `.f` must be a function object
expect_error(
  setj_if(dt_check, is.integer, "as.character")
)


# Test functionality ----
dt1 = data.table::copy(dt_check)

setj_if(dt1, is.integer, function(x) x + 10L)
expect_equal(dt1[, a], 11:15)
expect_equal(dt1[, b], 16:20)
expect_equal(dt1[, c], letters[1:5])

setj_if(dt1, is.integer, function(x) paste0("Number: ", x))
expect_true(dt1[, is.character(a)])
expect_true(dt1[, is.character(b)])
expect_true(dt1[, all(startsWith(a, "Number: "))])
expect_true(dt1[, all(startsWith(b, "Number: "))])
expect_true(dt1[, is.character(c)])
expect_false(dt1[, any(startsWith(c, "Number: "))])

dt2 = data.table::copy(dt_check)
setj_if(dt2, is.character, as.factor)
expect_true(dt2[, is.factor(c)])
expect_false(dt2[, is.factor(a)])
expect_false(dt2[, is.factor(b)])

# Scenario: predicate function returns FALSE for all columns
dt3 = data.table::copy(dt_check)
expect_message(
  setj_if(dt3, is.factor, as.character)
)

# Invisible return and chaining works
dt4 = data.table::copy(dt_check)
expect_true(
  inherits(setj_if(dt4, is.integer, function(x) x + 10L), "data.table")
)

dt5 = data.table::copy(dt_check)
dt5 = setj_if(dt5, is.integer, function(x) x + 10L)[1:3]
expect_equal(nrow(dt5), 3)


rm(dt_check, dt1, dt2, dt3, dt4, dt5)

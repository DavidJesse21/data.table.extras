dt_check = data.table::data.table(
  a = 1:5,
  b = 6:10,
  c = letters[1:5]
)

# Test basic sanity checks
expect_error(
  setj_if(data.frame(a = 1:3), is.integer, function(j) j + 1)
)
expect_error(
  setj_if(dt_check, "is.integer", function(j) j + 1)
)

# Test basic functionality
dt1 = data.table::copy(dt_check)

setj_if(dt1, is.integer, function(j) j + 10L)
expect_equal(dt1[, a], 11:15)
expect_equal(dt1[, b], 16:20)
expect_equal(dt1[, c], letters[1:5])

setj_if(dt1, is.integer, function(j) paste0("Number: ", j))
expect_true(dt1[, is.character(a)])
expect_true(dt1[, is.character(b)])
expect_true(dt1[, all(startsWith(a, "Number: "))])
expect_true(dt1[, all(startsWith(b, "Number: "))])
expect_true(dt1[, is.character(c)])
expect_false(dt1[, any(startsWith(c, "Number: "))])
rm(dt1)

dt2 = data.table::copy(dt_check)
setj_if(dt2, is.character, as.factor)
expect_true(dt2[, is.factor(c)])
expect_false(dt2[, is.factor(a)])
expect_false(dt2[, is.factor(b)])
rm(dt2)

rm(dt_check)

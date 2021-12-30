library(data.table)

dt_check = data.table(
  a = 1:5,
  b = 1:5,
  c = 1:5,
  d = 1:5
)

# Test basic sanity checks
expect_error(
  setj_at(data.frame(a = 1:3), 1:2, function(j) j + 1)
)
expect_error(
  setj_at(dt_check, c(1, 2, 3), function(j) j + 1)
)
expect_error(
  setj_at(dt_check, 1:3, function(x) x + 1)
)

# Test basic functionality
dt1 = copy(dt_check)
setj_at(dt1, 1:2, function(j) j + 1)
expect_equal(dt1[[1]], 2:6)
expect_equal(dt1[[2]], 2:6)
expect_equal(dt1[[3]], 1:5)
expect_equal(dt1[[4]], 1:5)
rm(dt1)

dt2 = copy(dt_check)
setj_at(dt2, c("a", "c"), function(j) j + 1)
expect_equal(dt2[["a"]], 2:6)
expect_equal(dt2[["c"]], 2:6)
expect_equal(dt2[["b"]], 1:5)
expect_equal(dt2[["b"]], 1:5)
rm(dt2)

# Scenario: Some invalid column names or indices have been provided
expect_message(
  setj_at(copy(dt_check), c(1L, 5L), function(j) j + 1)
)
expect_message(
  setj_at(copy(dt_check), c("a", "e"), function(j) j + 1)
)

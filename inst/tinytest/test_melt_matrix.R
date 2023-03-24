x = matrix(1:9, nrow = 3, ncol = 3)


# Test sanity checks ----

# `x` must be a matrix
expect_error(melt_matrix(as.data.frame(x)))

# `name_row`, `name_col` and `name_val` must all be strings
expect_error(melt_matrix(x, name_row = c("row1", "row2")))
expect_error(melt_matrix(x, name_row = 3))

expect_error(melt_matrix(x, name_col = c("col1", "col2")))
expect_error(melt_matrix(x, name_col = 3))

expect_error(melt_matrix(x, name_val = c("val1", "val2")))
expect_error(melt_matrix(x, name_val = 3))

# `name_row`, `name_col` and `name_val` must be distinct
expect_error(melt_matrix(x, "x", "x", "y"))
expect_error(melt_matrix(x, "x", "y", "x"))
expect_error(melt_matrix(x, "y", "x", "x"))
expect_silent(melt_matrix(x, "x", "y", "z"))


# Test functionality ----

out = melt_matrix(x, "row", "column", "value")

# Result should be a 9 x 3 data.table with corresponding column names
expect_equal(dim(out), c(9, 3))
expect_equal(colnames(out), c("row", "column", "value"))
expect_true(inherits(out, "data.table"))

# `value` column should contain matrix entries column by column
expect_equal(out$value, 1:9)

# `row` and `column` columns in resulting data.table are of class `character`
expect_true(inherits(out$row, "character"))
expect_true(inherits(out$column, "character"))

# Original matrix didn't have named rows and columns -> will be named with sequential numbers
expect_true(setequal(out$row, as.character(1:3)))
expect_true(setequal(out$column, as.character(1:3)))

# Operation on named matrix gives desired output
rownames(x) = letters[1:3]
colnames(x) = letters[4:6]
out = melt_matrix(x)
expect_true(setequal(out$row, letters[1:3]))
expect_true(setequal(out$column, letters[4:6]))

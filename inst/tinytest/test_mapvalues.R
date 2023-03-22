# Map from numbers to character
numbers = c(rep(1L, 5), rep(2L, 3), rep(3L, 10), rep(2L, 2))
out = mapvalues(numbers, 1:3, c("very good", "good", "ok"))
expect_equal(
  out,
  c(rep("very good", 5), rep("good", 3), rep("ok", 10), rep("good", 2))
)

# inputs `from` and `to` cover more values than there are in `x`.
out = mapvalues(numbers, 1:4, c("very good", "good", "ok", "one more"))
expect_equal(
  out,
  c(rep("very good", 5), rep("good", 3), rep("ok", 10), rep("good", 2))
)


# Map from character to character
chrs = c(rep(letters[1], 5), rep(letters[2], 3), rep(letters[3], 2), letters[1])
out = mapvalues(chrs, letters[1:3], c("very good", "good", "ok"))
expect_equal(
  out,
  c(rep("very good", 5), rep("good", 3), rep("ok", 2), "very good")
)

# Recode factor levels
fctrs = factor(letters)
out = mapvalues(fctrs, letters, LETTERS)
expect_equal(levels(out), LETTERS)



# Error if `from` and `to` are not the same length
expect_error(mapvalues(numbers, 1:3, c("very good", "good")))
expect_error(mapvalues(numbers, 1:3, letters[1:4]))

# Error if `from` doesn't cover `x`'s unique values
expect_error(mapvalues(chrs, letters[1:2], c("very good", "good")))
expect_error(mapvalues(chrs, letters[c(1:2, 4)], c("very good", "good", "ok")))


# What happens if there are values in x that are not covered in `from`
# Case 1: Type of input and output are the same -> no big deal
# Case 2: Output type differs from input type -> throw error

# Tests for color functions

test_that("color returns expected hex codes", {
  # Test default behavior (returning all colors when no args provided)
  all_colors <- color(unname = FALSE)
  expect_type(all_colors, "character")
  expect_true(length(all_colors) > 0)
  expect_named(all_colors)

  # Test requesting specific colors
  expect_identical(color("white"), "#FFFFFF")
  expect_identical(color("black"), "#000000")

  # Test requesting multiple colors
  expect_identical(
    color("white", "black"),
    c("#FFFFFF", "#000000")
  )

  # Test requesting multiple colors with names preserved
  named_colors <- color("white", "black", unname = FALSE)
  expect_identical(
    named_colors,
    c(white = "#FFFFFF", black = "#000000")
  )

  # Test requesting non-existent color
  expect_error(
    color("nonexistent_color"),
    "Some colors not defined"
  )
})

test_that("color_pattern works as expected", {
  # Positive test cases
  cat_colors <- color_pattern("cat_", unname = FALSE)
  expect_true(length(cat_colors) > 0)
  expect_true(all(startsWith(names(cat_colors), "cat_")))

  seq_colors <- color_pattern("seq_")
  expect_true(length(seq_colors) > 0)

  # Test with a pattern that should match nothing
  expect_warning(
    color_pattern("xyz_nonexistent_"),
    "No colors match the pattern"
  )

  # Test argument validation
  expect_error(color_pattern(123))
  expect_error(color_pattern(c("cat_", "seq_")))
  expect_error(
    color_pattern("cat_", unname = "yes"),
    "Assertion on 'unname' failed"
  )
})

test_that("color function parameter validation", {
  # Empty call should return all colors
  all_colors <- color()
  expect_type(all_colors, "character")
  expect_true(length(all_colors) > 0)

  # unname parameter behavior
  named_all <- color(unname = FALSE)
  expect_named(named_all)
  unnamed_all <- color(unname = TRUE)
  expect_null(names(unnamed_all))

  # Test validation of ... arguments to ensure they're strings
  expect_error(color(123), "Assertion on 'Argument #1' failed")
  expect_error(color(TRUE), "Assertion on 'Argument #1' failed")
  expect_error(
    color(list("white")),
    "Assertion on 'Argument #1' failed"
  )

  # Test that vectors are not allowed
  expect_error(
    color(c("white", "black")),
    "Assertion on 'Argument #1' failed: Must have length 1."
  )

  # Multiple arguments should still work as long as each is a single string
  expect_length(color("white", "black"), 2)
})

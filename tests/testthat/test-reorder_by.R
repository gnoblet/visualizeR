test_that("reorder_by works with character columns", {
  test_data <- data.frame(
    category = c("B", "A", "D", "C", "E"),
    value = c(25, 40, 15, 35, 30),
    group = c("Group 1", "Group 2", "Group 1", "Group 2", "Group 1"),
    stringsAsFactors = FALSE
  )

  # Test order by values (y)
  result <- reorder_by(test_data, "category", "value", order = "y")
  expected_order <- c("D", "B", "E", "C", "A")
  expect_equal(as.character(result$category), expected_order)

  # Test reversed order
  result_rev <- reorder_by(
    test_data,
    "category",
    "value",
    order = "y",
    dir_order = -1
  )
  expect_equal(as.character(result_rev$category), rev(expected_order))

  # Test alphabetical order
  result_alpha <- reorder_by(test_data, "category", "value", order = "x")
  expect_equal(as.character(result_alpha$category), c("A", "B", "C", "D", "E"))
})

test_that("reorder_by works with factor columns", {
  test_data <- data.frame(
    category = factor(c("B", "A", "D", "C", "E")),
    value = c(25, 40, 15, 35, 30),
    group = factor(c("Group 1", "Group 2", "Group 1", "Group 2", "Group 1"))
  )

  # Test order by values (y)
  result <- reorder_by(test_data, "category", "value", order = "y")
  expect_true(is.factor(result$category))
  expect_equal(as.character(result$category), c("D", "B", "E", "C", "A"))

  # Test factor levels
  expect_equal(levels(result$category), c("D", "B", "E", "C", "A"))
})

test_that("reorder_by handles grouped ordering correctly", {
  test_data <- data.frame(
    category = c("B", "A", "D", "C", "E"),
    value = c(25, 40, 15, 35, 30),
    group = c("Group 1", "Group 2", "Group 1", "Group 2", "Group 1"),
    stringsAsFactors = FALSE
  )

  # Test grouped_y ordering
  result <- reorder_by(
    test_data,
    "category",
    "value",
    group = "group",
    order = "grouped_y"
  )

  # Group 1 items should be ordered by value within Group 1
  group1_items <- result[result$group == "Group 1", "category"]
  group1_values <- result[result$group == "Group 1", "value"]
  expect_true(all(group1_values == sort(group1_values)))

  # Group 2 items should be ordered by value within Group 2
  group2_items <- result[result$group == "Group 2", "category"]
  group2_values <- result[result$group == "Group 2", "value"]
  expect_true(all(group2_values == sort(group2_values)))

  # Test grouped_x ordering
  result_x <- reorder_by(
    test_data,
    "category",
    "value",
    group = "group",
    order = "grouped_x"
  )

  # Groups should remain intact, and items should be ordered alphabetically within groups
  group1_items_x <- as.character(result_x[
    result_x$group == "Group 1",
    "category"
  ])
  expect_equal(group1_items_x, sort(group1_items_x))
})

test_that("reorder_by handles fallback cases", {
  test_data <- data.frame(
    category = c("B", "A", "D", "C", "E"),
    value = c(25, 40, 15, 35, 30),
    stringsAsFactors = FALSE
  )

  # Test empty group with grouped_y
  expect_warning(
    result <- reorder_by(
      test_data,
      "category",
      "value",
      group = "",
      order = "grouped_y"
    ),
    "Group is empty"
  )
  # Should fall back to ordering by y
  expect_equal(as.character(result$category), c("D", "B", "E", "C", "A"))

  # Test empty group with grouped_x
  expect_warning(
    result_x <- reorder_by(
      test_data,
      "category",
      "value",
      group = "",
      order = "grouped_x"
    ),
    "Group is empty"
  )
  # Should fall back to alphabetical ordering
  expect_equal(as.character(result_x$category), c("A", "B", "C", "D", "E"))
})

test_that("reorder_by preserves row data correctly", {
  test_data <- data.frame(
    category = c("B", "A", "D", "C", "E"),
    value = c(25, 40, 15, 35, 30),
    extra = c("x1", "x2", "x3", "x4", "x5"),
    stringsAsFactors = FALSE
  )

  # Test that reordering preserves all columns and their associations
  result <- reorder_by(test_data, "category", "value", order = "y")

  # Values should correspond to the correct categories
  expect_equal(result$value[result$category == "A"], 40)
  expect_equal(result$value[result$category == "B"], 25)
  expect_equal(result$value[result$category == "C"], 35)
  expect_equal(result$value[result$category == "D"], 15)
  expect_equal(result$value[result$category == "E"], 30)

  # Extra column should maintain its associations
  expect_equal(result$extra[result$category == "A"], "x2")
  expect_equal(result$extra[result$category == "B"], "x1")
})

test_that("reorder_by handles no reordering case", {
  test_data <- data.frame(
    category = c("B", "A", "D", "C", "E"),
    value = c(25, 40, 15, 35, 30),
    stringsAsFactors = FALSE
  )

  # Test no reordering
  result <- reorder_by(test_data, "category", "value", order = "none")

  # Order should be preserved from the original data
  expect_equal(as.character(result$category), c("B", "A", "D", "C", "E"))
})

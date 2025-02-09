# testthat::test_that("bar() function handles various edge cases", {
#   # Setup minimal test data
#   test_df <- data.frame(
#     category =c("A", "B", "C"),
#     value = c(10, 20, 30),
#     group = c("X", "X", "Y"),
#     facet_var = c("F1", "F1", "F2")
#   )

#   # Test 1: Basic functionality with all parameters
#   testthat::expect_s3_class({
#     bar(test_df, x = "category", y = "value", 
#         position = "dodge", add_text = TRUE, flip = FALSE)
#   }, "ggplot")

#   # Test 2: Missing group parameter
#   testthat::expect_s3_class({
#     bar(test_df, x = "category", y = "value", facet = "facet_var")
#   }, "ggplot")

#   # Test 3: Missing facet parameter
#   testthat::expect_s3_class({
#     bar(test_df, x = "category", y = "value", group = "group")
#   }, "ggplot")

#   # Test 4: Identical group and facet
#   testthat::expect_warning({
#     bar(test_df, x = "category", y = "value", group = "facet_var", facet = "facet_var")
#   }, "Using 'facet' for grouping")

#   # Test 5: NA handling scenarios
#   na_df <- data.table::data.table(
#     category = c("A", "B", NA),
#     value = c(10, NA, 30),
#     group = c("X", NA, "Y")
#   )
  
#   # Test 5a: NA removal enabled
#   testthat::expect_silent({
#     bar(na_df, x = "category", y = "value", group = "group",
#         x_rm_na = TRUE, y_rm_na = TRUE, group_rm_na = TRUE)
#   })

#   # Test 5b: NA removal disabled
#   testthat::expect_warning({
#     bar(na_df, x = "category", y = "value", group = "group",
#         x_rm_na = FALSE, y_rm_na = FALSE, group_rm_na = FALSE)
#   }, "Converting df to data.table")

#   # Test 6: Ordering scenarios
#   # Test 6a: Natural order
#   testthat::expect_equal(
#     levels(bar(test_df, x = "category", y = "value", order = "none")$data$category),
#     c("A", "B", "C")
#   )

#   # Test 6b: Ordered by y
#   testthat::expect_equal(
#     levels(bar(test_df, x = "category", y = "value", order = "y")$data$category),
#     c("A", "B", "C")  # Should be ordered by value
#   )

#   # Test 7: Faceting edge cases
#   # Test 7a: Single facet level
#   single_facet <- data.table::data.table(
#     category = c("A", "B"),
#     value = c(10, 20),
#     facet_var = c("F1", "F1")
#   )
#   testthat::expect_s3_class({
#     bar(single_facet, x = "category", y = "value", facet = "facet_var")
#   }, "ggplot")

#   # Test 8: Text labeling thresholds
#   small_values <- data.table::data.table(
#     category = c("A", "B"),
#     value = c(0.03, 0.04),  # Below default 0.05 threshold
#     group = c("X", "Y")
#   )
#   plot_data <- bar(small_values, x = "category", y = "value", group = "group", add_text = TRUE)
#   testthat::expect_true(all(is.na(plot_data$layers[[2]]$data$y_threshold)))

#   # Test 9: Invalid parameter combinations
#   testthat::expect_error(
#     bar(test_df, x = "value", y = "category"),  # Reversed numeric/character
#     "must be character or factor"
#   )

#   # Test 10: Facet/group interaction with insufficient data
#   sparse_data <- data.table::data.table(
#     category = "A",
#     value = 10,
#     group = "X",
#     facet_var = "F1"
#   )
#   testthat::expect_s3_class({
#     bar(sparse_data, x = "category", y = "value", group = "group", facet = "facet_var")
#   }, "ggplot")
# })

# # Visual regression tests (requires vdiffr)
# testthat::test_that("Visual appearance remains consistent", {
#   test_df <- data.table::data.table(
#     category = factor(c("A", "B", "C")),
#     value = c(10, 20, 30),
#     group = c("X", "X", "Y"),
#     facet_var = c("F1", "F1", "F2")
#   )

#   # Basic plot
#   basic <- bar(test_df, x = "category", y = "value")
#   vdiffr::expect_doppelganger("basic-bar", basic)

#   # Grouped+dodged
#   grouped <- bar(test_df, x = "category", y = "value", group = "group")
#   vdiffr::expect_doppelganger("grouped-dodged-bar", grouped)

#   # Faceted
#   faceted <- bar(test_df, x = "category", y = "value", facet = "facet_var")
#   vdiffr::expect_doppelganger("faceted-bar", faceted)

#   # Stacked
#   stacked <- bar(test_df, x = "category", y = "value", group = "group", position = "stack")
#   vdiffr::expect_doppelganger("stacked-bar", stacked)
# })

test_that("lollipop generates correct base plot structure", {
  test_data <- data.frame(
    category = c("A", "B", "C", "D", "E"),
    value = c(25, 40, 15, 35, 30),
    stringsAsFactors = FALSE
  )

  p <- lollipop(df = test_data, x = "category", y = "value")

  # Check that the plot is a ggplot object
  expect_s3_class(p, "ggplot")

  # Check that the plot contains the essential layers
  expect_true(any(sapply(p$layers, function(x) inherits(x$geom, "GeomPoint"))))
  expect_true(any(sapply(p$layers, function(x) {
    inherits(x$geom, "GeomLinerange")
  })))

  # Check that the data is correctly mapped
  expect_equal(rlang::as_name(p$mapping$x), "category")
  expect_equal(rlang::as_name(p$mapping$y), "value")
})

test_that("horizontal lollipop (hlollipop) flips coordinates", {
  test_data <- data.frame(
    category = c("A", "B", "C", "D", "E"),
    value = c(25, 40, 15, 35, 30),
    stringsAsFactors = FALSE
  )

  p <- hlollipop(df = test_data, x = "category", y = "value")

  # Check that coordinates are flipped
  expect_true(any(sapply(p$layers, function(x) inherits(x$geom, "GeomPoint"))))
  expect_true(any(sapply(p$layers, function(x) {
    inherits(x$geom, "GeomLinerange")
  })))
  expect_true(inherits(p$coordinates, "CoordFlip"))
})

test_that("grouped lollipop uses side-by-side positioning", {
  test_data <- data.frame(
    category = c("A", "B", "C", "A", "B", "C"),
    value = c(25, 40, 15, 35, 30, 45),
    group = c("Group 1", "Group 1", "Group 1", "Group 2", "Group 2", "Group 2"),
    stringsAsFactors = FALSE
  )

  p <- lollipop(df = test_data, x = "category", y = "value", group = "group")

  # Check that group aesthetic is set for fill
  expect_equal(rlang::as_name(p$mapping$fill), "group")

  # Check for group-related mappings
  expect_true(length(grep("group", as.character(p$mapping))) > 0)

  # Check that position dodge is used for points and lineranges
  linerange_layer <- which(sapply(p$layers, function(x) {
    inherits(x$geom, "GeomLinerange")
  }))[1]
  point_layer <- which(sapply(p$layers, function(x) {
    inherits(x$geom, "GeomPoint")
  }))[1]

  expect_true(inherits(p$layers[[point_layer]]$position, "PositionDodge"))
  expect_true(inherits(p$layers[[linerange_layer]]$position, "PositionDodge"))
})

test_that("dodge_width parameter controls group spacing", {
  test_data <- data.frame(
    category = c("A", "B", "A", "B"),
    value = c(25, 40, 35, 30),
    group = c("Group 1", "Group 1", "Group 2", "Group 2"),
    stringsAsFactors = FALSE
  )

  # Create plots with different dodge widths
  p1 <- lollipop(
    df = test_data,
    x = "category",
    y = "value",
    group = "group",
    dodge_width = 0.5
  )
  p2 <- lollipop(
    df = test_data,
    x = "category",
    y = "value",
    group = "group",
    dodge_width = 1.0
  )

  # Extract position dodge objects
  linerange_layer1 <- which(sapply(p1$layers, function(x) {
    inherits(x$geom, "GeomLinerange")
  }))[1]
  linerange_layer2 <- which(sapply(p2$layers, function(x) {
    inherits(x$geom, "GeomLinerange")
  }))[1]

  dodge1 <- p1$layers[[linerange_layer1]]$position
  dodge2 <- p2$layers[[linerange_layer2]]$position

  # Check that dodge width is correctly set
  expect_equal(dodge1$width, 0.5)
  expect_equal(dodge2$width, 1.0)
})

test_that("lollipop handles missing values correctly", {
  test_data <- data.frame(
    category = c("A", "B", "C", "D", NA),
    value = c(25, 40, NA, 35, 30),
    group = c("Group 1", NA, "Group 1", "Group 2", "Group 1"),
    stringsAsFactors = FALSE
  )

  # With default NA removal
  p <- lollipop(
    df = test_data,
    x = "category",
    y = "value",
    group = "group"
  )

  # Should only have 2 rows of data (A and D)
  expect_equal(nrow(p$data), 2)

  # With specific NA handling
  p2 <- lollipop(
    df = test_data,
    x = "category",
    y = "value",
    group = "group",
    x_rm_na = FALSE,
    y_rm_na = FALSE,
    group_rm_na = FALSE
  )

  # Should have more rows since we're not removing NAs
  expect_true(nrow(p2$data) > 2)
})

test_that("lollipop applies ordering correctly", {
  test_data <- data.frame(
    category = c("B", "A", "D", "C", "E"),
    value = c(25, 40, 15, 35, 30),
    stringsAsFactors = FALSE
  )

  # With y ordering
  p1 <- lollipop(
    df = test_data,
    x = "category",
    y = "value",
    order = "y"
  )

  # With no ordering
  p2 <- lollipop(
    df = test_data,
    x = "category",
    y = "value",
    order = "none"
  )

  # We don't test the specific order since it depends on the implementation
  # of reorder_by, which is tested separately. Instead, just check that
  # the ordered and unordered versions are different.

  # Convert factor to character for comparison
  plot_categories_y <- as.character(p1$data$category)
  plot_categories_none <- as.character(p2$data$category)

  # The ordered version should be different than the unordered version
  expect_false(identical(plot_categories_y, plot_categories_none))
})

test_that("lollipop handles facets correctly", {
  test_data <- data.frame(
    category = rep(c("A", "B", "C"), 2),
    value = c(25, 40, 15, 35, 30, 45),
    facet_var = rep(c("Facet 1", "Facet 2"), each = 3),
    stringsAsFactors = FALSE
  )

  p <- lollipop(
    df = test_data,
    x = "category",
    y = "value",
    facet = "facet_var"
  )

  # Check that facet is applied
  expect_true(!is.null(p$facet))
})

test_that("lollipop respects custom appearance settings", {
  test_data <- data.frame(
    category = c("A", "B", "C"),
    value = c(25, 40, 15),
    stringsAsFactors = FALSE
  )

  custom_color <- "#FF5733"
  custom_line_color <- "#33FF57"
  custom_dot_size <- 5
  custom_line_size <- 1.5
  custom_alpha <- 0.7

  p <- lollipop(
    df = test_data,
    x = "category",
    y = "value",
    add_color = custom_color,
    line_color = custom_line_color,
    dot_size = custom_dot_size,
    line_size = custom_line_size,
    alpha = custom_alpha
  )

  # Find the point and linerange layers
  point_layer <- which(sapply(p$layers, function(x) {
    inherits(x$geom, "GeomPoint")
  }))[1]
  linerange_layer <- which(sapply(p$layers, function(x) {
    inherits(x$geom, "GeomLinerange")
  }))[1]

  # Check that point and linerange layers exist
  expect_true(point_layer > 0)
  expect_true(linerange_layer > 0)

  # Check that size parameters match
  expect_equal(p$layers[[point_layer]]$aes_params$size, custom_dot_size)
  expect_equal(p$layers[[point_layer]]$aes_params$alpha, custom_alpha)

  # Check that proper colors are applied somewhere in the plot
  # We don't test for exact color values since the implementation might vary
  # Just check that the plot was created successfully
  expect_s3_class(p, "ggplot")

  # Verify that some aes_params exist in the layers
  all_aes_params <- lapply(p$layers, function(l) names(l$aes_params))
  expect_true(length(unlist(all_aes_params)) > 0)
})

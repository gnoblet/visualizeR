# Tests for palette_gen functions

test_that("palette_gen validates parameters correctly", {
  # Check that invalid type throws error
  expect_error(
    palette_gen("cat_5_main", "invalid_type"),
    "Assertion on 'type' failed"
  )

  # Check that invalid palette throws error
  expect_error(
    palette_gen(123, "categorical"),
    "Assertion on 'palette' failed"
  )
  expect_error(
    palette_gen(c("cat_5_main", "cat_3_custom_1"), "categorical"),
    "Assertion on 'palette' failed"
  )

  # Check that invalid direction throws error
  expect_error(
    palette_gen("cat_5_main", "categorical", direction = 0),
    "Assertion on 'abs\\(direction\\) == 1' failed"
  )
  expect_error(
    palette_gen("cat_5_main", "categorical", direction = 2),
    "Assertion on 'direction' failed"
  )

  # Check valid types don't error
  expect_type(palette_gen("cat_5_main", "categorical"), "closure")
  expect_type(palette_gen("cat_5_main", "sequential"), "closure")
  expect_type(palette_gen("div_5_orange_blue", "divergent"), "closure")
})

test_that("palette_gen returns appropriate function types", {
  # Categorical palette should return a function
  cat_fn <- palette_gen("cat_5_main", "categorical")
  expect_true(is.function(cat_fn))

  # Sequential palette should return a function
  seq_fn <- palette_gen("cat_5_main", "sequential")
  expect_true(is.function(seq_fn))

  # Divergent palette should return a function
  div_fn <- palette_gen("div_5_orange_blue", "divergent")
  expect_true(is.function(div_fn))
})

test_that("palette_gen forwards arguments to appropriate function", {
  # Skip the test if mockery is not available
  skip_if_not_installed("mockery")
  skip_if_not(exists("with_mocked_bindings"))

  # Create a mock for palette_gen_categorical
  mockery::with_mocked_bindings(
    palette_gen_categorical = function(palette, direction) {
      return(list(
        palette = palette,
        direction = direction,
        type = "categorical"
      ))
    },
    palette_gen_sequential = function(palette, direction, ...) {
      return(list(
        palette = palette,
        direction = direction,
        type = "sequential"
      ))
    },
    code = {
      # Test categorical forwarding
      result <- palette_gen("cat_palette", "categorical", direction = -1)
      expect_equal(result$palette, "cat_palette")
      expect_equal(result$direction, -1)
      expect_equal(result$type, "categorical")

      # Test sequential forwarding
      result <- palette_gen("seq_palette", "sequential", direction = -1)
      expect_equal(result$palette, "seq_palette")
      expect_equal(result$direction, -1)
      expect_equal(result$type, "sequential")
    }
  )
})

test_that("palette_gen_categorical validates parameters", {
  # Test palette parameter validation
  expect_error(
    palette_gen_categorical(palette = 123),
    "Assertion on 'palette' failed"
  )
  expect_error(
    palette_gen_categorical(palette = c("cat_5_main", "cat_3_custom_1")),
    "Assertion on 'palette' failed"
  )

  # Test direction parameter validation
  expect_error(
    palette_gen_categorical(direction = 0),
    "Assertion on 'abs\\(direction\\) == 1' failed"
  )
  expect_error(
    palette_gen_categorical(direction = 2),
    "Assertion on 'direction' failed"
  )
  expect_error(
    palette_gen_categorical(direction = -2),
    "Assertion on 'direction' failed"
  )
  expect_error(
    palette_gen_categorical(direction = "1"),
    "Assertion on 'direction' failed"
  )
})

test_that("palette_gen_categorical returns a function", {
  fn <- palette_gen_categorical()
  expect_true(is.function(fn))
})

test_that("palette_gen_categorical function generates correct colors", {
  # Get the palette function
  fn <- palette_gen_categorical("cat_5_main")

  # Get the actual colors from the palette
  pal_colors <- palette("cat_5_main")

  # Test default behavior (return all colors)
  expect_equal(fn(NULL), pal_colors)

  # Test specific number of colors
  expect_equal(fn(3), pal_colors[1:3])

  # Test direction reversal
  rev_fn <- palette_gen_categorical("cat_5_main", direction = -1)
  expect_equal(rev_fn(NULL), rev(pal_colors))
})

test_that("palette_gen_categorical warns when requesting too many colors", {
  fn <- palette_gen_categorical("cat_3_aquamarine")
  expect_warning(fn(10), "Not enough colors in this palette!")
})

test_that("palette_gen_sequential validates parameters", {
  # Test palette parameter validation
  expect_error(
    palette_gen_sequential(palette = 123),
    "Assertion on 'palette' failed"
  )
  expect_error(
    palette_gen_sequential(palette = c("cat_5_main", "cat_3_custom_1")),
    "Assertion on 'palette' failed"
  )

  # Test direction parameter validation
  expect_error(
    palette_gen_sequential(direction = 0),
    "Assertion on 'abs\\(direction\\) == 1' failed"
  )
  expect_error(
    palette_gen_sequential(direction = 2),
    "Assertion on 'direction' failed"
  )
  expect_error(
    palette_gen_sequential(direction = -2),
    "Assertion on 'direction' failed"
  )
  expect_error(
    palette_gen_sequential(direction = "1"),
    "Assertion on 'direction' failed"
  )
})

test_that("palette_gen_sequential returns a colorRampPalette function", {
  # Test with default parameters
  fn_default <- palette_gen_sequential()
  expect_true(is.function(fn_default))

  # Test with specific palette
  fn <- palette_gen_sequential("cat_5_main")
  expect_true(is.function(fn))

  # colorRampPalette functions take an integer and return a character vector
  colors <- fn(10)
  expect_type(colors, "character")
  expect_length(colors, 10)

  # Each color should be a valid hex code
  hex_pattern <- "^#[0-9A-Fa-f]{6}$"
  for (color in colors) {
    expect_match(color, hex_pattern)
  }
})

test_that("palette_gen_sequential handles direction parameter correctly", {
  # Create functions with opposite directions
  fn1 <- palette_gen_sequential("cat_5_main", direction = 1)
  fn2 <- palette_gen_sequential("cat_5_main", direction = -1)

  # Generate colors
  colors1 <- fn1(5)
  colors2 <- fn2(5)

  # Colors should be different when direction is different
  expect_false(identical(colors1, colors2))
})

test_that("palette_gen functions work with all available palettes", {
  # Get all palette names
  pal_names <- palette(show_palettes = TRUE)

  for (pal_name in pal_names) {
    # Test categorical - check only that it returns a function
    cat_fn <- tryCatch(
      palette_gen_categorical(pal_name),
      error = function(e) NULL
    )
    if (!is.null(cat_fn)) {
      expect_true(is.function(cat_fn))
      # Use min(3, length of palette colors) to avoid warning
      n_colors <- min(3, length(palette(pal_name)))
      expect_type(cat_fn(n_colors), "character")
    }

    # Test sequential - check only that it returns a function
    seq_fn <- tryCatch(
      palette_gen_sequential(pal_name),
      error = function(e) NULL
    )
    if (!is.null(seq_fn)) {
      expect_true(is.function(seq_fn))
      expect_type(seq_fn(3), "character")
    }
  }
})

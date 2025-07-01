# Tests for palette functions

test_that("palette returns expected color vectors", {
  # Test default palette
  default_pal <- palette()
  expect_type(default_pal, "character")
  expect_length(default_pal, 5) # cat_5_main has 5 colors

  # Test specific palette
  cat_3_pal <- palette("cat_3_aquamarine")
  expect_type(cat_3_pal, "character")
  expect_length(cat_3_pal, 3)

  # Test reversed palette
  reversed_pal <- palette("cat_5_main", reverse = TRUE)
  expect_equal(reversed_pal, rev(palette("cat_5_main")))

  # Test custom palettes
  custom_pal <- palette("cat_3_custom_1")
  expect_equal(custom_pal, c("#003F5C", "#58508D", "#FFA600"))
})

test_that("show_palettes option works", {
  # Test show_palettes parameter
  pal_names <- palette(show_palettes = TRUE)
  expect_type(pal_names, "character")
  expect_true(length(pal_names) > 0)
  expect_true("cat_5_main" %in% pal_names)
  expect_true("cat_3_custom_1" %in% pal_names)
})

test_that("palette handles errors correctly", {
  # Test non-existent palette
  expect_error(
    palette("not_a_real_palette"),
    "Palette not defined"
  )

  # Test parameter validation
  expect_error(palette(palette = 123))
  expect_error(palette(palette = c("cat_5_main", "cat_3_custom_1")))
  expect_error(palette(reverse = "yes"))
  expect_error(palette(show_palettes = "TRUE"))
})

test_that("all palette entries are valid colors", {
  # Get all palette names
  pal_names <- palette(show_palettes = TRUE)

  # Check each palette contains valid colors
  for (pal_name in pal_names) {
    pal_colors <- palette(pal_name)

    # Check that each color can be processed by grDevices
    for (color in pal_colors) {
      expect_true(
        # Check if the color is valid by converting it to RGB
        !is.na(grDevices::col2rgb(color)[1, 1]),
        info = paste("Invalid color in palette", pal_name, ":", color)
      )
    }
  }
})

test_that("palettes have expected lengths", {
  # Check specific palette lengths
  expect_length(palette("cat_2_yellow"), 2)
  expect_length(palette("cat_3_aquamarine"), 3)
  expect_length(palette("cat_5_main"), 5)
  expect_length(palette("cat_8_tol_adapted"), 8)
})

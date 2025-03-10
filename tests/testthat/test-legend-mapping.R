test_that("legend_revert_colours parameter works correctly", {
  # Test case 1: When legend_revert_colours is TRUE
  legend_paras <- list(legend_revert_colours = TRUE)
  test_vals <- c(1, 2, 3, 4, 5)
  
  # Create minimal test data
  leg_vals <- test_vals
  is_categorical <- FALSE
  
  # Call the function with required parameters
  result <- devPTIpack:::legend_map_satelite(
    leg_vals = leg_vals,
    legend_paras = legend_paras,
    is_categorical = is_categorical,
    n_groups = 5
  )
  
  # The function should return a list with a pal function
  expect_true(!is.null(result$pal))
  expect_type(result$pal, "closure")
  
  # Generate colors for comparison
  colors1 <- result$pal(test_vals)
  expect_equal(length(colors1), length(test_vals))
  
  # Test case 2: When legend_revert_colours is FALSE
  legend_paras <- list(legend_revert_colours = FALSE)
  result2 <- devPTIpack:::legend_map_satelite(
    leg_vals = leg_vals,
    legend_paras = legend_paras,
    is_categorical = is_categorical,
    n_groups = 5
  )
  
  # Generate colors for comparison
  colors2 <- result2$pal(test_vals)
  
  # Colors should be different when reversed vs not reversed
  expect_false(identical(colors1, colors2))
  
  # Test case 3: When legend_revert_colours is not provided
  result3 <- devPTIpack:::legend_map_satelite(
    leg_vals = leg_vals,
    legend_paras = list(),
    is_categorical = is_categorical,
    n_groups = 5
  )
  
  # Generate colors for comparison
  colors3 <- result3$pal(test_vals)
  
  # Should default to non-reversed colors (same as FALSE)
  expect_identical(colors2, colors3)
  
  # Test case 4: When legend_paras is NULL
  result4 <- devPTIpack:::legend_map_satelite(
    leg_vals = leg_vals,
    legend_paras = NULL,
    is_categorical = is_categorical,
    n_groups = 5
  )
  
  # Generate colors for comparison
  colors4 <- result4$pal(test_vals)
  
  # Should default to non-reversed colors
  expect_identical(colors2, colors4)
})

test_that("legend_revert_colours handles invalid inputs gracefully", {
  leg_vals <- c(1, 2, 3, 4, 5)
  is_categorical <- FALSE
  
  # Test with invalid values that should default to FALSE
  invalid_values <- list(
    "TRUE",     # character instead of logical
    1,          # numeric instead of logical
    NA,         # NA
    NULL        # NULL
  )
  
  for (invalid_value in invalid_values) {
    legend_paras <- list(legend_revert_colours = invalid_value)
    result <- devPTIpack:::legend_map_satelite(
      leg_vals = leg_vals,
      legend_paras = legend_paras,
      is_categorical = is_categorical,
      n_groups = 5
    )
    
    # Should not error and should return a valid pal function
    expect_true(!is.null(result$pal))
    expect_type(result$pal, "closure")
    
    # Generate colors to ensure it works
    colors <- result$pal(leg_vals)
    expect_equal(length(colors), length(leg_vals))
  }
}) 
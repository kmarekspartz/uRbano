test_that("transform_to_meters works with NAD", {
  # Create a dummy sf object
  dummy_sf <- sf::st_as_sf(
    data.frame(x = -100, y = 40),
    coords = c("x", "y"),
    crs = 4326
  )

  # Transform it
  transformed_sf <- transform_to_meters(dummy_sf, method = "NAD")

  # Check that the CRS is correct
  expect_equal(sf::st_crs(transformed_sf)$epsg, 26914)
})

test_that("transform_to_meters works with NAD", {
  # Create a dummy sf object
  dummy_sf <- sf::st_as_sf(data.frame(x = -100, y = 40), coords = c("x", "y"), crs = 4326)

  # Transform it
  transformed_sf <- transform_to_meters(dummy_sf, method = "NAD")

  # Check that the CRS is correct
  expect_equal(sf::st_crs(transformed_sf)$epsg, 26914)
})

test_that("transform_to_meters works with web_mercator", {
  # Create a dummy sf object
  dummy_sf <- sf::st_as_sf(data.frame(x = -100, y = 40), coords = c("x", "y"), crs = 4326)

  # Transform it
  transformed_sf <- transform_to_meters(dummy_sf)

  # Check that the CRS is correct
  expect_equal(sf::st_crs(transformed_sf)$epsg, 3857)
})

test_that("transform_to_meters works with NAD and different longitude", {
  # Create a dummy sf object
  dummy_sf <- sf::st_as_sf(data.frame(x = -90, y = 40), coords = c("x", "y"), crs = 4326)

  # Transform it
  transformed_sf <- transform_to_meters(dummy_sf, method = "NAD")

  # Check that the CRS is correct
  expect_equal(sf::st_crs(transformed_sf)$epsg, 26916)
})

test_that("transform_to_meters handles z10 case", {
  # Create a dummy sf object
  dummy_sf <- sf::st_as_sf(data.frame(x = -125, y = 40), coords = c("x", "y"), crs = 4326)

  # Transform it
  transformed_sf <- transform_to_meters(dummy_sf, method = "NAD")

  # Check that the CRS is correct
  expect_equal(sf::st_crs(transformed_sf)$epsg, 26910)
})


test_that("transform_to_meters handles coordinates outside North America", {
  # Create a dummy sf object
  dummy_sf <- sf::st_as_sf(data.frame(x = 0, y = 40), coords = c("x", "y"), crs = 4326)

  # Transform it and check for the warning
  expect_warning(transformed_sf <- transform_to_meters(dummy_sf, method = "NAD"))

  # Check that the CRS is unchanged
  expect_equal(sf::st_crs(transformed_sf)$epsg, 4326)
})

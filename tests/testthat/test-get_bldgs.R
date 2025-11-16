library(sf)

test_that("get_bldgs works with a single region", {
  skip_if_offline()

  # Get buildings for a small region
  blds <- get_bldgs("RhodeIsland")

  # Check that the result is an sf object
  expect_s3_class(blds, "sf")
})

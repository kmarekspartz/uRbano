#' @title Trim building footprints and remove invalid geometries
#'
#' @description This function is optional but recommended. It reduces the building footprint data to the extent of a given radius and removes invalid geometries to prevent errors in area calculations.
#'
#' @param buildings A large `sfc` object of polygons, typically the output of `get_buildings`.
#' @param radius A polygon representing the extent to which the building footprints should be clipped, typically the output of `get_city_radius`.
#'
#' @return A large `sfc` object of polygons representing building footprints, with invalid geometries removed and trimmed to the specified extent.
#'
#' @export
#'
#' @examples
#' # Fix invalid geometries and clip building footprints to the city radius
#' # buildings <- uRbano::fix_and_trim_buildings(buildings, city_radius)
fix_and_trim_buildings <- function(buildings, radius) {
  city_extent <- terra::ext(radius)
  bounding_box <- sf::st_bbox(city_extent, crs = 4326)
  bounding_box_sf <- sf::st_as_sfc(bounding_box)
  sf::sf_use_s2(FALSE) # turn off spherical geometry
  buildings <- sf::st_filter(buildings, bounding_box_sf)
  gc()

  invalid_geometries <- buildings[!sf::st_is_valid(buildings), ]
  if (nrow(invalid_geometries) > 0) {
    invalid_indices <- as.numeric(rownames(invalid_geometries))
    buildings <- buildings[-c(invalid_indices), ]
    sf::sf_use_s2(TRUE)
    return(buildings)
  } else {
    sf::sf_use_s2(TRUE)
    return(buildings)
  }
}

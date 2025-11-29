#' @title trim building footprints to a smaller extent and remove invalid geometries
#'
#' @description OPTIONAL BUT RECOMMENDED - reduce buildings to extent of a
#' radius and dplyr::filter out invalid geometries to avoid errors in
#' calculating terra::area
#'
#' @param buildings (sfc) large simple features collection of polygons, output of get_buildings
#' @param radius (polygon) output of get_city_radius or another extent to clip the
#' building footprints to
#
#' @return large simple features object of polygons representing building
#' footprints in invalid geometries omitted and trimmed to the extent of the
#' second parameter, rad
#'
#' @export
#'
#' @examples
#' # fix invalid geometries and clip building footprints to city radius
#' # buildings<-uRbano::fix_and_trim_buildings(buildings, rad)
fix_and_trim_buildings <- function(buildings, radius) {
  city_extent <- terra::ext(radius)
  bbox <- sf::st_bbox(city_extent, crs = 4326)
  bbox_sf <- sf::st_as_sfc(bbox)
  sf::sf_use_s2(FALSE) # turn off spherical geometry
  buildings <- sf::st_filter(buildings, bbox_sf)
  gc()

  invalid_geometries <- buildings[!sf::st_is_valid(buildings), ]
  if (nrow(invalid_geometries) > 0) {
    x_buildings <- as.numeric(rownames(invalid_geometries))
    buildings <- buildings[-c(x_buildings), ]
  }
  sf::sf_use_s2(TRUE)
  buildings
}

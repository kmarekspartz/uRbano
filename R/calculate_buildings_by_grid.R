#' @title calculate total building footprint area within each hexagon of the grid
#'
#' @description takes the result of create_hexagonal_grid and get_buildings to sum the area of building footprints (not including height info) in meters within each hexagon of the grid
#'
#' @param grid (sf object) output of create_hexagonal_grid
#' @param buildings (sf object) building footprints sf object
#
#' @return vector of total building area in meters that is the same length as the number of rows in the grid sf object
#'
#' @export
#'
#' @examples
#' # calculate the summed building footprint area per each 1km hexagon in the grid
#' # building_areas <- uRbano::calculate_buildings_by_grid(r_grid, buildings)
#' # add column for building area to the hex grid
#' # r_grid <- r_grid %>% dplyr::mutate(building_areas = building_areas)
#'
calculate_buildings_by_grid <- function(grid, buildings) {
  ID <- NULL
  area <- NULL
  if (!inherits(grid, "sf") || !inherits(buildings, "sf")) {
    stop("Both grid and roads must be sf objects")
  }

  # ensure parameters have same CRS
  if ((terra::crs(grid)) != (terra::crs(buildings))) {
    stop(
      "Both grid and roads must be transformed to same UTM zone, use transform_US_utm()"
    )
  }

  grid <- grid %>% dplyr::mutate(ID = rownames(grid))

  building_clips <- sf::st_intersection(grid, buildings)

  building_clips <- building_clips %>% dplyr::mutate(area = sf::st_area(building_clips))
  sum_of_areas <- building_clips %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(total_area = sum(area))

  building_join <- dplyr::left_join(grid, sf::st_drop_geometry(sum_of_areas), by = "ID")

  # Return as a numeric vector
  return(units::drop_units(building_join$total_area))
}

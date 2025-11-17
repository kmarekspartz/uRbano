#' @title calculate total road length within each hexagon of the grid
#'
#' @description takes the result of create_hexagonal_grid and extract_osm_roads to sum the length of road segments in meters within each hexagon of the grid
#'
#' @param grid (sf object) output of create_hexagonal_grid
#' @param roads (sf object) OSM roads sf object
#
#' @return vector of total road length in meters that is the same length as the number of rows in the grid sf object
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' # calculate the summed length of road segments per each 1km hexagon in the grid
#' # road_lengths <- uRbano::calculate_roads_by_grid(r_grid, roads)
#' # add column for road length to the hex grid
#' # r_grid <- r_grid %>% dplyr::mutate(road_lengths = road_lengths)
#'
calculate_roads_by_grid <- function(grid, roads) {
  ID <- NULL
  road_length <- NULL
  # Ensure parameters are sf objects
  if (!inherits(grid, "sf") || !inherits(roads, "sf")) {
    stop("Both grid and roads must be sf objects")
  }

  # ensure parameters have same CRS
  if ((terra::crs(grid)) != (terra::crs(roads))) {
    stop(
      "Both grid and roads must be transformed to same UTM zone, use transform_US_utm()"
    )
  }

  grid <- grid %>% dplyr::mutate(ID = rownames(grid))

  road_clips <- sf::st_intersection(grid, roads)

  road_clips <- road_clips %>% dplyr::mutate(road_length = sf::st_length(road_clips))
  sum_of_lengths <- road_clips %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(total_length = sum(road_length))

  road_join <- dplyr::left_join(grid, sf::st_drop_geometry(sum_of_lengths), by = "ID")

  # Return as a numeric vector
  return(units::drop_units(road_join$total_length))
}

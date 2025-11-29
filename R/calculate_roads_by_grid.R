#' @title calculate total road length within each hexagon of the grid
#'
#' @description takes the result of radius_hex_grid and extract_osm_roads to sum
#' the length of road segments in meters within each hexagon of the grid
#'
#' @param grid (sf object) output of radius_hex_grid
#' @param roads (sf object) OSM roads sf object
#
#' @return vector of total road length in meters that is the same length as the number of rows in the grid sf object
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # calculate the summed length of road segments per each 1km hexagon in the grid
#' # rd_lens <- uRbano::calculate_roads_by_grid(r_grid, roads)
#' # add column for road length to the hex grid
#' # r_grid <- r_grid %>% dplyr::mutate(rd_lens = rd_lens)
#'
calculate_roads_by_grid <- function(grid, roads) {
  id <- NULL
  rd_len <- NULL
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

  grid <- grid %>% dplyr::mutate(id = rownames(grid))

  rd_clips <- sf::st_intersection(grid, roads)

  rd_clips <- rd_clips %>% dplyr::mutate(rd_len = sf::st_length(rd_clips))
  sum_length <- rd_clips %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(total_length = sum(rd_len))

  rd_jn <- dplyr::left_join(grid, sf::st_drop_geometry(sum_length), by = "id")

  # Return as a numeric vector
  units::drop_units(rd_jn$total_length)
}

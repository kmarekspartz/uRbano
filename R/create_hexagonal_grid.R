#' @title Create a hexagonal grid over a given area
#'
#' @description This function uses `sf::st_make_grid` to produce a hexagonal mesh over a specified `sf` object. If the input is in WGS 84, it approximates the hexagon size from meters to decimal degrees.
#'
#' @param area An `sf` object representing the area to cover with the grid, typically a city radius.
#' @param hexagon_size The width of each hexagon in meters.
#'
#' @return An `sf` object of the hexagonal mesh. Hexagons intersecting the edge of the area are retained, not clipped.
#'
#' @export
#'
#' @examples
#' # Create a grid of 1km hexagons across a city_radius polygon
#' # hexagonal_grid <- uRbano::create_hexagonal_grid(city_radius, 1000)
#'
create_hexagonal_grid <- function(area, hexagon_size) {
  predicate <- NULL
  if (sf::st_crs(area)$epsg == 4326) {
    grid <- sf::st_make_grid(
      area,
      square = FALSE,
      cellsize = (hexagon_size / 111000)
    ) %>%
      sf::st_sf()
    warning(
      "Converting hexagon size to units of lat/lon; size is an approximate conversion to meters."
    )
  } else {
    grid <- sf::st_make_grid(area, square = FALSE, cellsize = hexagon_size) %>%
      sf::st_sf()
  }
  # Get a predicate for which hexagons intersect the area
  intersects_area <- lengths(sf::st_intersects(grid, area)) > 0
  grid <- grid %>% dplyr::mutate(predicate = intersects_area)
  # Filter the grid to keep only intersecting hexagons
  grid <- grid %>% dplyr::filter(predicate == TRUE)
  return(grid)
}

#' @title Get road features from the OSM Overpass API
#'
#' @description This function queries the OpenStreetMap (OSM) Overpass API to retrieve road features within the bounding box of a given city radius.
#'
#' @param city_radius An `sfc` object from which a bounding box can be extracted.
#'
#' @return An `sfc` object of LINESTRING features representing the road segments for the specified area.
#'
#' @importFrom magrittr `%>%`
#' @importFrom terra ext
#' @export
#'
#' @examples
#' # Get the city radius (e.g., for Minneapolis)
#' city_radius <- uRbano::get_city_radius("Minneapolis", 30000)
#'
#' # Extract OSM roads within the city's bounding box
#' roads <- uRbano::extract_osm_roads(city_radius = city_radius)
#'
extract_osm_roads <- function(city_radius) {
  if (sf::st_crs(city_radius)$epsg != 4326) {
    city_radius <- sf::st_transform(city_radius, crs = 4326)
  }
  city_extent <- terra::ext(city_radius)

  # Get roads from OSM API
  roads <- osmdata::opq(bbox = c(city_extent[1], city_extent[3], city_extent[2], city_extent[4])) %>%
    osmdata::add_osm_feature(
      key = "highway",
      value = c(
        "primary",
        "motorway_link",
        "unclassified",
        "tertiary",
        "residential",
        "motorway",
        "secondary",
        "service",
        "trunk",
        "living_street",
        "trunk_link",
        "primary_link",
        "secondary_link",
        "tertiary_link",
        "road"
      )
    ) %>%
    osmdata::osmdata_sf()
  osm_roads <- roads$osm_lines
  return(osm_roads)
}

#' @title Transform a Coordinate Reference System (CRS) to a meters-based datum
#'
#' @description This function transforms the CRS of an `sf` object to a meters-based datum. It supports Web Mercator (EPSG:3857) and can automatically determine the appropriate NAD 83 UTM zone for features in North America.
#'
#' @param sf_object An `sf` object to be transformed.
#' @param method A string specifying the transformation method: "web_mercator" for EPSG:3857, or "NAD" for the appropriate NAD 83 UTM zone.
#'
#' @return An `sf` object with the modified CRS.
#'
#' @export
#'
#' @examples
#' # Transform a city_radius object to the appropriate NAD 83 meters UTM zone
#' # city_radius_m <- uRbano::transform_crs_to_meters(city_radius, method = "NAD")
#'
transform_crs_to_meters <- function(sf_object, method = "web_mercator") {
  if (method == "web_mercator") {
    return(sf::st_transform(sf_object, 3857))
  } else if (method == "NAD") {
    transform_nad_utm <- function(sf_object) {
      longitude <- sf::st_coordinates(sf::st_centroid(sf_object))[, 1]
      utm_zone <- floor((longitude + 180) / 6) + 1
      epsg_code <- paste0("269", utm_zone)

      # Handle special cases for North America
      if (utm_zone < 10 || utm_zone > 19) {
          warning("Coordinates may be outside of standard North America UTM Zones. CRS transformation may be incorrect.")
      }

      return(sf::st_transform(sf_object, crs = as.numeric(epsg_code)))
    }

    return(transform_nad_utm(sf_object))
  }
}

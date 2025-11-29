#' @title transform coordinate reference system to meters datum
#'
#' @description Optional but recommended, transform buildings, roads and city
#' radius features to meters datum with either Web Mercator EPSG 3857 or look
#' up NAD 83 UTM zone to extract projection from the sf object parameter if
#' spatial features are in North America
#'
#' @param sf_data (sf object) sf object to transform
#' @param method (string) string that represents CRS transformation to perform:
#' "web-mercator" for EPSG 3857, "NAD" for NAD 83
#
#' @return sf object with modified CRS
#'
#' @export
#'
#' @examples
#' # transform to NAD 83 meters UTM
#' # rad<- uRbano::transform_to_meters(rad, method="NAD")
#'
transform_to_meters <- function(sf_data, method = "web-mercator") {
  if (method == "web-mercator") {
    sf::st_transform(sf_data, 3857)
  } else if (method == "NAD") {
    transform_nad_utm <- function(rad) {
      longitude <- sf::st_coordinates(sf::st_centroid(rad))[, 1]
      if (longitude >= (-126) && longitude < (-120)) {
        sf::st_transform(rad, crs = 26910)
      } else if (longitude >= (-120) && longitude < (-114)) {
        sf::st_transform(rad, crs = 26911)
      } else if (longitude >= (-114) && longitude < (-108)) {
        sf::st_transform(rad, crs = 26912)
      } else if (longitude >= (-108) && longitude < (-102)) {
        sf::st_transform(rad, crs = 26913)
      } else if (longitude >= (-102) && longitude < (-96)) {
        sf::st_transform(rad, crs = 26914)
      } else if (longitude >= (-96) && longitude < (-90)) {
        sf::st_transform(rad, crs = 26915)
      } else if (longitude >= (-90) && longitude < (-84)) {
        sf::st_transform(rad, crs = 26916)
      } else if (longitude >= (-84) && longitude < (-78)) {
        sf::st_transform(rad, crs = 26917)
      } else if (longitude >= (-78) && longitude < (-72)) {
        sf::st_transform(rad, crs = 26918)
      } else if (longitude >= (-72) && longitude < (-66)) {
        sf::st_transform(rad, crs = 26919)
      } else if (longitude >= (-180) && longitude < (-174)) {
        sf::st_transform(rad, crs = 26901)
      } else if (longitude >= (-174) && longitude < (-168)) {
        sf::st_transform(rad, crs = 26902)
      } else if (longitude >= (-168) && longitude < (-162)) {
        sf::st_transform(rad, crs = 26903)
      } else if (longitude >= (-162) && longitude < (-156)) {
        sf::st_transform(rad, crs = 26904)
      } else if (longitude >= (-156) && longitude < (-150)) {
        sf::st_transform(rad, crs = 26905)
      } else if (longitude >= (-150) && longitude < (-144)) {
        sf::st_transform(rad, crs = 26906)
      } else if (longitude >= (-144) && longitude < (-138)) {
        sf::st_transform(rad, crs = 26907)
      } else if (longitude >= (-138) && longitude < (-132)) {
        sf::st_transform(rad, crs = 26908)
      } else if (longitude >= (-132) && longitude < (-126)) {
        sf::st_transform(rad, crs = 26909)
      } else {
        warning("Coordinates outside North America UTM Zones")
        sf::st_transform(rad, crs = 4326)
      }
    }

    transform_nad_utm(sf_data)
  }
}

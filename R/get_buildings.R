#' @title Get building footprints for a region of interest
#'
#' @description Extracts building footprint data from Microsoft's ML Building Footprints for one or two specified regions. This is useful for analyzing border cities. The data is released under an ODbL license.
#'
#' @param region1 A character string of the primary region to look up in the online building footprint releases.
#' @param region2 An optional character string for a second region.
#'
#' @return A large `sf` object of polygons representing building footprints. If `region2` is in a different country and the footprints cannot be combined, a list containing two `sf` objects is returned.
#'
#' @export
#'
#' @examples
#' # Get building footprints for a US state
#' # buildings <- uRbano::get_buildings(region1 = "North Dakota")
#'
#' # Get building footprints for two US states
#' # buildings <- uRbano::get_buildings(region1 = "North Dakota", region2 = "Minnesota")
#'
#' # Get building footprints for a Canadian province
#' # buildings <- uRbano::get_buildings(region1 = "Manitoba")
#'
#' # Get buildings for other countries or territories
#' # buildings <- uRbano::get_buildings(region1 = "PuertoRico")
#'
#' # Get footprints for two regions in different countries
#' # buildings <- uRbano::get_buildings(region1 = "North Dakota", region2 = "Manitoba")
#' # nd_buildings <- buildings$region1
#' # mb_buildings <- buildings$region2
get_buildings <- function(region1, region2 = NULL) {
  if (region1 == "UnitedStates") {
    stop("For efficiency, please use individual US states as inputs.")
  }
  if (region1 == "Canada") {
    stop("For efficiency, please use Canadian provinces as inputs.")
  }

  us_states <- gsub(
    "\\b([a-z])",
    "\\U\\1",
    c(unique(sub(":.*", "", maps::state.fips$polyname)), "alaska", "hawaii"),
    perl = TRUE
  )
  canadian_provinces <- c(
    "Alberta", "British Columbia", "Manitoba", "New Brunswick",
    "Newfoundland and Labrador", "Northwest Territories", "Nova Scotia",
    "Nunavut", "Ontario", "Prince Edward Island", "Quebec",
    "Saskatchewan", "Yukon"
  )
  all_regions <- utils::read.table(
    "https://minedbuildings.z5.web.core.windows.net/global-buildings/dataset-links.csv",
    header = TRUE,
    sep = ","
  )

  get_footprints_by_region <- function(region) {
    Location <- NULL
    if (region %in% us_states) {
      region_no_space <- gsub(" ", "", region)
      footprint_link <- paste0(
        "https://minedbuildings.z5.web.core.windows.net/legacy/usbuildings-v2/",
        region_no_space,
        ".geojson.zip"
      )
    } else if (region %in% canadian_provinces) {
      region_no_space <- gsub(" ", "", region)
      footprint_link <- paste0(
        "https://minedbuildings.z5.web.core.windows.net/legacy/canadian-buildings-v2/",
        region_no_space,
        ".zip"
      )
    } else if (region %in% all_regions$Location) {
      region_features <- all_regions %>% dplyr::filter(Location == region)
      footprint_urls <- list()
      for (url in region_features$Url) {
        footprint_urls[[url]] <- readr::read_csv2(url, col_names = FALSE)
      }
      combined_footprints <- do.call(rbind, footprint_urls)
      county_features <- geojsonsf::geojson_sfc(combined_footprints$X1)
      return(sf::st_as_sf(county_features, crs = 4326))
    } else {
      warning("Cannot find buildings for the specified region.")
      return(NULL)
    }

    message(paste("Getting building footprints from", footprint_link))
    temp_file <- tempfile()
    try(utils::download.file(footprint_link, temp_file, method = "curl"))
    if (is.na(file.size(temp_file))) {
      utils::download.file(footprint_link, temp_file, method = "auto")
    }
    footprints <- sf::st_read(utils::unzip(temp_file))
    unlink(temp_file)
    return(footprints)
  }

  footprints1 <- get_footprints_by_region(region1)

  if (!is.null(region2)) {
    if (region2 == "UnitedStates") {
      warning("For efficiency, please use individual US states as inputs.")
      return(footprints1)
    }
    if (region2 == "Canada") {
      warning("For efficiency, please use Canadian provinces as inputs.")
      return(footprints1)
    }
    footprints2 <- get_footprints_by_region(region2)

    tryCatch(
      {
        combined_footprints <- rbind(footprints1, footprints2)
        message(paste(
          "Combined building footprints from",
          region1, "and", region2
        ))
        return(combined_footprints)
      },
      error = function(e) {
        warning(paste("Could not combine footprints:", e$message, "\nReturning as a list."))
        return(list(region1 = footprints1, region2 = footprints2))
      }
    )
  } else {
    return(footprints1)
  }
}

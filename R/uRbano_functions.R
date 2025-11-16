#' Create a radius around a user-specified city center
#'
#' @param city A character string of the city name.
#' @param radius A numeric value of the radius in meters.
#' @param state An optional character string of the state name.
#' @return An sf object of the city radius.
#' @examples
#' \dontrun{
#' rad <- get_city_rad("San Juan", 30000)
#' }
#' @export
get_city_rad <- function(city, radius, state) {
  # Find matching cities
  if(missing(state)) {
    cty <- subset(world.cities, world.cities$name == city)
  } else {
    cty <- subset(us.cities, ((sub(" .*", "", us.cities[,1])) == city) & (us.cities$country.etc == state))
  }
  
  # Handle case where no cities are found
  if(nrow(cty) == 0) {
    stop("No cities found matching the criteria")
  }
  
  # Handle case where multiple cities are found
  if(nrow(cty) > 1) {
    cat("Multiple cities found:\n")
    for(i in 1:nrow(cty)) {
      if(missing(state)) {
        cat(sprintf("%d: %s, %s (Pop: %s)\n", i, cty$name[i], cty$country.etc[i], 
                    format(cty$pop[i], big.mark = ",")))
      } else {
        cat(sprintf("%d: %s, %s (Pop: %s)\n", i, cty$name[i], cty$country.etc[i], 
                    format(cty$pop[i], big.mark = ",")))
      }
    }
    
    # Get user selection, they give the number in the list of the city they choose
    selection <- as.numeric(readline(prompt = "Please select a city by number: "))
    
    # Validate selection
    if(is.na(selection) || selection < 1 || selection > nrow(cty)) {
      stop("Invalid selection")
    }
    
    # Use selected city
    cty <- cty[selection, ]
  }
  
  # Convert to sf object and create buffer
  cty_coords <- st_as_sf(cty, coords = c("long", "lat"), crs = 4326)
  cty_crc <- st_buffer(cty_coords, radius)
  
  return(cty_crc)
}

#' Extract OpenStreetMap roads within a given city radius
#'
#' @param city_radius An sf object representing the city radius.
#' @return An sf object of the roads.
#' @examples
#' \dontrun{
#' rad <- get_city_rad("San Juan", 30000)
#' rds <- extract_osm_rds(rad)
#' }
#' @export
extract_osm_rds<-function(city_radius){
  cty_ex<-extent(city_radius)
  
  #get roads from open street map API 
  rds <- opq(bbox = c (cty_ex@xmin[[1]],cty_ex@ymin[[1]],cty_ex@xmax[[1]],cty_ex@ymax[[1]] )) %>%
    add_osm_feature(key = "highway", value=c("primary" , "motorway_link",  "unclassified" ,  "tertiary"  ,  "residential",   
                                             "motorway" , "secondary" ,  "service" ,  "trunk" ,        
                                             "living_street" , "trunk_link" , "primary_link",  "secondary_link" ,         
                                             "tertiary_link", "road" )) %>% osmdata_sf()
  wrds<-rds$osm_lines
  return(wrds)
}

#' Get building footprints for a given region
#'
#' @param region1 A character string of the first region.
#' @param region2 An optional character string of the second region.
#' @return An sf object of the building footprints.
#' @examples
#' \dontrun{
#' blds <- get_bldgs("PuertoRico")
#' }
#' @export
get_bldgs<-function(region1, region2=NULL){
  if (region1 == "UnitedStates") {
    stop("You are going to want to use individual US states as inputs, trust me on this one :)")
  }
  if (region1 == "Canada") {
    stop("Use Canadian provinces as inputs, it will be easier on all of us")
  }
  USstates<-gsub("\\b([a-z])", "\\U\\1", c(unique(sub(":.*", "", maps::state.fips$polyname)),"alaska", "hawaii"), perl = TRUE)
  Ca<-c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon")
  all_regions<-read.table("https://minedbuildings.z5.web.core.windows.net/global-buildings/dataset-links.csv", header = TRUE, sep = ",")
  #use helper function get_blds_by_regions to run multiple times in the event there are two regions
  get_blds_by_regions<-function(region){
    if(region %in% USstates){
      region<-gsub(" ", "", region)
      ft_link<-paste0("https://minedbuildings.z5.web.core.windows.net/legacy/usbuildings-v2/", region, ".geojson.zip")
      message(paste("getting building footprints from",ft_link))
      infile1 <- tempfile()
      try(download.file(ft_link,infile1,method="curl"))
      if (is.na(file.size(infile1))) download.file(ft_link,infile1,method="auto")
      foots<-st_read(unzip(infile1))
      unlink(infile1)
    }else if(region %in% Ca){
      region<-gsub(" ", "", region)
      ft_link<-paste0("https://minedbuildings.z5.web.core.windows.net/legacy/canadian-buildings-v2/", region, ".zip")
      message(paste("getting building footprints from",ft_link))
      infile1 <- tempfile()
      try(download.file(ft_link,infile1,method="curl"))
      if (is.na(file.size(infile1))) download.file(ft_link,infile1,method="auto")
      foots<-st_read(unzip(infile1))
      unlink(infile1)
    }else if(region %in% all_regions$Location){
      fts<-all_regions%>%filter(Location==region)
      foot_urls <- list()
      # Loop through the URLs and read each CSV
      for (url in fts$Url) {
        foot_urls[[url]] <- read_csv2(url, col_names = FALSE)
      }
      combined_foots <- do.call(rbind, foot_urls)
      cnty_fts<-geojson_sfc(combined_foots$X1)
      foots<-st_as_sf(cnty_fts, crs=4326)
    }else{warning("cannot find buildings for that region")}
    #ft_link<-paste0("https://minedbuildings.z5.web.core.windows.net/legacy/usbuildings-v2/", state, ".geojson.zip")
    
    return(foots)
    
  }

  # Download first region
  # try catch to return separate results if rbind fails
  
  foots1 <- get_blds_by_regions(region1)
  
  # If second region is specified, download and bind
  if (!is.null(region2)) {
    foots2 <- get_blds_by_regions(region2)
    
    # Bind the two datasets together
    tryCatch({
      foots_combined <- rbind(foots1, foots2)
      message(paste("Combined building footprints from", region1, "and", region2))
      
      return(foots_combined)
    },
    error=function(e){
      # rbind failed, return the inputs separately
      warning(paste("rbind failed:", e$message, "\nReturning as list"))
      return(list(region1 = foots1, region2 = foots2))
    }
    
    )
    
  } else {
    return(foots1)
  }
}

#' Trim building footprints to a given radius and fix invalid geometries
#'
#' @param blds An sf object of building footprints.
#' @param rad An sf object of the city radius.
#' @return An sf object of the trimmed and fixed building footprints.
#' @examples
#' \dontrun{
#' rad <- get_city_rad("San Juan", 30000)
#' blds <- get_bldgs("PuertoRico")
#' blds <- fix_n_trim_bldgs(blds, rad)
#' }
#' @export
fix_n_trim_bldgs<-function(blds, rad){
  
  cty_ex<-extent(rad)
  bbox <- st_bbox(c(xmin = cty_ex@xmin[[1]], ymin = cty_ex@ymin[[1]], xmax = cty_ex@xmax[[1]], ymax = cty_ex@ymax[[1]]), crs = 4326) # Use your CRS
  bbox_sf <- st_as_sfc(bbox)
  sf_use_s2(FALSE) #turn off spherical geometry
  blds<-st_filter(blds, bbox_sf)
  gc()
  
  invalid_geometries <- blds[!st_is_valid(blds), ] 
  if(nrow(invalid_geometries)>0){
    x_blds<-as.numeric(rownames(invalid_geometries))
    blds<-blds[-c(x_blds),]
  }else{return(blds)}
  sf_use_s2(TRUE)
}

#' Transform sf data to meters
#'
#' @param sf_data An sf object.
#' @param method A character string of the transformation method ("utm", "NAD", or "web_mercator").
#' @return An sf object transformed to meters.
#' @examples
#' \dontrun{
#' rad <- get_city_rad("San Juan", 30000)
#' rad <- transform_to_meters(rad, method = "NAD")
#' }
#' @export
transform_to_meters <- function(sf_data, method = "utm") {
  if (method == "utm") {
    get_utm_crs <- function(sf_data) {
      # Get centroid of data
      centroid <- st_centroid(st_union(sf_data))
      coords <- st_coordinates(centroid)
      lon <- coords[1]
      
      # Calculate UTM zone
      utm_zone <- floor((lon + 180) / 6) + 1
      
      # Determine hemisphere
      lat <- coords[2]
      hemisphere <- ifelse(lat >= 0, "32", "327")  # 32xxx for N, 327xx for S
      
      # Create EPSG code
      epsg_code <- as.numeric(paste0(hemisphere, sprintf("%02d", utm_zone)))
      
      return(epsg_code)
    }
    
    utm_epsg <- get_utm_crs(sf_data)
    message("Using UTM Zone EPSG:", utm_epsg)
    return(st_transform(sf_data, utm_epsg))
  } else if (method == "NAD") {
    transform_NAD_utm<-function(rad){
      longitude <- st_coordinates(st_centroid(rad))[, 1]
      if(longitude>=(-126) && longitude<(-120)){print("z10")}
      else if(longitude>=(-120) && longitude<(-114)){return(st_transform(rad,crs=26911))}
      else if(longitude>=(-114) && longitude<(-108)){return(st_transform(rad,crs=26912))}
      else if(longitude>=(-108) && longitude<(-102)){return(st_transform(rad,crs=26913))}
      else if(longitude>=(-102) && longitude<(-96)){return(st_transform(rad,crs=26914))}
      else if(longitude>=(-96) && longitude<(-90)){return(st_transform(rad,crs=26915))}
      else if(longitude>=(-90) && longitude<(-84)){return(st_transform(rad,crs=26916))}
      else if(longitude>=(-84) && longitude<(-78)){return(st_transform(rad,crs=26917))}
      else if(longitude>=(-78) && longitude<(-72)){return(st_transform(rad,crs=26918))}
      else if(longitude>=(-72) && longitude<(-66)){return(st_transform(rad,crs=26919))}
      else if(longitude>=(-180) && longitude<(-174)){return(st_transform(rad,crs=26901))}
      else if(longitude>=(-174) && longitude<(-168)){return(st_transform(rad,crs=26902))}
      else if(longitude>=(-168) && longitude<(-162)){return(st_transform(rad,crs=26903))}
      else if(longitude>=(-162) && longitude<(-156)){return(st_transform(rad,crs=26904))}
      else if(longitude>=(-156) && longitude<(-150)){return(st_transform(rad,crs=26905))}
      else if(longitude>=(-150) && longitude<(-144)){return(st_transform(rad,crs=26906))}
      else if(longitude>=(-144) && longitude<(-138)){return(st_transform(rad,crs=26907))}
      else if(longitude>=(-138) && longitude<(-132)){return(st_transform(rad,crs=26908))}
      else if(longitude>=(-132) && longitude<(-126)){return(st_transform(rad,crs=26909))}
      else{warning("Coordinates outside North America UTM Zones")}
    }
    
    return(transform_NAD_utm(sf_data))
  } else if (method == "web_mercator") {
    return(st_transform(sf_data, 3857))
  }
}

#' Create a hexagonal grid within a given radius
#'
#' @param radius An sf object of the city radius.
#' @param hex_size A numeric value of the hexagon size in meters.
#' @return An sf object of the hexagonal grid.
#' @examples
#' \dontrun{
#' rad <- get_city_rad("San Juan", 30000)
#' Rgrid <- radius_hex_grid(rad, 1000)
#' }
#' @export
radius_hex_grid<-function(radius, hex_size){
  if(st_crs(radius)$epsg==4326){
    gr <- st_make_grid(radius, square=FALSE, cellsize = (hex_size/111000)) %>% st_sf()
    warning("converting hex size to units of lat/lon, size is approximate conversion to meters")
  }else{gr <- st_make_grid(radius, square=FALSE, cellsize = hex_size) %>% st_sf()}
  #get predicate for where hexes intersect so they don't get cut off at the border
  ig = lengths(st_intersects(gr, radius)) > 0
  gr<-gr%>%mutate(pred=ig)
  # Filter the hex grid to keep only intersecting hexagons
  gr <- gr%>%filter(pred==TRUE)
  return(gr)
}

#' Calculate road length by grid cell
#'
#' @param grid An sf object of the hexagonal grid.
#' @param rds An sf object of the roads.
#' @return A numeric vector of the road lengths by grid cell.
#' @examples
#' \dontrun{
#' rad <- get_city_rad("San Juan", 30000)
#' rds <- extract_osm_rds(rad)
#' Rgrid <- radius_hex_grid(rad, 1000)
#' rd_lens <- calculate_rds_by_grid(Rgrid, rds)
#' }
#' @export
calculate_rds_by_grid <- function(grid, rds) {
  # Ensure parameters are sf objects
  if (!inherits(grid, "sf") || !inherits(rds, "sf")) {
    stop("Both grid and roads must be sf objects")
  }
  
  #ensure parameters have same CRS
  if ((crs(grid)) != (crs(rds))) {
    stop("Both grid and roads must be transformed to same UTM zone, use transform_US_utm()")
  }
  
  grid<-grid%>%mutate(ID=rownames(grid))
  
  
  rd_clips <-st_intersection(grid, rds)
  
  rd_clips <- rd_clips%>%mutate(rd_len=st_length(rd_clips))
  sumlen<-rd_clips%>%group_by(ID)%>%summarise(tlen=sum(rd_len))
  
  rd_jn<-left_join(grid, st_drop_geometry(sumlen), by="ID")
  
  # Return as a numeric vector
  return(drop_units(rd_jn$tlen))
}

#' Calculate building area by grid cell
#'
#' @param grid An sf object of the hexagonal grid.
#' @param blds An sf object of the building footprints.
#' @return A numeric vector of the building areas by grid cell.
#' @examples
#' \dontrun{
#' rad <- get_city_rad("San Juan", 30000)
#' blds <- get_bldgs("PuertoRico")
#' blds <- fix_n_trim_bldgs(blds, rad)
#' Rgrid <- radius_hex_grid(rad, 1000)
#' bld_ars <- calculate_blds_by_grid(Rgrid, blds)
#' }
#' @export
calculate_blds_by_grid<-function(grid, blds){
  if (!inherits(grid, "sf") || !inherits(blds, "sf")) {
    stop("Both grid and roads must be sf objects")
  }
  
  #ensure parameters have same CRS
  if ((crs(grid)) != (crs(blds))) {
    stop("Both grid and roads must be transformed to same UTM zone, use transform_US_utm()")
  }
  
  
  grid<-grid%>%mutate(ID=rownames(grid))
  
  
  bld_clips <-st_intersection(grid, blds)
  
  bld_clips <- bld_clips%>%mutate(area=st_area(bld_clips))
  sumars<-bld_clips%>%group_by(ID)%>%summarise(tarea=sum(area))
  
  bld_jn<-left_join(grid, st_drop_geometry(sumars), by="ID")
  # Calculate lengths and sum by grid square
  #bld_clips$area <- st_area(bld_clips)
  #total_ar_by_cell <- aggregate(bld_clips$area, by = list(bld_clips$ID), FUN = st_area)$x
  
  
  # Return as a numeric vector
  return(drop_units(bld_jn$tarea))
}

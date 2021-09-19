##### Geographic Data Loading
# State/County Boundaries Data
library(tidyverse)
library(sf)
load("R/state_bound.RData")
load("R/county.RData")
# Available Cities in API
load("R/available_loc2.RData")

available_coord2 = cbind(name = available_loc2$name,
                         lat = map_chr(available_loc2$coord$lat,1),
                         lon = map_chr(available_loc2$coord$lon,1)) %>%
  as.data.frame()
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
available_coord2 =  st_as_sf(x = available_coord2,
                             coords = c("lon", "lat"),
                             crs = projcrs) %>%
  st_transform(crs = st_crs(state_bound))



##### Get all cities in a state
get_locations = function(state){
  state_bound %>%
    filter(name == state) %>%
    st_contains(available_coord2, 
                sparse = FALSE)%>% 
    filter(.data = available_coord2, .)
}


##### Get all counties in a state
get_counties = function(state){
  county %>%
    filter(state_name == state)
}


##### Given state name and city name, find location data
get_location_one = function(state, city){
  state_all = get_locations(state)
  city_index = which(state_all$name == city)
  return(list(state,
              city,
              state_all$geometry[[city_index]]))
}














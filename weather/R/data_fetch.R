source("R/geo_data.R")


### Some helping functions
clean_hourly_data = function(data) {
  weather0 = data$weather
  data %>%
    mutate(weather_simp = map_chr(map(weather0, 'main'),1),
           weather_det = map_chr(map(weather0, 'description'),1),
           time_char = as.POSIXct(dt,
                                  origin = "1970-01-01",
                                  tz = "UTC")) %>%
    select(-weather)
}




############ For Map #############
##### Fetch current and forecasting for ONE location
##### (for map analysis)
get_map_data = function(location, apiKey){
  Sys.sleep(1) # space out
  lat = location[2]
  lon = location[1]
  
  # Fetch current and forecasting data
  base_url = "https://api.openweathermap.org/data/2.5/onecall?exclude=alerts"
  key = str_c("&appid=", apiKey)
  location = str_c("&lat=", lat, "&lon=", lon)
  target_url = str_c(base_url, key, location)
  source_data = try(fromJSON(target_url))
  forecast_hourly = clean_hourly_data(source_data$hourly) %>%
    mutate(lat = source_data$lat,
           lon = source_data$lon)
  
  return(forecast_hourly)
}


### Fetch all available data (coordinates) in a state
##### (for map analysis)
get_state_data = function(state, apiKey){
  city_loc = get_locations(state)
  state_data = lapply(city_loc$geometry, 
                      function(x) get_map_data(location=x, apiKey))
  state_data = map_dfr(state_data, ~ as.data.frame(.))
  
  geo = city_loc$geometry
  city_loc$lon = map_dbl(geo, 1) %>% round(4)
  city_loc$lat = map_dbl(geo, 2) %>% round(4)
  state_data_full = left_join(state_data, city_loc,
                              by = c("lon", "lat"))
  return(state_data_full)
}


### Manage Data to sf for Visualization
##### (for map analysis)
plt_data = function(state, 
                    apiKey = "b351a493243bfe5ad524b81507325daf"){
  state_data_full = get_state_data(state, apiKey)
  county_bd = get_counties(state)
  
  projcrs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  state_sf_full = state_data_full %>%
    select(-geometry)
  state_sf_full =  st_as_sf(x = state_sf_full,
                            coords = c("lon", "lat"),
                            crs = projcrs) %>%
    st_transform(crs = st_crs(county_bd))
  
  contain_logic = county_bd %>% 
    st_contains(state_sf_full, sparse = FALSE)
  county_ind = sapply(1:nrow(state_sf_full), 
                      function(x) (1:nrow(county_bd))[contain_logic[,x]])
  state_sf_full$county = sapply(county_ind,
                                function(x) county_bd$name[x])
  return(list(state_sf_full, county_bd))
}















##### Fetch PAST & Future data for ONE location
##### (for trend analysis)
get_loc_data = function(location,
                        apiKey = "b351a493243bfe5ad524b81507325daf"){
  lat = location[2]
  lon = location[1]
  
  # Fetch current and forecasting data
  base_url = "https://api.openweathermap.org/data/2.5/onecall?exclude=alerts"
  key = str_c("&appid=", apiKey)
  location = str_c("&lat=", lat, "&lon=", lon)
  target_url = str_c(base_url, key, location)
  source_data = try(fromJSON(target_url))
  forecast_hourly = clean_hourly_data(source_data$hourly)
  
  # Fetch historical data
  his_url = "https://api.openweathermap.org/data/2.5/onecall/timemachine?"
  get_his_urls = function(his_date){
    date = str_c("&dt=", his_date)
    target_url = str_c(his_url, key, location, date)
  }
  current = source_data$current
  curr_time = as.POSIXlt(current$dt, 
                         origin = "1970-01-01",
                         tz = "UTC")
  his_dates = sapply(5:0, function(d) curr_time - 3600*24*d)
  his_urls = sapply(his_dates, get_his_urls)
  his_data = lapply(his_urls, function(url) try(fromJSON(url)))
  history_hourly = map_dfr(his_data, "hourly") %>%
    clean_hourly_data()
  
  # Merge and Clean
    vars = intersect(names(forecast_hourly), 
                     names(history_hourly))
    forecast_hourly = forecast_hourly %>%
      select(all_of(vars))
    history_hourly = history_hourly %>%
      select(all_of(vars))
    loc_data = rbind(forecast_hourly,
                     history_hourly) %>%
      distinct(dt, .keep_all = T) %>%
      arrange(dt) %>%
      mutate(lat = source_data$lat,
             lon = source_data$lon)

  return(loc_data)
}


##### Give at most 3 "get_location_one" result, find and merge all data
##### (for trend analysis)
get_3loc_data = function(loc1, loc2, loc3,
                         apiKey = "b351a493243bfe5ad524b81507325daf"){
  # Fetch all data
  data1 = get_loc_data(loc1[[3]], apiKey)
  data1$state = loc1[[1]]
  data1$city = loc1[[2]]
  if (loc2!=""){
    data2 = get_loc_data(loc2[[3]], apiKey)
    data2$state = loc2[[1]]
    data2$city = loc2[[2]]
  } else {data2 = data1[0,]}
  if (loc3!=""){
    data3 = get_loc_data(loc3[[3]], apiKey)
    data3$state = loc3[[1]]
    data3$city = loc3[[2]]
  } else {data3 = data1[0,]}
  
  # Merge and Clean
  data1 = as.matrix(data1) #avoid rownames error
  data2 = as.matrix(data2)
  data3 = as.matrix(data3)
  vars = intersect(intersect(colnames(data1),
                             colnames(data2)),
                   colnames(data3))
  data1 = data1 %>% as.data.frame() %>% select(all_of(vars))
  data2 = data2 %>% as.data.frame() %>% select(all_of(vars))
  data3 = data3 %>% as.data.frame() %>% select(all_of(vars))
  full_data = rbind(data1, data2, data3) %>%
    mutate(name = str_c(state, city, sep = "--")) %>%
    mutate_at("time_char", as.POSIXct) %>%
    mutate_at(c("dt","temp","humidity",
                "lon","lat"), as.numeric)
  
  
  return(full_data)
}


source("R/data_fetch.R")


##### Map plot
map_plot = function(data, time, state){
  
  state_sf_full = data[[1]]
  county_bd = data[[2]]
  time = unique(state_sf_full$time_char)[time+1]
  
  # Background-filling data
  time_data = state_sf_full %>%
    filter(time_char == time)  %>%
    mutate(lat = map_dbl(geometry, 2),
           lon = map_dbl(geometry, 1))
  piece_data = time_data %>%
    mutate(temp = temp - 273.15) %>%
    group_by(county) %>%
    summarise(temperature = mean(temp),
              humidity = mean(humidity),
              clouds = mean(clouds),
              visibility = mean(visibility),
              wind_speed = mean(wind_speed)) %>%
    as.data.frame() %>%
    select(-geometry) %>%
    rename(name = county)
  fill_data = left_join(county_bd, piece_data,
                        by = "name")
  
  # Weather-points data
  add_points = function(base_fill){
    cols = c("Clear" = "Gold", 
             "Clouds" = "LightBlue",
             "Drizzle" = "LightSkyBlue",
             "Rain" = "DeepSkyBlue",
             "Snow" = "AliceBlue",
             "Thunderstorm" = "DarkSlateGrey", 
             "Squall" = "LightSlateGrey", 
             "Tornado" = "LightSlateGrey", 
             "Mist" = "plum",
             "Smoke" = "plum",
             "Haze" = "plum",
             "Dust" = "plum",
             "Ash" = "plum",
             "Fog" = "plum")
    base_fill  +
      geom_point(data=time_data,
                 aes(x=lon, y= lat, 
                     color = weather_simp),
                 size = 2) +
      scale_color_manual(values = cols) +
      guides(color = guide_legend(title = "weather",
                                  order = 1))
  }
  
  add_theme = function(base_fill){
    base_fill +
      theme_bw(base_size = 16) +
      theme(legend.position = "bottom",
            legend.margin = unit(0.1,"inches"),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 8),
            legend.key.height = unit(8, "pt"),
            legend.key.width = unit(50, "pt"),
            title = element_text(size = 14),
            legend.box = "vertical")
  }
  
  
  # Background-filling plots
  temp_fill = ggplot(fill_data) +
    geom_sf(aes(fill = temperature)) +
    scale_fill_gradient(low = "FloralWhite", high = "Tomato",
                        na.value = "transparent",
                        breaks=seq(-20,60,by=0.5)) +
    labs(x = NULL, y = NULL,
         title= str_c("Temperature in ", state)) +
    theme_bw(base_size = 16)
  temp_fill = temp_fill  %>% 
      add_points() %>% add_theme()
  
  humid_fill = ggplot(fill_data) +
    geom_sf(aes(fill = humidity)) +
    scale_fill_gradient(low = "Azure", high = "Cyan",
                        na.value = "transparent",
                        breaks=seq(0,100,by=5)) +
    labs(x = NULL, y = NULL,
         title= str_c("Humidity in ", state))
  humid_fill = humid_fill %>% 
    add_points() %>% add_theme()
  
  vis_fill = ggplot(fill_data) +
    geom_sf(aes(fill = visibility)) +
    scale_fill_gradient(low = "HoneyDew", high = "LimeGreen",
                        na.value = "transparent",
                        breaks=seq(0,10000,by=500)) +
    labs(x = NULL, y = NULL,
         title= str_c("Visibility in ", state))
  vis_fill = vis_fill %>% 
    add_points() %>% add_theme()
  
  cloud_fill = ggplot(fill_data) +
    geom_sf(aes(fill = clouds)) +
    scale_fill_gradient(low = "Linen", high = "Tan",
                        na.value = "transparent",
                        breaks=seq(0,100,by=5)) +
    labs(x = NULL, y = NULL,
         title= str_c("Clouds in ", state))
  cloud_fill = cloud_fill %>%
    add_points() %>% add_theme()
  
  
  
  # Final plots
  plts = list(temp = temp_fill,
              humid = humid_fill,
              vis = vis_fill,
              cloud = cloud_fill)
  return(plts)
}
















##### Temperature time series
temp_ts_plot = function(full_data){
  times = full_data$time_char %>%
    unique() %>%
    as.POSIXct()
  current  = times[length(times)-47]
  
  full_data %>%
    mutate(temp = temp - 273.15) %>% 
    ggplot(aes(x = as.POSIXct(time_char), y = temp, 
               color = name)) +
    geom_rect(aes(xmin = current , xmax = max(times), 
                  ymin = -Inf, ymax = Inf),
              alpha = 0.02, fill = c("grey")) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = 
                 as.POSIXct(full_data$time_char)
               [which(hour(full_data$time_char)==0)],
               linetype ="dotted", size = 0.5) +
    scale_color_brewer(palette="Reds") +
    scale_x_datetime(breaks = "12 hour",
                     date_labels ="%m-%d %H:%M") +
    labs(x = "UTC Time", y = "Temperature",
         title="Temperature in 5-days historical and 2-day forecast") +
    theme_bw(base_size = 14) +
    annotate("text", x = max(times)-3600*24, 
             y = range(full_data$temp)[1]-273.15, 
             label = "Forecast",
             hjust = 0.5, vjust = -1, size = 4) +
    theme(axis.text.x=element_text(angle=40, hjust=1,
                                   size = 7),
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.key.size = unit(9, "pt"),
          panel.grid=element_blank(),
          title = element_text(size = 12))
}



##### Humidity time series
humid_ts_plot = function(full_data){
  times = full_data$time_char %>%
    unique() %>%
    as.POSIXct()
  current  = times[length(times)-47]
  
  full_data %>%
    ggplot(aes(x = as.POSIXct(time_char), y = humidity, 
               color = name)) +
    geom_rect(aes(xmin = current , xmax = max(times), 
                  ymin = -Inf, ymax = Inf),
              alpha = 0.02, fill = c("grey")) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = 
                 as.POSIXct(full_data$time_char)
               [which(hour(full_data$time_char)==0)],
               linetype ="dotted", size = 0.5) +
    scale_color_brewer(palette="Blues") +
    scale_x_datetime(breaks = "12 hour",
                     date_labels ="%m-%d %H:%M") +
    labs(x = "UTC Time", y = "Humidity",
         title="Humidity in 5-days historical and 2-day forecast") +
    theme_bw(base_size = 14) +
    annotate("text", x = max(times)-3600*24, 
             y = range(full_data$temp)[1]-273.15, 
             label = "Forecast",
             hjust = 0.5, vjust = -1, size = 4) +
    theme(axis.text.x=element_text(angle=40, hjust=1,
                                   size = 7),
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.key.size = unit(9, "pt"),
          panel.grid=element_blank(),
          title = element_text(size = 12))
}


##### Temperature Circle Heatmap
temp_circ_plot = function(full_data){
  circle_data = full_data %>%
    mutate(hour = hour(as.POSIXct(time_char))) %>%
    group_by(name,hour) %>%
    summarise(avg_temp = mean(temp)-273.15)
  circle_data %>%
    ggplot(aes(x = name, y = 1,
               fill = avg_temp)) +
    geom_bar(stat="identity", position="stack")+
    scale_fill_distiller(palette="RdBu", direction = -1) +
    coord_polar(theta = 'y') +
    labs(x = NULL, y = NULL,
         title="Historical 5-day hourly average temperature
         \n (UTC time)") +
    scale_y_continuous(breaks = 0:23) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 7.5),
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.key.width = unit(40, "pt"),
          legend.key.height = unit(8, "pt"),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          title = element_text(size = 11))
}


##### Weather Grid map
weath_grid_plot = function(full_data){
  times = full_data$time_char %>%
    unique() %>%
    as.POSIXct()
  current  = times[length(times)-47]
  cols = c("Clear" = "Gold", 
           "Clouds" = "LightBlue",
           "Drizzle" = "LightSkyBlue",
           "Rain" = "DeepSkyBlue",
           "Snow" = "AliceBlue",
           "Thunderstorm" = "DarkSlateGrey", 
           "Squall" = "LightSlateGrey", 
           "Tornado" = "LightSlateGrey", 
           "Mist" = "plum",
           "Smoke" = "plum",
           "Haze" = "plum",
           "Dust" = "plum",
           "Ash" = "plum",
           "Fog" = "plum")
  
  full_data %>%
    ggplot(aes(y = time_char,
               x = name)) +
    geom_rect(aes(ymin = current , ymax = max(times), 
                  xmin = -Inf, xmax = Inf),
              alpha = 0.02, fill = c("grey")) +
    geom_raster(aes(fill = weather_simp)) +
    scale_fill_manual(values = cols) +
    scale_y_datetime(breaks = "12 hour",
                     date_labels ="%m-%d %H:%M") +
    labs(x = NULL, y = "UTC Time",
         title="5-days historical and 2-day forecast weather") +
    geom_hline(yintercept = 
                 as.POSIXct(full_data$time_char)
               [which(hour(full_data$time_char)==0)],
               linetype ="dotdash", size = 0.5) +
    theme_bw(base_size = 14) +
    theme(axis.text.y=element_text(angle=40, size = 7),
          axis.text.x = element_text(size = 8),
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.key.size = unit(9, "pt"),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          title = element_text(size = 11))
}

##### Total Visualization 2
compare_plot = function(full_data){
  list(weath_grid = weath_grid_plot(full_data),
       temp_ts = temp_ts_plot(full_data),
       temp_circ = temp_circ_plot(full_data),
       humid_ts = humid_ts_plot(full_data))
}






  
##### SARIMA for longer forecast
### Given a location name, plot SARIMA forecast
sarima_one_loc = function(full_data, locname){
  
  # Fit SARIMA
  ts = full_data %>%
    mutate(temp = temp - 273.15) %>% 
    filter(name == locname) %>%
    arrange(time_char) %>%
    select(temp) %>%
    as.ts()
  fit_sarima = Arima(ts, order = c(3,1,1), 
                     seasonal = list(order=c(1,1,0),
                                     period=24))
  
  # Clean Data for Visualization
  xx = forecast(fit_sarima, h=24)
  times = full_data$time_char %>%
    unique() %>%
    as.POSIXct()
  new_times = seq(times[length(times)]+ 3600, 
                  times[length(times)] + 3600*24,
                  by = "hours")
  ts_forc_data = data.frame(forecast = c(ts, xx$mean),
                            lower80 = c(rep(NA, length(times)), 
                                        xx$lower[,1]),
                            lower95 = c(rep(NA, length(times)), 
                                        xx$lower[,2]),
                            upper80 = c(rep(NA, length(times)), 
                                        xx$upper[,1]),
                            upper95 = c(rep(NA, length(times)), 
                                        xx$upper[,2]),
                            time = c(times, new_times))
  # Plot
  ts_forc_data %>%
    ggplot(aes(x = time, y = forecast)) + 
    geom_line(size = 1) + 
    geom_ribbon(aes(ymin=lower80, ymax=upper80, x=time), 
                fill = "red", alpha = 0.3) +
    geom_ribbon(aes(ymin=lower95, ymax=upper95, x=time), 
                fill = "blue", alpha = 0.1) +
    scale_x_datetime(breaks = "12 hour",
                     date_labels ="%m-%d %H:%M") +
    geom_vline(xintercept = 
                 as.POSIXct(ts_forc_data$time)
               [which(hour(ts_forc_data$time)==0)],
               linetype ="dotted", size = 0.5) +
    labs(x = "UTC Time", y = "Temperature",
         title=paste("Longer Temperature Forecast in 48~72-Hour: \n",
                     locname)) +
    theme(axis.text.x=element_text(angle=40, hjust=1,
                                   size = 7),
          title = element_text(size = 12))
  
}


## Automatically plot all locations' SARIMA in full-data
sarima_plot = function(full_data){
  names = unique(full_data$name)
  plt_list = lapply(names, function(x) sarima_one_loc(full_data, x))
  return(plt_list)
}




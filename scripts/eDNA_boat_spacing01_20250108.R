library(tidyverse)
library(geosphere)
library(lubridate)
library(zoo)

# Import and compile GPS track data ---------------------------------------

## Stryker ----
stryker_startstop <- data.frame(boat = rep("stryker",6), 
                        site = c(rep("812", 3), rep("809", 3)),
                        start = c(7, 83, 145, 168, 238, 297), 
                        end = c(79, 141, 164, 234, 293, 334))

stryker = data.frame()
stryker_raw = data.frame()

for(xs in 1:nrow(stryker_startstop)){
  print(xs)
  
  temp <- read.csv("C:/Users/eholmes/OneDrive - California Department of Water Resources/Documents/Projects/eDNA/data/Sample20250108/RouteHistoryFullBkup_DWRBoat_Jan8.csv", 
                   skip = stryker_startstop[xs, "start"] - 1, nrow = stryker_startstop[xs, "end"] - stryker_startstop[xs, "start"], header = F)
  
  colnames(temp) = c("Junk", "lat", "lon", "datetime", "speed")
  
  temp$timestamp <- as.POSIXct(temp$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "America/Los_Angeles")

  complete_timestamps <- seq(min(temp$timestamp), max(temp$timestamp), by = "1 sec")
  
  merged_data <- data.frame(timestamp = complete_timestamps) %>%
    left_join(temp, by = "timestamp")
  
  interpolated_data <- merged_data %>%
    mutate(lat = na.approx(lat, rule = 2),
           lon = na.approx(lon, rule = 2))
  
  interpolated_data$xs <- paste0(stryker_startstop[xs, "site"], "-xs", xs)
  temp$xs <- paste0(stryker_startstop[xs, "site"], "-xs", xs)
  
  interpolated_data$site <- stryker_startstop[xs, "site"]
  temp$site = stryker_startstop[xs, "site"]
  
  stryker <- rbind(stryker, interpolated_data[, c("site", "xs", "timestamp", "lat", "lon")])
  stryker_raw <- rbind(stryker_raw, temp)
  
  rm(temp, interpolated_data, merged_data)
}

stryker$boat = "Stryker"
colnames(stryker) <- c("site", "xs", "timestamp", "lat_st", "lon_st", "boat")

## Phantom ----
phantom_startstop <- data.frame(boat = rep("phantom", 6), 
                        site = c(rep("812", 3), rep("809", 3)),
                        start = c(7, 177, 292, 371, 486, 612), 
                        end = c(173, 288, 367, 482, 608, 697))

phantom = data.frame()
phantom_raw = data.frame()

for(xs in 1:nrow(phantom_startstop)){
  print(xs)
  
  temp <- read.csv("C:/Users/eholmes/OneDrive - California Department of Water Resources/Documents/Projects/eDNA/data/Sample20250108/20250108_RouteHistory_CFS.csv", 
                   skip = phantom_startstop[xs, "start"] - 1, nrow = phantom_startstop[xs, "end"] - phantom_startstop[xs, "start"], header = F)
  
  colnames(temp) = c("Junk", "lat", "lon", "datetime", "speed")
  
  temp$timestamp <- as.POSIXct(temp$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "America/Los_Angeles")

  complete_timestamps <- seq(min(temp$timestamp), max(temp$timestamp), by = "1 sec")
  
  merged_data <- data.frame(timestamp = complete_timestamps) %>%
    left_join(temp, by = "timestamp")
  
  interpolated_data <- merged_data %>%
    mutate(lat = na.approx(lat, rule = 2),
      lon = na.approx(lon, rule = 2))
  
  interpolated_data$xs <- paste0(phantom_startstop[xs, "site"], "-xs", xs)
  temp$xs <- paste0(phantom_startstop[xs, "site"], "-xs", xs)
  
  interpolated_data$site <- phantom_startstop[xs, "site"]
  temp$site = phantom_startstop[xs, "site"]
  
  phantom <- rbind(phantom, interpolated_data[, c("site", "xs", "timestamp", "lat", "lon")])
  phantom_raw <- rbind(phantom_raw, temp)
  
  rm(temp, interpolated_data, merged_data)
}

phantom$boat = "Phantom"
colnames(phantom) <- c("site", "xs", "timestamp", "lat_ph", "lon_ph", "boat")

## Scrutiny ---- now 
scrutiny_startstop <- data.frame(boat = rep("scrutiny", 1), 
                                site = c(rep("809", 1)),
                                start = c(7), 
                                end = c(34))

scrutiny = data.frame()
scrutiny_raw = data.frame()

for(xs in 1:nrow(scrutiny_startstop)){
  print(xs)
  
  temp <- read.csv("C:/Users/eholmes/OneDrive - California Department of Water Resources/Documents/Projects/eDNA/data/Sample20250108/RouteHistoryFullBkup_CDFW Jan8.csv", 
                   skip = scrutiny_startstop[xs, "start"] - 1, nrow = scrutiny_startstop[xs, "end"] - scrutiny_startstop[xs, "start"], header = F)
  
  colnames(temp) = c("Junk", "lat", "lon", "datetime", "speed")
  
  temp$timestamp <- as.POSIXct(temp$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "America/Los_Angeles")
  
  complete_timestamps <- seq(min(temp$timestamp), max(temp$timestamp), by = "1 sec")
  
  merged_data <- data.frame(timestamp = complete_timestamps) %>%
    left_join(temp, by = "timestamp")
  
  interpolated_data <- merged_data %>%
    mutate(lat = na.approx(lat, rule = 2),
           lon = na.approx(lon, rule = 2))
  
  interpolated_data$xs <- paste0(scrutiny_startstop[xs, "site"], "-xs", xs)
  temp$xs <- paste0(scrutiny_startstop[xs, "site"], "-xs", xs)
  
  interpolated_data$site <- scrutiny_startstop[xs, "site"]
  temp$site = scrutiny_startstop[xs, "site"]
  
  scrutiny <- rbind(scrutiny, interpolated_data[, c("site", "xs", "timestamp", "lat", "lon")])
  scrutiny_raw <- rbind(scrutiny_raw, temp)
  
  rm(temp, interpolated_data, merged_data)
}

scrutiny$boat = "Scrutiny"
colnames(scrutiny) <- c("site", "xs", "timestamp", "lat_na", "lon_na", "boat")

## Merge vessel tracks ----

vessels <- merge(merge(stryker, 
                       phantom[, c("timestamp", "lat_ph", "lon_ph")], by = "timestamp", all.x = T),
                 scrutiny[, c("timestamp", "lat_na", "lon_na")], by = "timestamp", all.x = T) 

xs_lookup <- data.frame(xs = c("809-xs6", "809-xs5", "809-xs4", "812-xs3", "812-xs2", "812-xs1"),
                              xs_cor = c("809-xs1", "809-xs2", "809-xs3", "812-xs1", "812-xs2", "812-xs3"))

vessels <- merge(vessels, xs_lookup, by = "xs", all.x = T)

# Visualize tracks --------------------------------------------------------

ggplot(stryker, aes(x = lon_st, y = lat_st, color = xs)) + 
  geom_path() + theme_bw() + 
  geom_point(data = stryker_raw, aes(x = lon, y = lat), size = .1) +
  facet_wrap(site ~ ., scales = "free")

ggplot(phantom, aes(x = lon_ph, y = lat_ph, color = xs)) + 
  geom_path() + theme_bw() + 
  geom_point(data = phantom_raw, aes(x = lon, y = lat), size = .1) +
  facet_wrap(site ~ ., scales = "free")

ggplot(scrutiny, aes(x = lon_na, y = lat_na, color = xs)) + 
  geom_path() + theme_bw() + 
  geom_point(data = scrutiny_raw, aes(x = lon, y = lat), color = "black", size = .1) +
  facet_wrap(site ~ ., scales = "free")

# Calculate vessel spacing ------------------------------------------------

vessels$stryker2phantom <- NA
vessels$scrutiny2phantom <- NA
vessels$stryker_velocity <- NA
vessels$phantom_velocity <- NA
vessels$scrutiny_velocity <- NA

for(i in 1:nrow(vessels)){
  print(i)
 
  # if(complete.cases(vessels[,c("lon_st", "lat_st", "lon_ph","lat_ph")])[i]){
    try(vessels[i,"stryker2phantom"] <- distm(c(vessels[i,"lon_st"], vessels[i,"lat_st"]), 
                                                  c(vessels[i,"lon_ph"], vessels[i,"lat_ph"]), 
                                                  fun = distHaversine))
  # }
  
  # if(complete.cases(vessels[,c("lon_na", "lat_na", "lon_ph","lat_ph")])[i]){
    try(vessels[i,"scrutiny2phantom"] <- distm(c(vessels[i,"lon_na"], vessels[i,"lat_na"]), 
                                          c(vessels[i,"lon_ph"], vessels[i,"lat_ph"]), 
                                          fun = distHaversine))
  # }
  
  # if(complete.cases(vessels[,c("lon_na", "lat_na", "lon_st","lat_st")])[i]){
    try(vessels[i,"stryker2scrutiny"] <- distm(c(vessels[i,"lon_na"], vessels[i,"lat_na"]), 
                                           c(vessels[i,"lon_st"], vessels[i,"lat_st"]), 
                                           fun = distHaversine))
  # }
  
  try(vessels[i,"stryker_velocity"] <- distm(c(vessels[i,"lon_st"], vessels[i,"lat_st"]), 
                                         c(vessels[i + 1,"lon_st"], vessels[i + 1,"lat_st"]), 
                                         fun = distHaversine) * 2.23693629)
  
  try(vessels[i,"phantom_velocity"] <- distm(c(vessels[i,"lon_ph"], vessels[i,"lat_ph"]), 
                                             c(vessels[i + 1,"lon_ph"], vessels[i + 1,"lat_ph"]), 
                                             fun = distHaversine) * 2.23693629)
  
  try(vessels[i,"scrutiny_velocity"] <- distm(c(vessels[i,"lon_na"], vessels[i,"lat_na"]), 
                                             c(vessels[i + 1,"lon_na"], vessels[i + 1,"lat_na"]), 
                                             fun = distHaversine) * 2.23693629)
      
  
}


# Plot vessel tracks, velocity, and spacing -------------------------------

(v_spacing_809 <- ggplot(vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3"),], 
       aes(x = timestamp, y = stryker2phantom)) + 
    geom_line(color = "cyan4", linewidth = 1) +
    geom_line(aes(y = scrutiny2phantom), linewidth = 1) +
  # geom_line(aes(y = stryker2scrutiny), color = "blue",linewidth = 1) + 
    theme_bw() + labs(y = "Distance (m)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0,250) +
  facet_grid(. ~ xs_cor, scales = "free"))

(v_spacing_812 <- ggplot(vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3"),], 
                         aes(x = timestamp, y = stryker2phantom)) + 
    geom_line(color = "cyan4", linewidth = 1) +
    geom_line(aes(y = scrutiny2phantom), linewidth = 1) + 
    theme_bw() + labs(y = "Distance (m)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0,250) +
    facet_grid(. ~ xs_cor, scales = "free"))

(v_velocity_809 <- ggplot(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3") &
                               vessels$stryker_velocity < 10 & is.na(vessels$stryker_velocity) == F,],
                          aes(x = timestamp, y = stryker_velocity)) + 
    geom_line(color = "cyan4", linewidth = 1, alpha = .25) +
    geom_line(color = "cyan4", linewidth = 1, stat = "smooth", span = .25) +
    geom_line(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3") &
                               vessels$phantom_velocity < 10 & is.na(vessels$phantom_velocity) == F, ],
              aes(y = phantom_velocity), color = "salmon3", linewidth = 1, alpha = .2) +
    geom_line(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3") &
                               vessels$phantom_velocity < 10 & is.na(vessels$phantom_velocity) == F, ],
              aes(y = phantom_velocity), color = "salmon3", linewidth = 1, stat = "smooth", span = .25) +
    geom_line(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3") &
                               vessels$scrutiny_velocity < 10 & is.na(vessels$scrutiny_velocity) == F, ],
              aes(y = scrutiny_velocity), linewidth = 1, alpha = .25) +
    theme_bw() + labs(y = "Velocity (mph)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0, 6) +
    facet_grid(. ~ xs_cor, scales = "free"))

(v_velocity_812 <- ggplot(data = vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3") &
                                           vessels$stryker_velocity < 10 & is.na(vessels$stryker_velocity) == F,], 
                         aes(x = timestamp, y = stryker_velocity)) + 
    geom_line(color = "cyan4", linewidth = 1, alpha = .25) +
    geom_line(color = "cyan4", linewidth = 1, stat = "smooth", span = .25) +
    geom_line(data = vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3") &
                               vessels$phantom_velocity < 10 & is.na(vessels$phantom_velocity) == F, ],
              aes(y = phantom_velocity), color = "salmon3", linewidth = 1, alpha = .25) +
    geom_line(data = vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3") &
                               vessels$phantom_velocity < 10 & is.na(vessels$phantom_velocity) == F, ],
              aes(y = phantom_velocity), color = "salmon3", linewidth = 1, stat = "smooth", span = .25) +
    geom_line(data = vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3") &
                        vessels$scrutiny_velocity < 10 & is.na(vessels$scrutiny_velocity) == F, ],
              aes(y = scrutiny_velocity), linewidth = 1) +
    theme_bw() + labs(y = "Velocity (mph)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0, 6) +
    facet_grid(. ~ xs_cor, scales = "free"))


(v_tracks_809 <- ggplot(vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3"),], 
                     aes(x = lon_st, y = lat_st)) + 
    geom_path(color = "cyan4", linewidth = 2) +
    geom_path(aes(x = lon_ph, y = lat_ph), color = "salmon3", alpha = .5, linewidth = 2) + 
    geom_path(aes(x = lon_na, y = lat_na), linewidth = 2) + 
    theme_bw() + labs(y = "Latitude", x = "Longitude") + coord_fixed() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    facet_grid(. ~ xs_cor))

(v_tracks_812 <- ggplot(vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3"),], 
                        aes(x = lon_st, y = lat_st)) + 
    geom_path(color = "cyan4", linewidth = 2) +
    geom_path(aes(x = lon_ph, y = lat_ph), color = "salmon3", alpha = .5, linewidth = 2) + 
    geom_path(aes(x = lon_na, y = lat_na), linewidth = 2) + 
    theme_bw() + labs(y = "Latitude", x = "Longitude") +  coord_fixed() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    facet_grid(. ~ xs_cor))


png("C:/Users/eholmes/OneDrive - California Department of Water Resources/Documents/Projects/eDNA/output/eDNA_vessel_spacing_20250108_%02d.png",
    height = 7, width = 10, units = "in", res = 1000, family = "serif")

cowplot::plot_grid(v_tracks_809, v_tracks_812,
                   v_spacing_809, v_spacing_812, 
                   align = "v")

dev.off()

png("C:/Users/eholmes/OneDrive - California Department of Water Resources/Documents/Projects/eDNA/output/eDNA_vessel_velocity_20250108_%02d.png",
    height = 10, width = 10, units = "in", res = 1000, family = "serif")

cowplot::plot_grid(v_tracks_809, v_tracks_812,
                   v_velocity_809, v_velocity_812, 
                   v_spacing_809, v_spacing_812, 
                   align = "v", nrow = 3)

dev.off()


# Use geographic coordinates ----------------------------------------------

bbox <- c(left = -123, bottom = 34, right = -73, top = 42)
osm_map <- get_map(location = bbox, source = "osm", maptype = "terrain-background")
(v_spacing_809_ggmap <- ggmap() + 
   geom_line(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3"),], 
             aes(x = timestamp, y = stryker2phantom),color = "cyan4", linewidth = 1) +
   geom_line(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3"),],
             aes(x = timestamp, y = scrutiny2phantom), linewidth = 1) +
   # geom_line(aes(y = stryker2scrutiny), color = "blue",linewidth = 1) + 
   theme_bw() + labs(y = "Distance (m)") + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         axis.text.y = element_text(angle = 90, hjust = .5)) +
   ylim(0,250) + coord_fixed() +
   facet_grid(. ~ xs_cor, scales = "free"))



(v_spacing_812_ggmap <- ggplot(vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3"),], 
                         aes(x = timestamp, y = stryker2phantom)) + 
    geom_line(color = "cyan4", linewidth = 1) +
    geom_line(aes(y = scrutiny2phantom), linewidth = 1) + 
    theme_bw() + labs(y = "Distance (m)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0,250) +
    facet_grid(. ~ xs_cor, scales = "free"))

library(tmap)
library(tmaptools)
library(sf)
library(OpenStreetMap)
# Definetmap# Define Bounding Box
bbox <- c(left = -123, bottom = 34, right = -73, top = 42)

# Fetch Basemap
basemap <- read_osm(bbox, type = "bing", zoom=6)  # Adjust 'zoom' level as needed; requires a Bing API key

# Plotting with tmap
tm_shape(basemap) +
  tm_rgb() +
  tm_shape(coordinates) +
  tm_dots(col = "red", size = 0.5) +
  tm_text("city", just = "top", size = 0.8, col = "blue") +
  tm_layout(title = "GPS Coordinates with Satellite Imagery",
            legend.outside = TRUE

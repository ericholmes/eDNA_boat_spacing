library(tidyverse)
library(geosphere)
library(lubridate)
library(zoo)

# Import and compile GPS track data ---------------------------------------

## Stryker ----
stryker_startstop <- data.frame(boat = rep("stryker",6), 
                        site = c(rep("812", 3), rep("809", 3)),
                        start = c(7, 43, 94, 131, 201, 297), 
                        end = c(39, 90, 127, 197, 293, 372))

stryker = data.frame()
stryker_raw = data.frame()

for(xs in 1:nrow(stryker_startstop)){
  print(xs)
  
  temp <- read.csv("data/Survey00_20241119/RouteHistoryFullBkup_DWRBoat.csv", 
                   skip = stryker_startstop[xs, "start"] - 1, nrow = stryker_startstop[xs, "end"] - stryker_startstop[xs, "start"], header = F)
  
  colnames(temp) = c("Junk", "lat", "lon", "datetime", "speed")
  
  temp$timestamp <- as.POSIXct(temp$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "America/Los_Angeles")
  
  # Convert the timestamp to a POSIXct object
  temp$timestamp <- ymd_hms(temp$timestamp, tz = "UTC")
  
  # Convert it to Pacific Time (PT)
  temp$timestamp <- with_tz(temp$timestamp, tzone = "America/Los_Angeles")
  
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
                        start = c(7, 78, 157, 216, 335, 489), 
                        end = c(74, 153, 212, 331, 485, 607))

phantom = data.frame()
phantom_raw = data.frame()

for(xs in 1:nrow(phantom_startstop)){
  print(xs)
  
  temp <- read.csv("data/Survey00_20241119/20241119_RouteHistory_Cramer.csv", 
                   skip = phantom_startstop[xs, "start"] - 1, nrow = phantom_startstop[xs, "end"] - phantom_startstop[xs, "start"], header = F)
  
  colnames(temp) = c("Junk", "lat", "lon", "datetime", "speed")
  
  temp$timestamp <- as.POSIXct(temp$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "America/Los_Angeles")
  
  # Convert the timestamp to a POSIXct object
  temp$timestamp <- ymd_hms(temp$timestamp, tz = "UTC")
  
  # Convert it to Pacific Time (PT)
  temp$timestamp <- with_tz(temp$timestamp, tzone = "America/Los_Angeles")
  
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

## New Alosa ----
new_alosa_startstop <- data.frame(boat = rep("new_alosa", 6), 
                                site = c(rep("812", 3), rep("809", 3)),
                                start = c(140, 68, 7, 314,449, 493), 
                                end = c(163, 95, 24, 348, 488, 531))

new_alosa = data.frame()
new_alosa_raw = data.frame()

for(xs in 1:nrow(new_alosa_startstop)){
  print(xs)
  
  temp <- read.csv("data/Survey00_20241119/RouteHistoryFullBkup_CDFW_fixed.csv", 
                   skip = new_alosa_startstop[xs, "start"] - 1, nrow = new_alosa_startstop[xs, "end"] - new_alosa_startstop[xs, "start"], header = F)
  
  colnames(temp) = c("Junk", "lat", "lon", "datetime", "speed")
  
  temp$timestamp <- as.POSIXct(temp$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "America/Los_Angeles")
  
  # Convert the timestamp to a POSIXct object
  temp$timestamp <- ymd_hms(temp$timestamp, tz = "UTC")
  
  # Convert it to Pacific Time (PT)
  temp$timestamp <- with_tz(temp$timestamp, tzone = "America/Los_Angeles")
  
  complete_timestamps <- seq(min(temp$timestamp), max(temp$timestamp), by = "1 sec")
  
  merged_data <- data.frame(timestamp = complete_timestamps) %>%
    left_join(temp, by = "timestamp")
  
  interpolated_data <- merged_data %>%
    mutate(lat = na.approx(lat, rule = 2),
           lon = na.approx(lon, rule = 2))
  
  interpolated_data$xs <- paste0(new_alosa_startstop[xs, "site"], "-xs", xs)
  temp$xs <- paste0(new_alosa_startstop[xs, "site"], "-xs", xs)
  
  interpolated_data$site <- new_alosa_startstop[xs, "site"]
  temp$site = new_alosa_startstop[xs, "site"]
  
  new_alosa <- rbind(new_alosa, interpolated_data[, c("site", "xs", "timestamp", "lat", "lon")])
  new_alosa_raw <- rbind(new_alosa_raw, temp)
  
  rm(temp, interpolated_data, merged_data)
}

new_alosa$boat = "New Alosa"
colnames(new_alosa) <- c("site", "xs", "timestamp", "lat_na", "lon_na", "boat")

## Merge vessel tracks ----

vessels <- merge(merge(stryker, 
                       phantom[, c("timestamp", "lat_ph", "lon_ph")], by = "timestamp", all.x = T),
                 new_alosa[, c("timestamp", "lat_na", "lon_na")], by = "timestamp", all.x = T) 

xs_lookup <- data.frame(xs = c("809-xs7", "809-xs6", "809-xs5", "809-xs4", "812-xs3", "812-xs2", "812-xs1"),
                              xs_cor = c("809-xstest", "809-xs1", "809-xs2", "809-xs3", "812-xs1", "812-xs2", "812-xs3"))

vessels <- merge(vessels, xs_lookup, by = "xs", all.x = T)

# Visualize tracks --------------------------------------------------------

ggplot(stryker, aes(x = lon_st, y = lat_st, color = xs)) + 
  geom_path() + theme_bw() + 
  geom_point(data = stryker_raw, aes(x = lon, y = lat), size = .1) +
  facet_wrap(site ~ ., scales = "free")

ggplot(phantom, aes(x = lon_ph, y = lat_ph, color = xs)) + 
  geom_path() + theme_bw() + 
  geom_point(data = phantom_raw, aes(x = lon, y = lat), color = "black", size = .1) +
  facet_wrap(site ~ ., scales = "free")

ggplot(new_alosa, aes(x = lon_na, y = lat_na, color = xs)) + 
  geom_path() + theme_bw() + 
  geom_point(data = new_alosa_raw, aes(x = lon, y = lat), color = "black", size = .1) +
  facet_wrap(site ~ ., scales = "free")

# Calculate vessel spacing ------------------------------------------------

vessels$stryker2phantom <- NA
vessels$newalosa2phantom <- NA
vessels$stryker_velocity <- NA
vessels$phantom_velocity <- NA
vessels$new_alosa_velocity <- NA

for(i in 1:nrow(vessels)){
  print(i)
 
  if(complete.cases(vessels[,c("lon_st", "lat_st", "lon_ph","lat_ph")])[i]){
    vessels[i,"stryker2phantom"] <- distm(c(vessels[i,"lon_st"], vessels[i,"lat_st"]), 
                                                  c(vessels[i,"lon_ph"], vessels[i,"lat_ph"]), 
                                                  fun = distHaversine)
  }
  
  if(complete.cases(vessels[,c("lon_na", "lat_na", "lon_ph","lat_ph")])[i]){
    vessels[i,"newalosa2phantom"] <- distm(c(vessels[i,"lon_na"], vessels[i,"lat_na"]), 
                                          c(vessels[i,"lon_ph"], vessels[i,"lat_ph"]), 
                                          fun = distHaversine)
  }
  
  if(complete.cases(vessels[,c("lon_na", "lat_na", "lon_st","lat_st")])[i]){
    vessels[i,"stryker2newalosa"] <- distm(c(vessels[i,"lon_na"], vessels[i,"lat_na"]), 
                                           c(vessels[i,"lon_st"], vessels[i,"lat_st"]), 
                                           fun = distHaversine)
  }
  
  try(vessels[i,"stryker_velocity"] <- distm(c(vessels[i,"lon_st"], vessels[i,"lat_st"]), 
                                         c(vessels[i + 1,"lon_st"], vessels[i + 1,"lat_st"]), 
                                         fun = distHaversine) * 2.23693629)
  
  try(vessels[i,"phantom_velocity"] <- distm(c(vessels[i,"lon_ph"], vessels[i,"lat_ph"]), 
                                             c(vessels[i + 1,"lon_ph"], vessels[i + 1,"lat_ph"]), 
                                             fun = distHaversine) * 2.23693629)
  
  try(vessels[i,"new_alosa_velocity"] <- distm(c(vessels[i,"lon_na"], vessels[i,"lat_na"]), 
                                             c(vessels[i + 1,"lon_na"], vessels[i + 1,"lat_na"]), 
                                             fun = distHaversine) * 2.23693629)
      
  
}


# Plot vessel tracks, velocity, and spacing -------------------------------

(v_spacing_809 <- ggplot(vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3"),], 
       aes(x = timestamp, y = stryker2phantom)) + 
    geom_line(color = "cyan4", linewidth = 1) +
    geom_line(aes(y = newalosa2phantom), linewidth = 1) +
  # geom_line(aes(y = stryker2newalosa), color = "blue",linewidth = 1) + 
    theme_bw() + labs(y = "Distance (m)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0,415) +
  facet_grid(. ~ xs_cor, scales = "free"))

(v_spacing_812 <- ggplot(vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3"),], 
                         aes(x = timestamp, y = stryker2phantom)) + 
    geom_line(color = "cyan4", linewidth = 1) +
    geom_line(aes(y = newalosa2phantom), linewidth = 1) + 
    theme_bw() + labs(y = "Distance (m)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0,415) +
    facet_grid(. ~ xs_cor, scales = "free"))

(v_velocity_809 <- ggplot(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3") &
                               vessels$stryker_velocity < 10 & is.na(vessels$stryker_velocity) == F,],
                          aes(x = timestamp, y = stryker_velocity)) + 
    geom_line(color = "cyan4", linewidth = 1) +
    geom_line(color = "cyan4", linewidth = 1, stat = "smooth", span = .25) +
    geom_line(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3") &
                               vessels$phantom_velocity < 10 & is.na(vessels$phantom_velocity) == F, ],
              aes(y = phantom_velocity), color = "salmon3", linewidth = 1) +
    geom_line(data = vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3") &
                               vessels$new_alosa_velocity < 10 & is.na(vessels$new_alosa_velocity) == F, ],
              aes(y = new_alosa_velocity), linewidth = 1) +
    theme_bw() + labs(y = "Velocity (mph)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0, 6) +
    facet_grid(. ~ xs_cor, scales = "free"))

(v_velocity_812 <- ggplot(data = vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3") &
                                           vessels$stryker_velocity < 10 & is.na(vessels$stryker_velocity) == F,], 
                         aes(x = timestamp, y = stryker_velocity)) + 
    geom_line(color = "cyan4", linewidth = 1) +
    geom_line(color = "cyan4", linewidth = 1, stat = "smooth", span = .25) +
    geom_line(data = vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3") &
                               vessels$phantom_velocity < 10 & is.na(vessels$phantom_velocity) == F, ],
              aes(y = phantom_velocity), color = "salmon3", linewidth = 1) +
    geom_line(data = vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3") &
                        vessels$new_alosa_velocity < 10 & is.na(vessels$new_alosa_velocity) == F, ],
              aes(y = new_alosa_velocity), linewidth = 1) +
    theme_bw() + labs(y = "Velocity (mph)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    ylim(0, 6) +
    facet_grid(. ~ xs_cor, scales = "free"))


(v_tracks_809 <- ggplot(vessels[vessels$xs_cor %in% c("809-xs1","809-xs2","809-xs3"),], 
                     aes(x = lon_st, y = lat_st)) + 
    geom_path(color = "cyan4", linewidth = 2) +
    geom_path(aes(x = lon_ph, y = lat_ph), color = "salmon3", alpha = .5, linewidth = 4) + 
    geom_path(aes(x = lon_na, y = lat_na), linewidth = 2) + 
    theme_bw() + labs(y = "Latitude", x = "Longitude") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    facet_grid(. ~ xs_cor, scales = "free"))

(v_tracks_812 <- ggplot(vessels[vessels$xs_cor %in% c("812-xs1","812-xs2","812-xs3"),], 
                        aes(x = lon_st, y = lat_st)) + 
    geom_path(color = "cyan4", linewidth = 2) +
    geom_path(aes(x = lon_ph, y = lat_ph), color = "salmon3", alpha = .5, linewidth = 4) + 
    geom_path(aes(x = lon_na, y = lat_na), linewidth = 2) + 
    theme_bw() + labs(y = "Latitude", x = "Longitude") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = .5)) +
    facet_grid(. ~ xs_cor, scales = "free"))


png("output/eDNA_vessel_spacing_20241119_%02d_updated.png",
    height = 7, width = 10, units = "in", res = 1000, family = "serif")

cowplot::plot_grid(v_tracks_809, v_tracks_812,
                   v_spacing_809, v_spacing_812, 
                   align = "v")

dev.off()

png("output/eDNA_vessel_velocity_20241119_%02d_updated.png",
    height = 10, width = 10, units = "in", res = 1000, family = "serif")

cowplot::plot_grid(v_tracks_809, v_tracks_812,
                   v_velocity_809, v_velocity_812, 
                   v_spacing_809, v_spacing_812, 
                   align = "v", nrow = 3)

dev.off()


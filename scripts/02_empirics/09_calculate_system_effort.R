############################################################################
#####                   02_calculate_system_effort                  #####
############################################################################
#
# 
############################################################################

## SETUP
# Load packages
library(here)
library(cowplot)
library(raster)
library(sf)
library(tidyverse)

# Load helper functions
source(here("scripts", "00_helpers.R"))

## Load data
# System polygons
system <- 
  st_read(here("data", "mpa_systems.gpkg"),
          stringsAsFactors = F) %>% 
  filter(!id %in% c("EEZ", "50% HUD"))

sys_bbox <- st_bbox(system)

# Effort data
effort_data <- 
  readRDS(here("data", "gridded_effort_by_gear_and_year_dist_to_mpa.rds")) %>% 
  filter(!(year <= 2017 & wdpaid == 555629385))

# Night lights data
night_data <- 
  readRDS(here("data", "gridded_night_lights_by_year_dist_to_mpa.rds")) %>% 
  filter(!(year <= 2017 & wdpaid == 555629385))

# Filter to keep only data within the bounding box
effort <- 
  effort_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  mutate(year = paste("effort", year, sep = "_")) %>% 
  group_by(lon, lat, year) %>%
  summarize(fishing_days = sum(fishing_hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(year, fishing_days)

lights <- night_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  mutate(year = paste("radiance", year, sep = "_")) %>% 
  group_by(lon, lat, year) %>%
  summarize(radiance = mean(sum_rad, na.rm = T)) %>% 
  ungroup() %>% 
  spread(year, radiance)

effort <- effort %>% 
  full_join(lights, by = c("lon", "lat"))

# effort_plot <- effort %>% 
#   drop_na(fishing_days) %>% 
#   ggplot() +
#   geom_raster(aes(x = lon, y = lat, fill = fishing_days)) +
#   geom_sf(data = system, aes(color = id), fill = "transparent") +
#   scale_fill_viridis_c() +
#   scale_color_brewer(palette = "Set1") +
#   plot_theme() +
#   ggtitle("Fishing days (2016 - 2019)")
# 
# lights_plot <- effort %>% 
#   drop_na(radiance) %>% 
#   ggplot() +
#   geom_raster(aes(x = lon, y = lat, fill = radiance)) +
#   geom_sf(data = system, aes(color = id), fill = "transparent") +
#   scale_fill_viridis_c() +
#   scale_color_brewer(palette = "Set1") +
#   plot_theme() +
#   ggtitle("Radiance (2016 - 2019)")
# 
# plot_grid(effort_plot, lights_plot, ncol = 1)

effort_raster <- 
  rasterFromXYZ(effort, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

extracted_values <- raster::extract(effort_raster, system,
                                    method = "simple",
                                    fun = sum,
                                    na.rm = T,
                                    df = T) %>% 
  as_tibble() %>% 
  select(-ID)

data <- system %>% 
  st_drop_geometry() %>% 
  cbind(extracted_values) %>% 
  gather(variable, value, -c(wdpaid, iso3, id, area, fraction_as_reserve, where)) %>% 
  separate(variable, into = c("variable", "year"), sep = "_") %>% 
  spread(variable, value) %>% 
  group_by(wdpaid, year) %>% 
  mutate(tot_fishing = sum(effort, na.rm = T),
         tot_rad = sum(radiance, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(fishing_days = effort / tot_fishing,
         radiance = radiance / tot_rad) %>% 
  select(-contains("tot"))

data %>% 
  group_by(iso3, id) %>% 
  summarize(fd_sd = sd(fishing_days, na.rm = T),
            fishing_days = mean(fishing_days, na.rm = T)) %>% 
  ungroup() %>% 
  select(-fd_sd) %>% 
  spread(id, fishing_days) %>% 
  knitr::kable()

data %>% 
  select(wdpaid, id, iso3, where, year, fishing_days, radiance) %>% 
  gather(source, value, -c(wdpaid, id, iso3, where, year)) %>% 
  ggplot(aes(x = id, y = value, fill = where)) +
  stat_summary(geom = "col", fun = mean, na.rm = T, color = "black") +
  stat_summary(geom = "errorbar", fun.data = mean_sdl, na.rm = T, color = "black", width = 0.1) +
  facet_wrap(iso3~source, scales = "free_y", ncol = 2) +
  labs(x = "Polygon", y = "Total effort / radiance") +
  scale_fill_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Two measures of effort (Mean SD")























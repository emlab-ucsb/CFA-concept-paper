############################################################################
#####                   09_calculate_galapagos_effort                  #####
############################################################################
#
# 
############################################################################

## SETUP
# Load packages
library(here)
library(raster)
library(sf)
library(tidyverse)

# Load helper functions
source(here("scripts", "00_helpers.R"))

## Load data
# System polygons
gal_system <- 
  st_read(here("data", "galapagos_tuna_polygons.gpkg")) %>% 
  st_transform(4326)

sys_bbox <- st_bbox(gal_system)

# Effort data
effort_data <- 
  readRDS(here("data", "gridded_effort_by_gear_and_year_dist_to_mpa.rds"))

# Filter to keep only data within the bounding box
gal_effort <- 
  effort_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  group_by(lon, lat) %>% 
  summarize(fishing_hours = mean(fishing_hours, na.rm = T))

ggplot(gal_effort) +
  geom_raster(aes(x = lon, y = lat, fill = fishing_hours)) +
  geom_sf(data = gal_system, aes(color = id), fill = "transparent") +
  scale_fill_viridis_c(trans = "log10") +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Fishing effort (2016 - 2019)")


gal_effort_raster <- 
  rasterFromXYZ(gal_effort)
plot(gal_effort_raster)

raster::extract(gal_effort_raster, gal_system,
                method = "simple",
                fun = sum,
                na.rm = T)

gal_system %>% 
  mutate(effort = raster::extract(gal_effort_raster, .,
                                  method = "simple",
                                  fun = mean,
                                  na.rm = T)) %>% 
  mutate(where = ifelse(id == "Galapagos MPA", "MPA", "Outside")) %>% 
  ggplot(aes(x = id, y = effort, fill = where)) +
  geom_col(color = "black") +
  labs(x = "Polygon", y = "Total effort in polygon\n(fishing hours)") +
  scale_fill_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Mean effort extracted in each polygon")


























############################################################################
#####                   02_calculate_galapagos_effort                  #####
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
gal_system <- 
  st_read(here("data", "galapagos_tuna_polygons.gpkg")) %>% 
  st_transform(4326)

sys_bbox <- st_bbox(gal_system)

# Effort data
effort_data <- 
  readRDS(here("data", "gridded_effort_by_gear_and_year_dist_to_mpa.rds"))

# Night lights data
night_data <- 
  readRDS(here("data", "gridded_night_lights_by_year_dist_to_mpa.rds"))

# Filter to keep only data within the bounding box
gal_effort <- 
  effort_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  group_by(lon, lat) %>%
  summarize(fishing_days = mean(fishing_hours, na.rm = T) / 24)

gal_lights <- night_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  group_by(lon, lat) %>%
  summarize(radiance = mean(sum_rad, na.rm = T))

effort <- gal_effort %>% 
  full_join(gal_lights, by = c("lon", "lat"))

effort_plot <- effort %>% 
  drop_na(fishing_days) %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = fishing_days)) +
  geom_sf(data = gal_system, aes(color = id), fill = "transparent") +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Fishing days (2016 - 2019)")

lights_plot <- effort %>% 
  drop_na(radiance) %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = radiance)) +
  geom_sf(data = gal_system, aes(color = id), fill = "transparent") +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Radiance (2016 - 2019)")

plot_grid(effort_plot, lights_plot)

gal_effort_raster <- 
  rasterFromXYZ(effort, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

extracted_values <- raster::extract(gal_effort_raster, gal_system,
                                    method = "simple",
                                    fun = sum,
                                    na.rm = T,
                                    df = T)

data <- gal_system %>% 
  mutate(fishing_days = extracted_values$fishing_days,
         radiance = extracted_values$radiance)

effort_radiance_model <- filter(data, id %in% c("Ecuadorian EEZ", "Galapagos EEZ", "95% HUD")) %>% 
  lm(fishing_days ~ radiance, data = .) 

data %>% 
  mutate(fishing_days_model = predict(effort_radiance_model, .))%>% 
  ggplot() +
  geom_point(aes(x = radiance, y = fishing_days_model), color = "red") +
  geom_point(aes(x = radiance, y = fishing_days), color = "blue")

data %>% 
  mutate(fishing_days_model = predict(effort_radiance_model, .))%>% 
  ggplot(aes(x = fishing_days, y =fishing_days_model)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0, 200))


data %>% 
  st_drop_geometry() %>%
  mutate(fishing_days_corrected = predict(effort_radiance_model, .)) %>% 
  gather(source, value, -c(id, area, fraction_as_reserve, where)) %>% 
  # filter(!id == "Ecuadorian EEZ") %>% 
  ggplot(aes(x = id, y = value, fill = where)) +
  geom_col(color = "black") +
  facet_wrap(~source, scales = "free", ncol = 1) +
  labs(x = "Polygon", y = "Total effort radiance") +
  scale_fill_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Two measres of effort") +
  labs(subtitle = "AIS and VIIRS data show that the Galapagos MPA contains ~2% (1.6 days) of the fishing effort\ninand 30% of radiance in the system (using HUD 95%).\n\nAfter correcting with radiance, Galapagos contains 43% of effort (72 days / 166)")


























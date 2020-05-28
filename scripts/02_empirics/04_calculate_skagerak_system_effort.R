############################################################################
#####               09_calculate_skagerak_system_effort               #####
############################################################################
#
# This script calculate the effort occurring in each polygon of the 
# Skagerak system MPA.
# 
############################################################################

## SETUP
# Load packages
library(here)
library(cowplot)
library(raster)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load helper functions
source(here("scripts", "00_helpers.R"))

## Load data
# System polygons
system <- 
  st_read(here("data", "Skagerak.gpkg"),
          stringsAsFactors = F) %>% 
  filter(!id %in% c("EEZ", "EDM"))

# Effort data
effort_data <- 
  readRDS(here("data", "skagerak_trawling.rds"))

dnk <- ne_countries(country = c("Denmark", "Norway", "Sweden"),
                    returnclass = "sf",
                    scale = "large")

## Define boundaries
d <- 0.5
bbox <- st_bbox(system) + c(-d, -d, d, d)

# Filter and clean
effort <- 
  effort_data %>% 
  filter(year == 2019,
         between(lon, bbox[1], bbox[3]),
         between(lat, bbox[2], bbox[4])) %>% 
  group_by(lon, lat) %>% 
  summarize(fishing_days = sum(fishing_hours, na.rm = T) / 24)

effort_raster <- 
  rasterFromXYZ(effort, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

extracted_values <- raster::extract(effort_raster, system,
                                    method = "simple",
                                    fun = sum,
                                    na.rm = T,
                                    sp = T)

data <- extracted_values %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  mutate(id = fct_reorder(id, fishing_days, .desc = T))


## FIGURES
map <- effort %>%
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = fishing_days)) +
  geom_sf(data = system, aes(color = id), fill = "transparent") +
  geom_sf(data = dnk) +
  geom_sf(data = mpa) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  labs(title = "Trawiling days (2019)",
       subtitle = "Data are on a 0.01 degree grid") +
  coord_sf(expand = FALSE,
           xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

effort_in_poly <- ggplot(data, aes(x = id, y = fishing_days)) +
  geom_col(fill = "steelblue", color = "black") +
  labs("Total trawling days in each polygon",
       x = "Polygon",
       y = "Trawling days") +
  plot_theme()

percent_per_system <- data %>% 
  select(id, fishing_days) %>% 
  spread(id, fishing_days) %>% 
  gather(id, fishing_days, -c(MPA)) %>% 
  mutate(percent = (MPA / (fishing_days + MPA)),
         id = fct_reorder(id, percent, .desc = T)) %>% 
  ggplot(aes(x = id, y = percent)) +
  geom_col(fill = "steelblue", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs("Percent of fishing inside MPA for each deffinition of 'System'",
       x = "Deffinition of system",
       y = "Percent activity inside MPA") +
  plot_theme()


plot_grid(map, plot_grid(effort_in_poly, percent_per_system, ncol = 1), ncol = 2)
















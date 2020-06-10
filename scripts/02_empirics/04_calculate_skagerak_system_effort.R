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
d <- 0.1
bbox <- st_bbox(system) + c(-d, -d, d, d)

# Filter and clean
effort <- 
  effort_data %>% 
  filter(#year == 2019#,
         between(lon, bbox[1], bbox[3]),
         between(lat, bbox[2], bbox[4])
         ) %>%
  group_by(lon, lat, year) %>%
  summarize(fishing_days = sum(fishing_hours, na.rm = T) / 24) %>% 
  spread(year, fishing_days, fill = NA)

effort_raster <- 
  rasterFromXYZ(effort, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

plot(effort_raster)

extracted_values <- raster::extract(effort_raster, system,
                                    method = "simple",
                                    fun = sum,
                                    na.rm = T,
                                    sp = T)

data <- extracted_values %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  select(-area) %>% 
  gather(year, fishing_days, -id) %>%  
  mutate(id = fct_reorder(id, fishing_days, .desc = T))


## FIGURES
map <- effort %>%
  gather(year, fishing_days, -c(lon, lat)) %>% 
  group_by(lon, lat) %>% 
  summarize(fishing_days = mean(fishing_days, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = fishing_days)) +
  geom_sf(data = dnk) +
  geom_sf(data = system, aes(color = id), fill = "transparent") +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1", direction = -1) +
  plot_theme() +
  labs(title = "Mean trawiling effort (2012 - 2019)",
       subtitle = "Data are on a 0.01 degree grid") +
  coord_sf(expand = FALSE,
           xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  guides(fill = guide_colorbar(title = "Days",
                               frame.colour = "black",
                               ticks.colour = "black"),
         color = guide_legend(title = "Polygon"))

effort_in_poly <- ggplot(data, aes(x = id, y = fishing_days)) +
  stat_summary(geom = "col", fun = mean, fill = "steelblue", color = "black") +
  stat_summary(geom = "errorbar", fun.data = mean_sdl, width = 0.1) +
  labs(title = "Mean trawling days in each polygon (2012 - 2019)",
       x = "Polygon",
       y = "Trawling days") +
  plot_theme()

plot <- plot_grid(map, effort_in_poly, ncol = 2)

data %>% 
  group_by(id) %>% 
  summarise(fishing_days = mean(fishing_days, na.rm = T)) %>% 
  ungroup()

lazy_ggsave(plot = plot, filename = "skagerak_system_effort")


cpue <- data %>% 
  group_by(year) %>% 
  summarize(effort = sum(fishing_days)) %>% 
  mutate(landings = c(landings, NA))

lm(landings ~ effort, data = cpue) %>% 
  summary()

ggplot(cpue, aes(x = effort, y = landings)) +
  geom_smooth(method = "lm") +
  geom_point()









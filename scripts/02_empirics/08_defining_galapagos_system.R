############################################################################
#####                   08_defining_galapagos_system                    ####
############################################################################
#
# We need to provide a real-world example of how CFA's could work. The
# first challenge is to define what the system is, so that we can 
# correctly say how big the MPA is relative to "the system".
# 
# There are some deffinitions of "the system", which we'll try to use:
# - Ecuadorian EEZ
# - 95% habitat utlization density (areas probably used)
# - 50% habitat utiliation density (probable core areas)
# 
# We'll need the following shapefiles:
# - EEZs
# - MPAs
# 
# The HUD for tuna are reported in km2. We'll therefore create a buffer with
# a radius of r = sqrt(HUD / pi), to obtain a cyrcle with area HUD. The 
# buffer will be created from the Galapagos MPA centroid.
# 
############################################################################

## SETUP
# Load packages
library(here)
library(sf)
library(tidyverse)

# Load helper functions
source(here("scripts", "00_helpers.R"))

## Load data

# Galapagos MPA
gal <- st_read(here("data", "no_take_lsmpa_boundaries.gpkg")) %>% 
  filter(wdpaid == 11753) %>%                                               # Keep only Galapagos MPA
  mutate(id = "Galapagos MPA") %>%                                          # Rename for identfication
  select(id)                                                                # Keep only id column

# Centroid of Galapagos MPA
gal_centroid <- st_centroid(gal)                                            # Calculate centroid

gal_area <- st_area(gal)                                                    # Area of Galapagos MPA (to be used later)

# Ecuadorian EEZ
ecu_eez <- st_read(here("data", "clean_world_eez_v11.gpkg")) %>% 
  filter(mrgid %in% c(8431, 8403)) %>%                                      # Keep only the Ecuadorian EEZ
  st_transform("ESRI:54009") %>%                                            # Reproject to mollewiede
  mutate(id = "Ecuadorian EEZ") %>%                                         # Rename for identification
  group_by(id) %>%                                                          # Union geometries
  summarize(geom = st_combine(geom)) %>%                                    
  ungroup() %>%
  st_difference(gal) %>%                                                    # Remove the galapagos polygon from it
  select(id)                                                                # Keep only id column

## Movement data
# Load data on tuna habitat utilization
tuna_movement <- read.csv(here("data", "tuna_habitat_utilization.csv"),
                          stringsAsFactors = F) %>% 
  filter(name == "Yellowfin")                                               # !!!!!!Keep yellowfin for now !!!!!!

# Define targets
hr_95 <- mean(tuna_movement$util_95, na.rm = T)                             # mean 95% utilization distribution (km2)
hr_50 <- mean(tuna_movement$util_50, na.rm = T)                             # mean 50% utilization distribution (km2)

# Define corresponding radius
hr_95_rad <- sqrt(hr_95 / pi) * 1e3                                         # Radius of 95% utilization (m)
hr_50_rad <- sqrt(hr_50 / pi) * 1e3                                         # Radius of 50% utilizatio (m)

# Create "system" circle around the centroid
HUD_95 <- gal_centroid %>% 
  st_buffer(dist = hr_95_rad) %>%                                           # For 95% (probable areas)
  st_difference(gal) %>%                                                    # Remove the galapagos polygon from it
  mutate(id = "95% HUD") %>%                                                # Reame for identification
  select(id)                                                                # Keep id column only

HUD_50 <- gal_centroid %>% 
  st_buffer(dist = hr_50_rad) %>%                                           # For 50% (core areas)
  mutate(id = "50% HUD") %>%                                                # Reame for identification
  select(id)                                                                # Keep id column only

# Combine all polygons into one object
all_polygons <- rbind(ecu_eez,
                      gal,
                      HUD_95,
                      HUD_50) %>% 
  mutate(area = st_area(.),                                                 # Calculate the area of each polygon
         id = fct_relevel(id, c("Ecuadorian EEZ",                            # Reorder polygons for display
                                "95% HUD",
                                "50% HUD",
                                "Galapagos MPA")),
         fraction_as_reserve = gal_area / (area + gal_area),                # Calculate fraction of system that is resrve
         fraction_as_reserve = ifelse(area < gal_area,                      # If polygon area is less than mpa area, then ignore this as a system
                                      NA, 
                                      fraction_as_reserve),
         fraction_as_reserve = ifelse(area == gal_area,                     # If polygon area is same as MPA, than this is the MPA, not the system
                                      NA,
                                      fraction_as_reserve))
all_polygons %>% 
  st_drop_geometry() %>% 
  knitr::kable(format = "markdown")

# Plot the final polygons
ggplot(data = all_polygons) +
  geom_sf(aes(color = id), fill = "transparent") +
  theme_bw() +
  scale_color_brewer(name = "Polygon",
                     palette = "Set1",
                     direction = -1)


# Save polygons
polygons_fn <- here("data", "galapagos_tuna_polygons.gpkg")                 # Create file name
file.remove(polygons_fn)                                                    # Remove from disk to avoid appending
st_write(obj = all_polygons,                                                # Save file to disk
         dsn = polygons_fn)









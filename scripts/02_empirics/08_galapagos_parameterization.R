

# Calculate parameters for model run

# What is r?

# How much effor tis there inside vs outside?

# A few definitions of system size:

# All equatorian EEZ
# 200 NMS

library(here)
library(ggsflabel)
library(sf)
library(tidyverse)

# Load helper functions
source(here("scripts", "00_helpers.R"))

ecu <- st_read(here("data", "clean_world_eez_v11.gpkg")) %>% 
  filter(mrgid %in% c(8431, 8403)) %>% 
  mutate(id = "Ecuadorian EEZ") %>% 
  select(id) %>% 
  st_transform("ESRI:54009") %>% 
  group_by(id) %>% 
  summarize(geom = st_combine(geom))
  

ecu_gal <- st_read(here("data", "clean_world_eez_v11.gpkg")) %>% 
  filter(mrgid %in% 8403) %>% 
  mutate(id = "Galapagos EEZ") %>% 
  select(id) %>% 
  st_transform("ESRI:54009")

galapagos <- st_read(here("data", "no_take_lsmpa_boundaries.gpkg")) %>% 
  filter(wdpaid == 11753) %>% 
  mutate(id = "Galapagos MR") %>% 
  select(id)


# Home range
# "For the 12 fish that demonstrated site fidelity, the mean 95 and 50%
# utilization distributions were 258,730 km2 and 41,260 km2, respectively"
# From Schaefer et al., 2007: doi.org/10.1007/s00227-007-0689-x

# Create polygon with this area around galapagos centroid.
# Define targets
hr_95 <- 258730                         # mean 95% utilization distribution (km2)
hr_50 <- 41260                          # mean 50% utilization distribution (km2)

hr_95 <- mean(c(258730,
                642179))

hr_50 <- mean(c(21260,
                66511))

# Define corresponding radius
hr_95_rad <- sqrt(hr_95 / pi) * 1e3     # Radius of 95% utilization (m)
hr_50_rad <- sqrt(hr_50 / pi) * 1e3     # Radius of 50% utilizatio (m)

galapagos_centroid <- st_centroid(galapagos) # Calculate centroid

utilization_95 <- galapagos_centroid %>% 
  st_buffer(dist = hr_95_rad) %>% 
  mutate(id = "95% Utilization")

utilization_50 <- galapagos_centroid %>% 
  st_buffer(dist = hr_50_rad) %>% 
  mutate(id = "50% Utilization")

all_polygons <- rbind(ecu,
                      ecu_gal,
                      galapagos,
                      utilization_95,
                      utilization_50) %>% 
  mutate(area = st_area(.),
         id = fct_reorder(id, -area),
         percent_as_reserve = (st_area(galapagos) / area) * 100,
         geometry = geom)

all_polygons %>% 
  st_drop_geometry() %>% 
  select(-geometry) %>% 
  knitr::kable(format = "markdown")

# Plot the final polygons
ggplot(data = all_polygons) +
  geom_sf(aes(fill = id)) +
  geom_sf(data = ecu, fill = "transparent") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1")












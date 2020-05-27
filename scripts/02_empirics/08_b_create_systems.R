
library(here)
library(sf)
library(tidyverse)

source(here("scripts", "02_empirics", "get_system.R"))


## Load data
# MPA
mpa <- st_read(here::here("data", "WDPA_Nov2019_No_Terrestrial/"),
                layer = "WDPA_Nov2019_No_Terrestrial",
                stringsAsFactors = FALSE,
                as_tibble = TRUE) %>% 
  janitor::clean_names() %>% 
  filter(wdpaid  == 555522470) %>% 
  st_transform(54009) %>% 
  mutate(id = "MPA",
         area = st_area(.)) %>% 
  select(id, area)

# EEZ
eez <- st_read(here("data", "clean_world_eez_v11.gpkg"),
               stringsAsFactors = F) %>% 
  filter(iso3 == "DNK") %>% 
  rename(geometry = geom) %>% 
  mutate(id = "EEZ",
         area = st_area(.)) %>% 
  select(id, area)

## Movement data
# Cod movement (km converted to m) from Table 2 in Hobson et al.: Movements of North Sea cod
cod_edm <- mean(c(204, 317, 76, 162, 101, 213, 105, 239, 220, 229)) * 1e3
cod_res <- mean(c(6, 36, 17, 1, 83, 41, 6, 5, 56, 9, 20, 42, 60, 7)) * 1e3
cod_elm <- mean(c(21.9, 55.9, 14.8, 20.7, 18.6, 15.9, 11.7, 28.7,6.9, 59.2)) * 1e3
cod_mean <- 46.12 * 1e3

cod_movement <- read.csv(here("data", "cod_movement_data.csv")) %>% 
  mutate(movement = ((n_oc_edm * duration_edm * dist_edm) +
           (n_oc_res * duration_res * dist_res) +
           (n_oc_elm * duration_elm * dist_elm)) / time_at_liberty)

#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
# MPA characteristics
mpa_centroid <- st_centroid(mpa)
mpa_area <- st_area(mpa)

# Create "system" circle around the centroid
EDM_poly <- mpa_centroid %>% 
  st_buffer(dist = cod_edm) %>%                                          # For Extended Direct Movement
  mutate(id = "EDM") %>%                                                 # Reame for identification                                             # Keep id column only
  mutate(area = st_area(.)) %>%                                          # Calculate area
  # st_difference(mpa) %>%                                                 # Remove the MPA from it (to avoid double counting effort)
  select(id, area)

RES_poly <- mpa_centroid %>% 
  st_buffer(dist = cod_res) %>%                                          # For Residence
  mutate(id = "RES") %>%                                                 # Reame for identification                                             # Keep id column only
  mutate(area = st_area(.)) %>%                                          # Calculate area
  # st_difference(mpa) %>%                                                 # Remove the MPA from it (to avoid double counting effort)
  select(id, area)

ELM_poly <- mpa_centroid %>% 
  st_buffer(dist = cod_elm) %>%                                          # For Extended Localized Movement
  mutate(id = "ELM") %>%                                                 # Reame for identification                                             # Keep id column only
  mutate(area = st_area(.)) %>%                                          # Calculate area
  # st_difference(mpa) %>%                                               # Remove the MPA from it (to avoid double counting effort)
  select(id, area)

MEAN_poly <- mpa_centroid %>% 
  st_buffer(dist = cod_mean) %>%                                          # For Extended Localized Movement
  mutate(id = "Mean") %>%                                                 # Reame for identification                                             # Keep id column only
  mutate(area = st_area(.)) %>%                                          # Calculate area
  select(id, area)

# Combine all polygons into one object
all_polygons <- rbind(eez,
                      EDM_poly,
                      RES_poly,
                      ELM_poly,
                      MEAN_poly,
                      mpa)

plot(all_polygons[,1])

all_polygons <- all_polygons %>% 
  mutate(id = fct_relevel(id, c("EEZ",                                      # Reorder polygons for display
                                "EDM",
                                "RES",
                                "ELM",
                                "MPA")),
         fraction_as_reserve = units::drop_units(mpa_area / area),                # Calculate fraction of system that is resrve
         fraction_as_reserve = ifelse(area == mpa_area,                     # If polygon area is same as MPA, than this is the MPA, not the system
                                      NA,
                                      fraction_as_reserve),
         where = ifelse(id == "MPA",
                        "MPA",
                        "Outside")) %>%                                          # Define inside / outside
  st_transform(4326) %>% 
  select(id, area, fraction_as_reserve, where)

plot(all_polygons[,1], col = "transparent")

all_polygons %>% 
  st_drop_geometry() %>% 
  select(id, fraction_as_reserve) %>% 
  filter(!id == "MPA") %>% 
  spread(id, fraction_as_reserve) %>% 
  knitr::kable(digits= 2)

ggplot(all_polygons) +
  geom_sf() +
  facet_wrap(~id)

###########################
###########################
###########################
###########################
###########################
###########################
###########################
###########################
###########################
###########################
###########################
###########################
###########################
###########################
file.remove(here("data", "mpa_systems.gpkg"))
st_write(systems, here("data", "mpa_systems.gpkg"))
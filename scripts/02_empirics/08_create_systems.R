library(sf)
library(tidyverse)
library(here)

source(here("scripts", "02_empirics", "get_system.R"))


## Load data
# MPA
mpas <- st_read(here("data", "no_take_lsmpa_boundaries.gpkg"),
                stringsAsFactors = F)

# EEZ
eez <- st_read(here("data", "clean_world_eez_v11.gpkg"),
               stringsAsFactors = F) %>% 
  mutate(iso3 = ifelse(iso3 == "UMI", "USA", iso3))

range <- st_read(here("data", "yel_shapefile.gpkg")) %>% 
  st_make_valid()

## Movement data
# Load data on tuna habitat utilization
tuna_movement <- read.csv(here("data", "tuna_habitat_utilization.csv"),
                          stringsAsFactors = F) %>% 
  filter(name == "Yellowfin")                                               # !!!!!!Keep yellowfin for now !!!!!!

# Define targets
hr_95 <- mean(tuna_movement$util_95, na.rm = T)                             # mean 95% utilization distribution (km2)
hr_50 <- mean(tuna_movement$util_50, na.rm = T)                             # mean 50% utilization distribution (km2)


# Proceed to generate systems

# Define WDPAID and Countries of itnerest

wdpaid_want <- c(11753,                                                     # Galapagos MPA
                 555629385,                                                 # Revilla MPA
                 309888,                                                    # PIPA
                 400011                                                     # PRIMNM
)

iso3_want <- c("ECU",
               "MEX",
               "KIR",
               "USA")

# Combine into a tibble
systems <- tibble(wdpaid = wdpaid_want, iso3 = iso3_want) %>% 
  mutate(system = pmap(.l = list(wdpaid_want = wdpaid, iso3_want = iso3),
                       .f = get_system,
                       mpa = mpas, eez = eez, hud95 = hr_95, hud50 = hr_50)) %>% 
  unnest(system) %>% 
  st_as_sf() %>% 
  st_make_valid() %>% 
  arrange(iso3)

systems %>% 
  st_drop_geometry() %>% 
  select(iso3, id, fraction_as_reserve) %>% 
  filter(!id == "MPA") %>% 
  spread(id, fraction_as_reserve) %>% 
  knitr::kable(digits= 2)

file.remove(here("data", "mpa_systems.gpkg"))
st_write(systems, here("data", "mpa_systems.gpkg"))


get_system(mpa = mpas, wdpaid_want = 400011, eez = eez, iso3_want = "USA", hud95 = hr_95, hud50 = hr_50)






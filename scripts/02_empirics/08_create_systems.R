library(sf)
library(tidyverse)
library(here)

source(here("scripts", "02_empirics", "get_system.R"))


## Load data
# MPA
mpas <- st_read(here("data", "no_take_lsmpa_boundaries.gpkg")) %>% 
  filter(wdpaid %in% c(309888, 555629385, 400011, 11753))                                                  

# EEZ
eez <- st_read(here("data", "clean_world_eez_v11.gpkg")) %>% 
  filter(iso3 %in% c("USA", "KIR", "MEX", "ECU")) %>% 
  rmapshaper::ms_simplify(keep_shapes = T)

# Galapagos EEZ
mpa_eez <- st_read(here("data", "clean_world_eez_v11.gpkg")) %>% 
  filter(mrgid %in% c(8403)) 

## Movement data
# Load data on tuna habitat utilization
tuna_movement <- read.csv(here("data", "tuna_habitat_utilization.csv"),
                          stringsAsFactors = F) %>% 
  filter(name == "Yellowfin")                                               # !!!!!!Keep yellowfin for now !!!!!!

# Define targets
hr_95 <- mean(tuna_movement$util_95, na.rm = T)                             # mean 95% utilization distribution (km2)
hr_50 <- mean(tuna_movement$util_50, na.rm = T)                             # mean 50% utilization distribution (km2)



galapagos_system <- get_system(mpa = mpas,
                                wdpaid_want = 11753,
                               eez = eez,
                               iso3_want = "ECU",
                               hud95 = hr_95,
                               hud50 = hr_50)

revilla_system <- get_system(mpa = mpas,
                             wdpaid_want = 555629385,
                             eez = eez,
                             iso3_want = "MEX",
                             hud95 = hr_95,
                             hud50 = hr_50)

pipa_system <- get_system(mpa = mpas,
                          wdpaid_want = 309888,
                          eez = eez,
                          iso3_want = "KIR",
                          hud95 = hr_95,
                          hud50 = hr_50)

primnm_system <- get_system(mpa = mpas,
                    wdpaid_want = 400011,
                    eez = eez,
                    iso3_want = "USA",
                    hud95 = hr_95,
                    hud50 = hr_50)
  
  
  

rbind(galapagos_system,
      revilla_system,
      pipa_system) %>% 
  select(id) %>% 
  startR::st_rotate() %>% 
  plot()

plot(revilla_system, max.plot = 1)

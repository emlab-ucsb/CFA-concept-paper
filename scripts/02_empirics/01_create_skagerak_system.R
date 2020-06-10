
library(startR)
library(here)
library(sf)
library(tidyverse)

# Load helper functions
source(here("scripts", "00_helpers.R"))

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

# Cod stock boundary
cod <- st_read(dsn = "raw_data", layer = "WGBFAS-CODKAT-1971-2012-CHING", stringsAsFactors = F) %>% 
  rmapshaper::ms_simplify() %>% 
  st_transform(54009) %>% 
  st_difference(mpa) %>% 
  mutate(id = "COD",
         area = st_area(.)) %>% 
  select(id, area)

s <- units::drop_units(mpa$area / (cod$area + mpa$area))

## Movement data
# Cod movement (km) from Table 2 in Hobson et al.: Movements of North Sea cod
cod_movement <- read.csv(here("data", "cod_movement_data.csv")) %>% 
  mutate(days_edm = n_oc_edm * duration_edm,
         days_res = n_oc_res * duration_res,
         days_elm = n_oc_elm * duration_elm)

cod_movement_summary <- cod_movement %>% 
  select(contains("days")) %>% 
  gather(variable, value) %>% 
  group_by(variable) %>% 
  summarize(value = sum(value, na.rm = T)) %>% 
  ungroup()

ggplot(data = cod_movement_summary, aes(x = variable, y = value / sum(value))) +
  geom_col(fill = "steelblue", color = "black") +
  plot_theme() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Type of movement", y = "Days spent as portion of total")

cod_edm <- sum(cod_movement$days_edm * cod_movement$dist_edm) / sum(cod_movement$days_edm) * 1e3
cod_res <- sum(cod_movement$days_res * cod_movement$dist_res) / sum(cod_movement$days_res) * 1e3
cod_elm <- sum(cod_movement$days_elm * cod_movement$dist_elm) / sum(cod_movement$days_elm) * 1e3
cod_mean <- sum(cod_movement_summary$value * c(cod_edm, cod_elm, cod_res)) / sum(cod_movement_summary$value)

# Calculte buffer around mpa
mpa_buffer <- st_buffer(mpa, cod_mean)  %>% 
  mutate(id = "MPA buffer",
         area = st_area(.)) %>% 
  select(id, area)
  
d_11 <- units::drop_units(mpa$area / mpa_buffer$area)
d_12 <- 1 - d_11
d_21 <- s
d_22 <- 1 - s

d <- matrix(c(d_11, d_12, d_21, d_22), ncol = 2, byrow = T)

# Combine all polygons into one object
all_polygons <- rbind(cod,
                      mpa) %>% 
  st_transform(4326)

plot(all_polygons[,1])

ggplot(all_polygons) +
  geom_sf() +
  facet_wrap(~id) +
  ggtheme_plot()

## Save data
# Skagerak system
file.remove(here("data", "skagerak.gpkg"))
st_write(all_polygons, here("data", "skagerak.gpkg"))

## Portion of system as reserve
saveRDS(s, file = here("data", "skagerak_s.rds"))

## Save dispersal matrix
saveRDS(d, file = here("data", "skagerak_d.rds"))

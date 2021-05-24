# Load packages
library(here)     
library(cowplot)
library(raster)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(scales)

# Source functions
source(here("scripts", "00_helpers.R"))
source(here("scripts", "01_simulations", "01_model.R"))
source(here("scripts", "01_simulations", "02_wrapper.R"))
source(here("scripts", "02_empirics", "skagerak", "05_cod_default_parameters.R"))

#### Running simulations #######################################################
#
# To run simulations for many combinations of parameters, we use the 
# model wrapper function (wrapper.R). It takes as input all of the parameters
# passed to the model, as well as the desired variable (X_vec is the default).
#
################################################################################

# For loop iterating across all values of L - all other parameters are fixed

for (L in L_range){
  
  run <- wrapper(chi = chi, 
                 r = r,
                 K = K,
                 X0 = X0,
                 s = s,
                 D = D,
                 p = p,
                 q = q,
                 c = c,
                 beta = beta,
                 L = L,
                 alpha = alpha,
                 mu = mu,
                 w = w,
                 years = years,
                 want = "All",
                 b = b) %>% 
    mutate(X_rel = X_r_vec / X_f_vec,
           L = L)
  
  if(L == L_range[1]){
    best_results <- run
    
  }else{
    best_results <- rbind(best_results, run)
  }
  
}

(best_results$E_i_vec / best_results$E_f_vec)[1]

### ------------------------------------------------------
# Plot 1: L vs B for default parameters (chi is fixed)
### ------------------------------------------------------

equil_biomass_plot <- 
  ggplot(data = best_results,
         mapping = aes(x = L, y = X_vec / K)) +
  geom_point(color = "black", fill = "steelblue", shape = 21) +
  geom_vline(data = filter(best_results, X_vec == max(X_vec)),
             aes(xintercept = L),
             linetype = "dashed") +
  scale_fill_viridis_c() +
  plot_theme() +
  guides(size = guide_legend(title = expression(X[M] / X[`F`]))) +
  theme(legend.position = c(0.01, 1),
        legend.justification = c(0, 1)) +
  labs(x = L_legend,
       y = X_legend)

equil_biomass_plot


### ------------------------------------------------------
# Table 1: Information of optimal L for default paramters (chi is fixed)
### ------------------------------------------------------

max_conservation_benefit <- best_results %>%
  dplyr::filter(L == 0 | X_vec == max(X_vec, na.rm = T)) %>%
  dplyr::select(L, X = X_vec)



####################
############################################################################
#####               09_calculate_skagerak_system_effort               #####
############################################################################
#
# This script calculate the effort occurring in each polygon of the 
# Skagerak system MPA.
# 
############################################################################

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

extracted_values <- raster::extract(effort_raster, system,
                                    method = "simple",
                                    fun = sum,
                                    na.rm = T,
                                    sp = T)

data <- extracted_values %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  gather(year, fishing_days, -c(id, area)) %>%  
  mutate(id = fct_reorder(id, fishing_days, .desc = T),
         fishing_intensity = fishing_days / (area / 1e6))

## FIGURES
map <- effort %>%
  gather(year, fishing_days, -c(lon, lat)) %>% 
  group_by(lon, lat) %>% 
  summarize(fishing_days = mean(fishing_days, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = fishing_days)) +
  geom_sf(data = dnk, color = "transparent", fill = "gray") +
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

effort_in_poly <- ggplot(data, aes(x = id, y = fishing_intensity)) +
  stat_summary(geom = "col", fun = mean, fill = "steelblue", color = "black") +
  stat_summary(geom = "errorbar", fun.data = mean_sdl, width = 0.1) +
  labs(title = "Mean trawling intensity (2012 - 2019)",
       x = "Polygon",
       y = bquote("Trawling days"~km^-1)) +
  plot_theme()

plot <- plot_grid(map,
                  plot_grid(effort_in_poly,
                            equil_biomass_plot,
                            ncol = 1,
                            labels = c("B", "C")),
                  ncol = 2,
                  labels = "A")

lazy_ggsave(plot = plot, filename = "skagerak_system_effort", width = 22, height = 13)

####################

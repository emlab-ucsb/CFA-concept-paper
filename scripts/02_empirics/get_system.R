############################################################################
#####                             get_system                            ####
############################################################################
#
# This function combines different spatial layers to create "the system"
# in which our MPA is simulated.
# 
# There are some deffinitions of "the system", which we'll try to use:
# - A countries EEZ
# - The EEZ around the MPA only (when applicable)
# - 95% habitat utlization density (areas probably used)
# - 50% habitat utiliation density (probable core areas)
# 
# The function takes five parametrs, to of which are optional
# - mpa: a shapefile of the MPA of interest
# - eez (optional): The EEZ of the country of interest
# - mpa_eez (optional): The EEZ immediately around the MPA
# - HUD95 the habitat utilization density for a 95% kernel (area in km2) 
# - HUD50 the habitat utilization density for a 50% kernel (area in km2) 
# 
# We'll create a buffer with a radius of r = sqrt(HUD / pi), to obtain
# a cyrcle with area HUD. The buffer will be created from the MPA centroid.
# 
############################################################################

get_system <- function(mpa, wdpaid_want, eez = NULL, iso3_want, mpa_eez = NULL, hud95, hud50) {
  # Housekeeping
  mpa <- mpa %>% 
    filter(wdpaid %in% wdpaid_want) %>% 
    mutate(id = "MPA") %>% 
    select(id)
  
  # MPA characteristics
  mpa_centroid <- st_centroid(mpa)
  mpa_area <- st_area(mpa)
  
  ## CREATE POLYGONS FOR HABITAT UTILIZATION DENSITIES
  # Define radius
  hud_95_rad <- sqrt(hr_95 / pi) * 1e3                                        # Radius of 95% utilization (m)
  hud_50_rad <- sqrt(hr_50 / pi) * 1e3                                        # Radius of 50% utilizatio (m)
  
  # Create "system" circle around the centroid
  hud95_poly <- mpa_centroid %>% 
    st_buffer(dist = hud_95_rad) %>%                                           # For 95% (probable areas)
    st_difference(mpa) %>%                                                    # Remove the galapagos polygon from it
    mutate(id = "95% HUD") %>%                                                # Reame for identification
    select(id)                                                                # Keep id column only
  
  hud50_poly <- mpa_centroid %>% 
    st_buffer(dist = hud_50_rad) %>%                                          # For 50% (core areas)
    mutate(id = "50% HUD") %>%                                                # Reame for identification
    select(id)                                                                # Keep id column only
  
  # Combine all polygons into one object
  all_polygons <- rbind(mpa,
                        hud95_poly,
                        hud50_poly)
  
  # If an EEZ is provided, rbind it to the data
  if(!is.null(eez)) {
    eez <- eez %>% 
      filter(iso3 %in% iso3_want) %>% 
      st_difference(mpa) %>% 
      mutate(id = "EEZ") %>% 
      select(id)
    
    all_polygons <- rbind(all_polygons, eez)
  }
  
  # If an MPA EEZ is provided, rbind it to the data
  if(!is.null(mpa_eez)) {
    mpa_eez <- mpa_eez %>% 
      filter(iso3 %in% iso3_want) %>% 
      st_difference(mpa) %>% 
      mutate(id = "MPA EEZ") %>% 
      select(id)
    
    all_polygons <- rbind(all_polygons, mpa_eez)
  }  
  
  ## COMBINE ALL POLYGONS
  all_polygons <- all_polygons %>% 
    mutate(area = st_area(.),                                                 # Calculate the area of each polygon
           id = fct_relevel(id, c("EEZ",                                      # Reorder polygons for display
                                  "MPA EEZ",
                                  "95% HUD",
                                  "50% HUD",
                                  "MPA")),
           fraction_as_reserve = mpa_area / (area + mpa_area),                # Calculate fraction of system that is resrve
           fraction_as_reserve = ifelse(area < mpa_area,                      # If polygon area is less than mpa area, then ignore this as a system
                                        NA, 
                                        fraction_as_reserve),
           fraction_as_reserve = ifelse(area == mpa_area,                     # If polygon area is same as MPA, than this is the MPA, not the system
                                        NA,
                                        fraction_as_reserve),
           where = ifelse(id == "MPA",
                          "MPA",
                          "Outside")) %>%                                          # Define inside / outside
    st_transform(4326)
  
  return(all_polygons)
}












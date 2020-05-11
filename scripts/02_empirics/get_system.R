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
  # browser()
  # Housekeeping
  mpa <- mpa %>% 
    filter(wdpaid %in% wdpaid_want) %>% 
    mutate(id = "MPA") %>% 
    select(id) %>% 
    mutate(area = st_area(.))
  
  # MPA characteristics
  mpa_centroid <- st_cast(mpa, "POLYGON") %>% 
    st_centroid()
  mpa_area <- st_area(mpa)
  
  ## CREATE POLYGONS FOR HABITAT UTILIZATION DENSITIES
  # If the MPA has multiple polygons, we have to split the area for each
  area_95_split <- hud95
  area_50_split <- hud50
  
  # Define radius
  hud_95_rad <- (sqrt(area_95_split / pi) * 1e3)                             # Radius of 95% utilization (m)
  hud_50_rad <- (sqrt(area_50_split / pi) * 1e3)                             # Radius of 50% utilizatio (m)
  
  # Create "system" circle around the centroid
  hud95_poly <- mpa_centroid %>% 
    st_buffer(dist = hud_95_rad) %>%                                          # For 95% (probable areas)
    mutate(id = "95% HUD") %>%                                                # Reame for identification
    group_by(id) %>% 
    summarize(a = 1) %>% 
    ungroup() %>% 
    select(id) %>%                                                             # Keep id column only
    mutate(area = st_area(.))                                                               
  
  if(st_area(hud95_poly) > mpa_area){
    hud95_poly <- st_difference(hud95_poly, mpa) %>%                          # Remove the galapagos polygon from it
      select(id, area)
  }
  
  hud50_poly <- mpa_centroid %>% 
    st_buffer(dist = hud_50_rad) %>%                                          # For 50% (core areas)
    mutate(id = "50% HUD") %>%                                                # Reame for identification
    group_by(id) %>% 
    summarize(a = 1) %>% 
    ungroup() %>% 
    select(id) %>%                                                                 # Keep id column only
    mutate(area = st_area(.))
  
  # range <- range %>% 
  #   mutate(id = "Range") %>% 
  #   group_by(id) %>% 
  #   summarize(a = 1) %>% 
  #   ungroup() %>% 
  #   st_difference(mpa) %>% 
  #   select(id) 
  
  # Combine all polygons into one object
  all_polygons <- rbind(mpa,
                        hud95_poly,
                        hud50_poly)
  
  # If an EEZ is provided, rbind it to the data
  if(!is.null(eez)) {
    eez <- eez %>% 
      filter(iso3 %in% iso3_want) %>% 
      group_by(iso3) %>% 
      summarize(a = 1) %>% 
      ungroup() %>% 
      st_difference(mpa) %>% 
      mutate(id = "EEZ") %>% 
      select(id) %>% 
      mutate(area = st_area(.))
    
    all_polygons <- rbind(all_polygons, eez)
  }
  
  # If an MPA EEZ is provided, rbind it to the data
  # if(!is.null(mpa_eez)) {
  #   mpa_eez <- mpa_eez %>% 
  #     filter(iso3 %in% iso3_want) %>% 
  #     group_by(iso3) %>% 
  #     summarize(a = 1) %>% 
  #     ungroup() %>% 
  #     st_difference(mpa) %>% 
  #     mutate(id = "MPA EEZ") %>% 
  #     select(id)
  #   
  #   all_polygons <- rbind(all_polygons, mpa_eez)
  # }  
  # 
  ## COMBINE ALL POLYGONS
  all_polygons <- all_polygons %>% 
    mutate(id = fct_relevel(id, c("EEZ",                                      # Reorder polygons for display
                                  "MPA EEZ",
                                  "95% HUD",
                                  "50% HUD",
                                  "MPA")),
           fraction_as_reserve = units::drop_units(mpa_area / area),                # Calculate fraction of system that is resrve
           # fraction_as_reserve = ifelse(mpa_area > area,                      # If polygon area is less than mpa area, then ignore this as a system
                                        # NA,
                                        # fraction_as_reserve),
           fraction_as_reserve = ifelse(area == mpa_area,                     # If polygon area is same as MPA, than this is the MPA, not the system
                                        NA,
                                        fraction_as_reserve),
           where = ifelse(id == "MPA",
                          "MPA",
                          "Outside")) %>%                                          # Define inside / outside
    st_transform(4326) %>% 
    select(id, area, fraction_as_reserve, where)
  
  return(all_polygons)
}












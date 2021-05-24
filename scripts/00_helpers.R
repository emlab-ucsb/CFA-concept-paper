########################################################
#                    HELPER FUNCTIONS                  #
########################################################
# This script contains some helper functions to 
# streamline our work.
########################################################

#### MODEL STUFF ####
# The function below returns a panel figure
# with all state variables through time
fast_check <- function(chi, r, K, X0, D, p, q, c, beta, L, alpha, mu, w, years, tolerance = 0.05) {
  model(
    chi = chi,
    r = r,
    K = K,
    X0 = X0,
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
    tolerance = tolerance
  ) %>%
    mutate_at(.vars = vars(X_r_vec, X_f_vec), .funs =  ~ . / (K / 2)) %>% 
    mutate(X_vec = X_vec / K) %>% 
    gather(variable, value, - time) %>% 
    ggplot(aes(x = time, y = value)) +
    geom_line() +
    facet_wrap(~variable, scale = "free_y")
}



#### FIGURE STUFF ####
# Plot theme function
plot_theme <- function (font_size = 10, font_family = "", line_size = 0.5) {
  
  half_line <- font_size / 2
  
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      rect = element_rect(
        fill = "transparent",
        colour = NA,
        color = NA,
        size = 0,
        linetype = 0),
      text = element_text(
        family = font_family,
        face = "plain",
        colour = "black",
        size = font_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        margin = margin(),
        debug = FALSE),
      strip.text = element_text(
        hjust = 0,
        margin = margin(b = 10)),
      plot.title = element_text(
        family = font_family,
        face = "plain",
        colour = "black",
        size = rel(1.2),
        margin = margin(),
        debug = FALSE),
      axis.text = element_text(
        colour = "black",
        size = rel(0.8)),
      axis.ticks = element_line(
        colour = "black"),
      axis.line = element_line(
        colour = "black",
        size = line_size,
        lineend = "square"),
      axis.line.x = element_line(
        colour = "black",
        size = line_size,
        lineend = "square"),
      axis.line.y = element_line(
        colour = "black",
        size = line_size,
        lineend = "square"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(1, "lines"),
      legend.spacing = unit(0.4, "cm"),
      legend.text = element_text(size = rel(0.8)),
      panel.background = element_rect(fill = "transparent", color = "transparent"),
      strip.background = element_rect(fill = "transparent", color = "transparent"),
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      complete = TRUE
    )
}


# Save function
# The function saves figures as png and pdf
lazy_ggsave <- function(plot, filename, width = 7, height = 5){
  # Save as png
  ggsave(plot = plot,
         here("results", "img", paste0(filename, ".png")),
         width = width,
         height = height,
         units = "cm")
  
  # Save as pdf
  ggsave(plot = plot,
         here("results", "img", paste0(filename, ".pdf")),
         width = width,
         height = height,
         units = "cm")
}

# Different dispersal scenarios
# Function that creates a Dispersal matrix
make_D <- function(self_rec){
  
  exports <- 1 - self_rec
  
  D <- matrix(c(self_rec, exports,
                exports, self_rec),
              nrow = 2,
              byrow = T)
  
  return(D)
}


################## EMPIRICS #####################

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
  # Define radius
  hud_95_rad <- (sqrt(hud95 / pi) * 1e3)                             # Radius of 95% utilization (m)
  hud_50_rad <- (sqrt(hud50 / pi) * 1e3)                             # Radius of 50% utilizatio (m)
  
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
                                  # "MPA EEZ",
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


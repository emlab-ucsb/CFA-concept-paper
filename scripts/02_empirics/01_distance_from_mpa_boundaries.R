####################################################################################
#####                       distance_from_mpa_boundaries                        ####
####################################################################################
#
# This script will create a gridded dataset of "distance to MPA boundary".
# We will use all MPAs with more than 10,000 km2 extension, and 
# with some portion designated as no-take.
# 
# The final dataset will have the following columns:
#   - lat:        center of the latitude bin
#   - lon:        center of the longitude bin
#   - distance:   negative (positive) values are inside (outside) the border
#   - wdpa_pid:   polygon identifier in WDPA
#   - no_take:    dummy variable indicating if ALL the MPA is no-take
# 
# This dataset will be used to categorize fishing effort around MPA boundaries.
####################################################################################



## SET UP SECTION ##################################################################
## Load libraries
library(here)
library(sf)
library(bigrquery)
library(lwgeom)
library(raster)
library(fasterize)
library(janitor)
library(tidyverse)

## Load helper functions
source(here("scripts", "00_helpers.R"))

## Load data
# We beggin by loading the MPA boundaries, available from protectedplanet.net
mpa_boundaries <-
  st_read(here::here("data", "WDPA_Nov2019_No_Terrestrial/"),
          layer = "WDPA_Nov2019_No_Terrestrial",
          stringsAsFactors = FALSE,
          as_tibble = TRUE) %>%
  clean_names(case = "snake") %>%                              # Clean column names
  select(wdpaid,                                              # Select features
         name,
         iucn_cat,
         area_km = rep_m_area,
         no_take,
         year = status_yr)



## PROCESS SECTION ####################################################################
## Filter MPAS we want
# There are polygons we don't want, like duplicated polygons or the ross sea area
wdpaid_discard <- c("10708",     # Galapagos, same as 11753
                    "312243",    # Old Papahānaumokuākea before expansion
                    "555512062", # Kermadek, same as 478297
                    "555586815", # Mariana trench, same as 400010
                    "555622118", # PNMS, to be implemented in 2020
                    "2628")

no_takes <- c("309888", # PIPA is no take for industrials
              "400011", # PRINMS is no take for industrials
              "902308") # Revilla core is no take

take <- c("555586806", # Research area, I call BS
          "555586979", # Mid atlantic, just a US management area
          "555586980", # Mid atlantic, same as 555586979
          "555586981", # Same as above
          "555587005", # Same as above
          "555586970", # Same as above
          "555512241", # Charlie-Gibbs South High Seas MPA in Areas Beyond National Jurisdiction
          "555557228") # Charlie-Gibbs North High Seas MPA in Areas Beyond National Jurisdiction
          
# Filter for only MPAs that include some portion as a no-take and filter by siz
no_take_lsmpa_boundaries <-
  mpa_boundaries %>%
  filter(!wdpaid %in% wdpaid_discard) %>%                     # Remove duplicates
  filter(area_km > 5e3) %>%                                   # Keep areas larger than 5,000 Km2
  filter(year != 0) %>%                                       # Remove ones with missing year
  mutate(iucn_cat = case_when(wdpaid == 309888 ~ "Ib",
                              wdpaid == 400011 ~ "Ib",
                              wdpaid == 11753 ~ "IV",
                              wdpaid == 555622041 ~ "Ia",
                              T ~ iucn_cat)) %>% 
  st_make_valid() %>%                                         # Fix weird polygons
  group_by(wdpaid, name, no_take, iucn_cat) %>%
  summarize(a = 1) %>% 
  ungroup() %>% 
  select(-a) %>% 
  st_cast("MULTIPOLYGON") %>%                                 # Cast to multipolygon
  st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%                               # Reproject to Mollweide (https://epsg.io/54009)
  nngeo::st_remove_holes() %>%                                # Keep the boundary only (i.e. remove interior borders)
  mutate(iucn_cat_rast = case_when(iucn_cat %in% c("Ia", "Ib") ~ 1,
                                   iucn_cat == "II" ~ 2,
                                   iucn_cat == "III" ~ 3,
                                   iucn_cat == "IV" ~ 4,
                                   iucn_cat == "V" ~ 5,
                                   iucn_cat == "VI" ~ 6,
                                   T ~ 7))

lonlat_crs <- "+proj=longlat +datum=WGS84 +no_defs"           # longlat crs proj4string
mol_crs <- st_crs(no_take_lsmpa_boundaries)$proj4string       # Extract the mollweide crs proj4string

# Export no take LSMPA boundaties
file.remove(here("data", "no_take_lsmpa_boundaries.gpkg"))
st_write(obj = no_take_lsmpa_boundaries,
         dsn = here("data", "no_take_lsmpa_boundaries.gpkg"))

# Visual check #1
plot(no_take_lsmpa_boundaries[, "iucn_cat"])

## Rassterization
# The first step is to create a reference raster that covers the whole world
r <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
            res = 0.1,                                        # Set resolution
            val = 1,                                          # Assign a value
            crs = lonlat_crs) %>%                             # Unprojected coords
  projectRaster(crs = mol_crs)

# We will have three rasters:
# - One with the identity of each polygon
# - One with the category of each polygon
# - One with the distance to each boundary
# First ,the identity one
wdpaid_raster <- 
  no_take_lsmpa_boundaries %>% 
  st_buffer(100 * 1e3 * 1.854) %>%                             # Create a 100 nm buffer
  st_cast("MULTIPOLYGON") %>%                                  # Cast
  fasterize(r, field = "wdpaid") %>%                           # Rasterize
  projectRaster(crs = lonlat_crs,
                res = 0.1,
                method = "ngb")                                # Reproject to longlat (whan we need for GFW)

# Visual check #2
plot(wdpaid_raster)

# Now the one with IUCN categories
iucn_cat_raster <- 
  no_take_lsmpa_boundaries %>% 
  st_buffer(100 * 1e3 * 1.854) %>%                             # Create a 100 nm buffer
  st_cast("MULTIPOLYGON") %>%                                  # Cast
  fasterize(r, field = "iucn_cat_rast") %>%                    # Rasterize
  projectRaster(crs = lonlat_crs,
                res = 0.1,
                method = "ngb")                                # Reproject to longlat (whan we need for GFW)

# Visual check #3
plot(iucn_cat_raster)

# Now, we'll create the distance to border raster.
# This will be composed of two rasters:
# - Distance from outside to the border
# - Distance from inside to the border
outside_raster <- fasterize(no_take_lsmpa_boundaries, r)        # Rasterize the boundaries
distance_outside <- distance(outside_raster)                    # Calculate the distance to the boundaries

# Visual check #4
plot(distance_outside)

# Now we repeat the process but calculate distance form inside (negative numbers)
inside_raster <- is.na(outside_raster)                          # A raster with 1 and 0 for outside / inside
inside_raster[inside_raster == 0] <- NA                         # Replace 0 (inside) with NAs to be filled in
distance_inside <- - distance(inside_raster)                    # Calculate distance to border (note the "-")

# Visual check #5
plot(distance_inside)

# We'll now combine both
distance_raster <- distance_outside
distance_raster[distance_inside < 0] <- distance_inside[distance_inside < 0]
distance_final <- projectRaster(distance_raster, crs = lonlat_crs, res = 0.1)
distance_final[distance_final > 100 * 1e3 * 1.854] <- NA

# Visual check #6
plot(distance_final)

## TABULIZE ################################################################################
# The next step is to make the rasters into a table that can be uploaded to GBQ
# wdpa_pid first
wdpa_pid_table <- as.data.frame(wdpaid_raster, xy = T) %>% 
  rename(lon = x, lat = y, wdpaid = layer)

iucn_cat_table <- as.data.frame(iucn_cat_raster, xy = T) %>% 
  rename(lon = x, lat = y, iucn_cat = layer)

# Now distance
distance_final_table <- as.data.frame(distance_final, xy = T) %>% 
  rename(lon = x, lat = y, distance = layer)

# Cobmine them
output_table <- left_join(distance_final_table, wdpa_pid_table, by = c("lon", "lat")) %>%
  left_join(iucn_cat_table, by = c("lon", "lat")) %>% 
  mutate_at(vars(lon, lat), function(x){(floor(x / 0.1) * 0.1) + 0.05}) %>% 
  drop_na(distance)

# Export the data
write.csv(output_table,
          here("data", "gridded_distance_from_lsmpa_borders.csv"),
          row.names = F)

# Export a nicer version as a map
coast <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

distance_to_no_take_mpa_border_map <- 
  output_table %>% 
  filter(distance > -100e3 * 1.854) %>% 
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = distance)) +
  geom_sf(data = st_transform(no_take_lsmpa_boundaries, lonlat_crs),
          fill = "transparent",
          color = "red") +
  geom_sf(data = coast, color = "transparent", fill = "black") +
  startR::ggtheme_map() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  guides(fill = guide_colorbar(title = "Distance",
                               ticks.colour = "black",
                               frame.colour = "black")) 
  
lazy_ggsave(plot = distance_to_no_take_mpa_border_map,
            filename = "distance_to_no_take_mpa_border_map")

## SEND TO GOOGLE BIG QUERY ##############################################################
ucsb_project <- "emlab-gcp" # Name of our project

# Create a bq_table object
dist_to_lsmpa <- 
  bq_table(project = ucsb_project,
           dataset = "ocean_halos_v2",
           table = "dist_to_lsmpa") 

# Overwrite or create
if(!bq_table_exists(dist_to_lsmpa)){
  bq_table_create(dist_to_lsmpa,
                  fields = output_table) %>%
    bq_table_upload(values = output_table)
  
} else {
  bq_table_delete(bq_table(project = ucsb_project,
                           dataset = "ocean_halos_v2",
                           table = "dist_to_lsmpa"))
  
  bq_table_create(dist_to_lsmpa,
                  fields = output_table) %>%
    bq_table_upload(values = output_table)
}


library(here)
library(rnaturalearth)
library(sf)
library(tidyverse)


# Load helper functions
source(here("scripts", "00_helpers.R"))

world <- ne_countries(returnclass = "sf")

# Map of PNA with PINA and PNMS

pipa <- st_read(here::here("raw_data", "PNA", "PIPA.gpkg")) %>% 
  mutate(id = "No Take") %>% 
  select(id)

# Load PNMS
pnms <- st_read(here("raw_data", "PNA", "PNMS.gpkg")) %>% 
  mutate(id = "No Take") %>% 
  select(id)

pna_iso3 <- c("FSM", "NRU", "PLW", "KIR", "SLB", "TKL", "MHL", "PNG", "TUV")

eez_subset <- st_read(here::here("raw_data", "PNA", "EEZ_subset.gpkg")) %>% 
  rmapshaper::ms_simplify(keep_shapes = T) %>%
  group_by(ISO_Ter1) %>% 
  summarize(g = 1) %>% 
  ungroup() %>% 
  st_make_valid() %>% 
  mutate(id = ifelse(ISO_Ter1 %in% pna_iso3, "PNA", "Other EEZ")) %>% 
  select(id)

hsp <- tibble(x = c(170, 155, 155, 175, 180, 170),
              y = c(-10.5, 0, 7, 7, -10.5, -10.5)) %>%
  as.matrix() %>% 
  list() %>%
  st_polygon() %>% 
  st_sfc() %>% 
  st_set_crs(4326) %>% 
  # st_sf() %>% 
  # mutate(a = 1) %>% 
  st_difference(st_union(eez_subset)) %>% 
  st_as_sf() %>% 
  mutate(id = "No Take") %>% 
  select(id) %>% rename(geom = x)

pna_coast <- st_crop(world, extent(eez_subset)) %>% 
  filter(!sov_a3 == "AU1")

poly <- rbind(pipa, pnms, hsp, eez_subset) %>% 
  mutate(id =fct_relevel(id, c("Other EEZ", "PNA", "No Take")))

effort <- readRDS(here("raw_data", "PNA", "gridded_effort_by_gear_and_year_dist_to_mpa.rds")) %>% 
  filter(best_vessel_class == "tuna_purse_seines",
         between(lat, -20, 20),
         lon < -160 | lon > 100) %>% 
  mutate(lon = ifelse(lon < 0, lon + 360, lon)) %>%
  group_by(lon, lat) %>% 
  summarize(days = mean(fishing_hours) / 24) %>% 
  ungroup()

pna_map <- ggplot() +
  geom_sf(data = pna_coast, color = "black", size = 0.3) +
  geom_raster(data = effort, aes(x = lon, y = lat, fill = days)) +
  geom_sf(data = poly, aes(color = id), fill = "transparent", size = 0.3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(130, 210)) +
  startR::ggtheme_map() +
  labs(color = "Polygon") +
  guides(fill = guide_colorbar(title = "Fishing days",
                               frame.colour = "black",
                               ticks.colour = "black"),
         color = FALSE)

pna_map


# Map of Mexico with Natividad and SAM

# Load ZRP shapefile
zrp <- st_read(dsn = here("raw_data", "mex"), layer = "ZRP", quiet = T) %>% 
  filter(Name %in% c("El Cabezo",
                     "Gallineros",
                     "Punta Loria",
                     "San Roman Norte",
                     "San Roman Sur",
                     "Anegado de Chal",
                     "Laguna Canche Balam",
                     "Niluc",
                     "Mimis",
                     "El Faro (Pesca de Langosta)",
                     "El Faro")) %>%
  st_transform(crs = 4326) %>%
  mutate(id = "No Take",
         loc = "sam") %>% 
  select(id, loc) %>% 
  st_make_valid()

# Load Community reserve shapefile
rescom <- st_read(dsn = here("raw_data", "mex"), layer = "Res_Com", quiet = T) %>% 
  filter(Nombre %in% c("La_Plana", "PuntaPrieta")) %>% 
  st_transform(crs = 4326) %>% 
  mutate(id = "No Take") %>% 
  group_by(id) %>% 
  summarize(a = 1) %>%
  ungroup() %>% 
  st_make_valid() %>% 
  select(id) %>% 
  mutate(loc = "pbc")


# Load TURF shapefile
turf <- st_read(dsn = here("raw_data", "mex"), layer = "turfs", quiet = T) %>% 
  filter(Coop %in% c("SCPP Buzos y Pescadores de la Baja California",
                     "SCPP Cozumel",
                     "SCPP Jose Maria Azcorra")) %>% 
  mutate(id = "TURF") %>% 
  st_transform(crs = 4326) %>% 
  mutate(loc = ifelse(Coop == "SCPP Buzos y Pescadores de la Baja California", "pbc", "sam")) %>% 
  select(id, loc)

coastline_mx <- st_read(here("raw_data", "mex"), layer = "coastline") %>% 
  rmapshaper::ms_simplify(keep_shapes = T)

ph <- ggplot() +
  geom_sf(data = turf[c(1,3),], fill = "steelblue", alpha = 0.3, color = "black") +
  geom_sf(data = zrp, fill = "red2", color = "red2") +
  geom_sf(data = coastline_mx, color = "black", size = 0.3) +
  lims(x = c(-87.85, -87.25), y = c(19.15, 19.6)) +
  startR::ggtheme_map()

# Map for PBC #################################################
pbc <- ggplot() +
  geom_sf(data = turf[4,], fill = "steelblue", alpha = 0.5, size = 0.3, color = "black") +
  geom_sf(data = rescom, fill = "red2", color = "red2") +
  lims(x = c(-115.65, -115.109), y = c(27.7, 28)) +
  geom_sf(data = coastline_mx, color = "black", size = 0.3) +
  startR::ggtheme_map()

# Map of the world

sam_bbox <- (st_bbox(zrp) + c(-4, -3, 4, 3)) %>% 
  st_as_sfc() %>% 
  st_sf()

pbc_bbox <- (st_bbox(rescom) + c(-4, -3, 4, 3)) %>% 
  st_as_sfc() %>% 
  st_sf()

pna_bbox <- #(st_bbox(eez_subset) + c(-360, 0, 0, 0)) %>% 
  # st_as_sfc()
  
  tibble(x = c(145, 145, 180, 180, 145),
         y = c(-10.5, 7, 7, -10.5, -10.5)) %>%
  as.matrix() %>% 
  list() %>%
  st_polygon() %>% 
  st_sfc() %>% 
  st_set_crs(4326) %>% 
  st_sf()

references <- rbind(sam_bbox, pbc_bbox, pna_bbox)

w <- world %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=-180")) %>%
  st_transform("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

w_plot <- ggplot() + 
  geom_sf(data = w, fill = "black", color = "black") +
  geom_sf(data = references, color = "red", fill = "transparent", size = 1) +
  geom_sf_text(data = references, label = c("D", "C", "B"),
               nudge_x = 1.5e6 * c(-1, -1, 0),
               nudge_y = -1e6 * c(1, 1, 0)) +
  startR::ggtheme_map()

final_plot <- plot_grid(w_plot, pbc, pna_map, ph,
                        ncol = 2,
                        labels = c("A", "C", "B", "D"))

lazy_ggsave(plot = final_plot,
            filename = "proto_cfa_map",
            width = 22,
            height = 12.5)


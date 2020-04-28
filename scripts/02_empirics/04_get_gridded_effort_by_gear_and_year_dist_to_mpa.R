# Load packages
library(here)
library(connections)
library(bigrquery)
library(tidyverse)

# Create a connection to the blue halos project
# You will be prompted to authenticate using your authorized email
# After this, you'll see them appear in your connections pane, where you can explroe each project!
ocean_halos_v2 <- connection_open(
  bigquery(),
  project = "emlab-gcp",          # project you want to connect to
  dataset = "ocean_halos_v2",     # dataset we're working on
  billing = "emlab-gcp",          # Who's paying for this?
  use_legacy_sql = FALSE,         # Just don't
  allowLargeResults = TRUE        # Give me all the data
)

table_name <- "gridded_effort_by_gear_and_year_dist_to_mpa"

# Download the data (and export it)
data <- tbl(ocean_halos_v2, table_name) %>% 
  collect()

# Save the data
saveRDS(object = data,
        file = here("data", paste0(table_name, ".rds")))

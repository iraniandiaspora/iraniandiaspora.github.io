# generate_ontario_csds.R
# Produces: output_population_page/ontario_csds_iranian.rds
# Ontario CSD-level Iranian population from cancensus API.
# Requires cancensus API key set in environment: CANCENSUS_API_KEY
#
# NOTE: This script calls the cancensus API and requires an active internet
# connection. The API key is: CensusMapper_ac515a11272c029c783598b782adb52b

library(tidyverse)
library(cancensus)
library(sf)
library(here)

cat("=== Generating ontario_csds_iranian.rds ===\n")

# Set API key if not already set
if (Sys.getenv("CANCENSUS_API_KEY") == "") {
  Sys.setenv(CANCENSUS_API_KEY = "CensusMapper_ac515a11272c029c783598b782adb52b")
}

# Iranian-related census vectors for 2021 Census
# v_CA21_4917: Iranian ethnic origin (single + multiple responses)
# v_CA21_4245: Born in Iran (place of birth)
vectors <- c(
  ethnic_iranian = "v_CA21_4917",
  born_iran = "v_CA21_4245"
)

# Pull CSD-level data for Ontario (PR=35) with geometry
ontario_csds <- get_census(
  dataset = "CA21",
  regions = list(PR = "35"),
  vectors = vectors,
  level = "CSD",
  geo_format = "sf",
  quiet = TRUE
)

# Filter to CSDs with any Iranian population
ontario_iranian <- ontario_csds %>%
  mutate(pop = pmax(ethnic_iranian, born_iran, na.rm = TRUE)) %>%
  filter(pop > 0) %>%
  arrange(desc(pop))

saveRDS(ontario_iranian, here("output_population_page/ontario_csds_iranian.rds"))
cat("Saved ontario_csds_iranian.rds:", nrow(ontario_iranian), "CSDs with Iranian population\n")
cat("Top CSDs:\n")
print(head(ontario_iranian %>% st_drop_geometry() %>% select(name, pop), 10))

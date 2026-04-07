# Export CA county choropleth data for dashboard
# Uses ACS 5-year PUMS microdata (iran_data.Rda) with PUMA-to-county crosswalk
# Replaces old approach that used published ACS tables (B04006 + B05006)
# Output: joined_counties.Rda for IDD dashboard
# Run: Rscript export_ca_counties.R
#
# Previous version (pre-2026-04-01): used published tables, got 245,893
# Current version: uses compound definition from microdata, gets 353,221
# The difference is children in Iranian households + MENA race (in 1-year only,
# but inherited via compound definition in iran_data.Rda from 5-year PUMS)

library(sf)
library(tigris)
library(dplyr)
options(tigris_use_cache = TRUE)

DATA_DIR <- "output/dashboard_us"

cat("=== Exporting CA County Map Data (PUMA crosswalk method) ===\n")
cat("Started:", format(Sys.time()), "\n\n")

# --- Load Iranian microdata ---
cat("Loading iran_data.Rda...\n")
ep <- new.env(); load(file.path(DATA_DIR, "iran_data.Rda"), envir = ep)
iran <- ep$iran_data

ca_iran <- iran %>% filter(STATEFIP == 6)
cat("CA records:", nrow(ca_iran), "\n")
cat("CA weighted pop:", format(sum(ca_iran$PERWT), big.mark = ","), "\n\n")

# --- Build PUMA-to-county crosswalk via spatial area intersection ---
# 2020 PUMAs: 281 in CA, most map 1:1 to counties, 7 span multiple counties
cat("Loading PUMA and county geometries (2020 vintage)...\n")
ca_pumas <- pumas(state = "06", year = 2020, cb = TRUE) %>% st_transform(3310)
ca_counties <- counties(state = "06", year = 2020, cb = TRUE) %>% st_transform(3310)

cat("Computing spatial intersection for allocation factors...\n")
sf_use_s2(FALSE)
xwalk <- st_intersection(ca_pumas, ca_counties) %>%
  mutate(int_area = as.numeric(st_area(geometry)))

puma_areas <- ca_pumas %>%
  mutate(puma_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  select(PUMACE20, puma_area)

xwalk_df <- xwalk %>%
  st_drop_geometry() %>%
  select(PUMACE20, COUNTYFP, NAME, NAMELSAD, GEOID, int_area) %>%
  left_join(puma_areas, by = "PUMACE20") %>%
  mutate(alloc_factor = int_area / puma_area) %>%
  # Drop boundary slivers (<0.1% of PUMA area)
  filter(alloc_factor > 0.001)

# Renormalize after dropping slivers
xwalk_df <- xwalk_df %>%
  group_by(PUMACE20) %>%
  mutate(alloc_factor = alloc_factor / sum(alloc_factor)) %>%
  ungroup()

cat("Crosswalk:", nrow(xwalk_df), "rows,",
    length(unique(xwalk_df$PUMACE20)), "PUMAs,",
    length(unique(xwalk_df$COUNTYFP)), "counties\n")
multi_county <- xwalk_df %>% group_by(PUMACE20) %>% filter(n() > 1, alloc_factor > 0.01)
cat("PUMAs spanning multiple counties (>1% overlap):",
    length(unique(multi_county$PUMACE20)), "\n\n")

# --- Allocate weighted population to counties ---
cat("Allocating population via area-weighted crosswalk...\n")
ca_iran$PUMA_char <- ca_iran$PUMA
county_pop <- ca_iran %>%
  select(PUMA_char, PERWT) %>%
  left_join(xwalk_df %>% select(PUMACE20, COUNTYFP, alloc_factor),
            by = c("PUMA_char" = "PUMACE20"),
            relationship = "many-to-many") %>%
  mutate(allocated_wt = PERWT * alloc_factor) %>%
  group_by(COUNTYFP) %>%
  summarize(pop_estimate = round(sum(allocated_wt, na.rm = TRUE)), .groups = "drop")

# Check for unmatched PUMAs
unmatched <- ca_iran %>% filter(!PUMA_char %in% xwalk_df$PUMACE20)
if (nrow(unmatched) > 0) {
  cat("WARNING:", nrow(unmatched), "records with unmatched PUMAs\n")
  cat("  Lost weight:", sum(unmatched$PERWT), "\n")
}

# --- Join to county geometries ---
cat("Joining to county geometries...\n")
ca_counties_4326 <- counties(state = "06", year = 2020, cb = TRUE)

joined_counties <- ca_counties_4326 %>%
  left_join(county_pop, by = "COUNTYFP") %>%
  mutate(pop_estimate = ifelse(is.na(pop_estimate), 0, pop_estimate))

state_total <- sum(joined_counties$pop_estimate)
joined_counties$percent <- round(joined_counties$pop_estimate / state_total * 100, 2)
# No MOE from microdata PUMA allocation (unlike published tables)
joined_counties$moe <- NA_real_
joined_counties$GEOIDFQ <- paste0("0500000US", joined_counties$GEOID)

joined_counties <- joined_counties %>%
  select(STATEFP, COUNTYFP, COUNTYNS, GEOIDFQ, GEOID, NAME, NAMELSAD,
         STUSPS, STATE_NAME, LSAD, ALAND, AWATER, pop_estimate, moe, geometry, percent)

# --- Summary ---
cat("\n=== Top 15 counties ===\n")
top <- joined_counties %>%
  st_drop_geometry() %>%
  arrange(desc(pop_estimate)) %>%
  head(15)
for (i in 1:nrow(top)) {
  cat(sprintf("  %-20s %8s  (%5.1f%%)\n",
    top$NAME[i], format(top$pop_estimate[i], big.mark = ","), top$percent[i]))
}

cat("\nTotal CA Iranian pop (compound def):", format(state_total, big.mark = ","), "\n")
cat("Counties with pop > 0:", sum(joined_counties$pop_estimate > 0), "\n")

# --- Save ---
outfile <- file.path(DATA_DIR, "joined_counties.Rda")
save(joined_counties, file = outfile)
cat("\nSaved:", outfile, "\n")
cat("File size:", round(file.size(outfile) / 1e3), "KB\n")
cat("Finished:", format(Sys.time()), "\n")

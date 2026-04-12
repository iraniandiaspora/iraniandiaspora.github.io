# Extract clean Iran-born rows from the raw NOMIS bulk download CSVs and
# write compact tidy files that the UK page builder consumes.
#
# Input (raw NOMIS CSVs, 28 columns each, one row per local authority):
#   data/uk/iran_by_lad154.csv     -- Lower-tier local authorities (E+W, 331)
#   data/uk/iran_by_utla155.csv    -- Upper-tier local authorities (E+W, 174)
#   data/uk/iran_top_las_2021.csv  -- Already-tidy top-20 ranking
#   data/uk/iran_uk_summary_2021.csv -- UK nations summary (E/W/S/NI + UK total)
#   data/uk/la_to_region_lookup.csv -- LAD -> Region (9 English + S + W + NI)
#
# Output:
#   data/uk/uk_la_clean.csv         -- la_name, la_code, iran_born (331 LADs)
#   data/uk/uk_utla_clean.csv       -- la_name, la_code, iran_born (174 UTLAs)
#   data/uk/uk_region_iran.csv      -- region_code, region_name, iran_born
#                                       (11 regions: 9 English + Scotland + Wales)
#
# Run from the deployment repo root:
#   Rscript R/uk_export/clean_nomis_data.R

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

clean_one <- function(in_file, out_file, label) {
  d <- read_csv(in_file, show_col_types = FALSE, progress = FALSE)
  out <- d %>%
    transmute(
      la_name   = GEOGRAPHY_NAME,
      la_code   = GEOGRAPHY_CODE,
      iran_born = as.integer(OBS_VALUE)
    ) %>%
    filter(!is.na(iran_born)) %>%
    arrange(desc(iran_born))
  write_csv(out, out_file)
  cat(sprintf("  %s: %d rows, top = %s (%s)\n",
    label, nrow(out), out$la_name[1], format(out$iran_born[1], big.mark = ",")))
  invisible(out)
}

cat("Cleaning UK NOMIS data...\n")
dir.create("data/uk", showWarnings = FALSE, recursive = TRUE)

la_clean <- clean_one(
  "data/uk/iran_by_lad154.csv",
  "data/uk/uk_la_clean.csv",
  "LAD (lower-tier)"
)

clean_one(
  "data/uk/iran_by_utla155.csv",
  "data/uk/uk_utla_clean.csv",
  "UTLA (upper-tier)"
)

# Aggregate the 331 England+Wales LADs into 9 English regions + Wales using
# the drkane LAD->region lookup, then append Scotland from the summary file.
lookup <- read_csv("data/uk/la_to_region_lookup.csv", show_col_types = FALSE,
                   progress = FALSE)
summary_df <- read_csv("data/uk/iran_uk_summary_2021.csv",
                       show_col_types = FALSE, progress = FALSE)

# The lookup file has Wales LADs with RGNCD=NA but RGNNM=NA too; fill those
# with a synthetic "Wales" region so the aggregation rolls them up.
lookup$RGNCD <- ifelse(is.na(lookup$RGNCD) & grepl("^W", lookup$LADCD),
                       "W92000004", lookup$RGNCD)
lookup$RGNNM <- ifelse(is.na(lookup$RGNNM) & grepl("^W", lookup$LADCD),
                       "Wales", lookup$RGNNM)

# Note: ONS rounds local-authority counts to protect disclosure, so the
# sum of LAD-aggregated regions differs from the published E+W total by
# about +/-6. This is source-level non-additivity, not a join bug.
# Do not rescale the regional counts to match the national total —
# that would hide real noise behind false precision.
regions <- la_clean %>%
  left_join(lookup %>% select(la_code = LADCD, region_code = RGNCD,
                              region_name = RGNNM),
            by = "la_code") %>%
  filter(!is.na(region_code)) %>%
  group_by(region_code, region_name) %>%
  summarise(iran_born = sum(iran_born, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(iran_born))

# Append Scotland (separate 2022 census). Northern Ireland is left out of
# the map because the GB regions GeoJSON does not include it; the UK page
# footer mentions its count (461) in text.
scotland_n <- summary_df$iran_born[summary_df$country == "Scotland"]
regions <- bind_rows(
  regions,
  tibble(region_code = "S92000003", region_name = "Scotland",
         iran_born = scotland_n)
) %>% arrange(desc(iran_born))

write_csv(regions, "data/uk/uk_region_iran.csv")
cat(sprintf("  Regions (aggregated): %d rows, top = %s (%s)\n",
  nrow(regions), regions$region_name[1],
  format(regions$iran_born[1], big.mark = ",")))
cat("Done.\n")

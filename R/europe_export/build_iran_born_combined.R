# Build the combined Iran-born-in-Europe dataset for the overview page.
#
# Sources:
#   1. Eurostat migr_pop3ctb (population by country of birth) for 11 countries,
#      1998-2025. Extracted separately via _data/eurostat/extract_eurostat_iran.py
#      and imported here as iran_born_eurostat.csv.
#   2. Germany Mikrozensus 2024 Erstergebnisse (Destatis, Table 12211-53) —
#      used instead of Eurostat because Eurostat doesn't publish Iran-born for
#      Germany. The Mikrozensus figure is the authoritative national source.
#   3. UK ONS Census 2021 (England+Wales) + Scotland Census 2022 + NI — used
#      because the UK is not in Eurostat's reporting countries after Brexit.
#
# Output: data/europe/iran_born_combined.csv
#   columns: geo, country, year, value, source
#   One long-format table suitable for both bar chart (latest year) and
#   time series (full history).
#
# Run from the deployment repo root:
#   Rscript R/europe_export/build_iran_born_combined.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# Paths
eurostat_csv <- "../_data/eurostat/iran_born_eurostat.csv"
out_csv      <- "data/europe/iran_born_combined.csv"

# Load Eurostat ------------------------------------------------------------
euro <- read_csv(eurostat_csv, show_col_types = FALSE) %>%
  mutate(source = "Eurostat migr_pop3ctb")

# Germany (Mikrozensus 2024) -----------------------------------------------
# 250,000 = first-generation Iran-born, including naturalized Germans.
# Matches what the de-population page displays. The Mikrozensus has been
# running annually since 1957, but only the 2024 edition is on the dashboard
# (earlier editions are available at GESIS but are not needed for the overview).
de_data <- tibble(
  geo = "DE",
  country = "Germany",
  year = 2024,
  value = 250000,
  source = "Destatis Mikrozensus 2024, Table 12211-53"
)

# UK (ONS Census 2021 + Scotland Census 2022 + NI 2021) --------------------
# 114,432 = sum of Iran-born across the four UK nations.
#   England 106,801 + Wales 2,367 + Scotland 4,803 + NI 461 = 114,432
uk_data <- tibble(
  geo = "UK",
  country = "United Kingdom",
  year = 2021,
  value = 114432,
  source = "ONS Census 2021 (E+W) + Scotland 2022 + NI 2021"
)

# Combine ------------------------------------------------------------------
combined <- bind_rows(euro, de_data, uk_data) %>%
  arrange(geo, year)

# Write --------------------------------------------------------------------
dir.create("data/europe", showWarnings = FALSE, recursive = TRUE)
write_csv(combined, out_csv)
cat(sprintf("Wrote %s (%d rows, %d countries)\n",
            out_csv, nrow(combined), length(unique(combined$geo))))

# Quick summary of the latest year per country
latest <- combined %>%
  group_by(geo, country) %>%
  slice_max(year, n = 1) %>%
  arrange(desc(value))
cat("\nLatest Iran-born by country:\n")
print(latest, n = 20)

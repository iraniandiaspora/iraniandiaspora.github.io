# Build the combined Iran-born-in-Europe dataset for the overview page.
#
# Sources:
#   1. Eurostat migr_pop3ctb (population by country of birth) for 11 countries,
#      1998-2025. Extracted separately via pipelines/europe/extract_eurostat_iran.py
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
# France is dropped from the Eurostat series and replaced with INSEE below —
# INSEE "immigré" definition differs from Eurostat foreign-born, and INSEE
# publishes a denser annual series (2006-2019) for France specifically.
# Excluded geos:
#   FR  - handled below with INSEE (different "immigré" definition)
#   TR  - Türkiye is its own top-level tab, not in Europe
#   UK  - added below from the 2021/22 census; Eurostat only has stale pre-Brexit UK
#   LI  - Liechtenstein microstate (n=22), too small to map
#   DK  - added below from the DST register; Eurostat's Denmark series ends 2022
#   CH  - added below from the BFS register; Eurostat's Switzerland series ends
#         2021 and is the SAME series shifted one year (Eurostat 1-Jan labels)
# Recency filter (max year >= 2020) drops countries whose only Eurostat data is
# stale single-year snapshots: Ireland (2011) and Poland (2009). The surviving
# set is the 9 established countries plus the 11 smaller reporters added
# 2026-05-31 so visitors from those countries can find themselves on the map:
# BG, CZ, EE, HU, IS, LT, LU, LV, RO, SI, SK.
euro <- read_csv(eurostat_csv, show_col_types = FALSE) %>%
  filter(!geo %in% c("FR", "TR", "UK", "LI", "DK", "CH")) %>%
  group_by(geo) %>%
  filter(max(year) >= 2020) %>%
  ungroup() %>%
  mutate(source = "Eurostat migr_pop3ctb")

# France (INSEE Recensement de la population) ------------------------------
# Annual "immigrés" series 2006-2017 from pays_naissance_detaille_2017.xlsx
# (INSEE table 4510549), plus 2019 revised snapshot from asie_pays_naissance_2019
# (INSEE table 6478089). 2018 is not published as an Iran-specific figure in
# these INSEE tables; the gap is rendered as a line break in the France page
# chart. Values are INSEE "immigrés" (foreign-born, foreign nationality at
# birth), NOT directly comparable with Eurostat migr_pop3ctb for other
# countries — noted in the Europe overview chart footnote.
fr_trend <- read_csv("data/france/fr_trend.csv", show_col_types = FALSE)
fr_data <- tibble(
  geo = "FR",
  country = "France",
  year = fr_trend$year,
  value = fr_trend$iran_born,
  source = "INSEE Recensement de la population"
)

# Germany (Mikrozensus 2025) -----------------------------------------------
# 267,000 = first-generation Iran-born (mit eigener Migrationserfahrung),
# including naturalized Germans. Matches what the de-population page displays.
# The Mikrozensus has been running annually since 1957, but only the latest
# edition is on the dashboard (earlier editions are available at GESIS but are
# not needed for the overview).
de_data <- tibble(
  geo = "DE",
  country = "Germany",
  year = 2025,
  value = 267000,
  source = "Destatis Mikrozensus 2025, Table 12211-53"
)

# Denmark (Statistics Denmark register) -------------------------------------
# Eurostat's Denmark Iran-born series ends at 2022: the live migr_pop3ctb API
# returns nulls for DK 2023-2025 (verified 2026-08-02 —
# curl '.../migr_pop3ctb?format=JSON&c_birth=IR&sex=T&age=TOTAL&geo=DK&sinceTimePeriod=2020').
# DST publishes annually, so Denmark uses the national register instead, same
# pattern as DE/UK/FR. dk_trend.csv gen1 = immigrants ("indvandrere") of
# Iranian origin; for immigrants DST origin is in practice own country of
# birth, and the two series agree within ~1.2% in every overlap year
# 2011-2022 (2022: DST 18,044 vs Eurostat 17,838). The WHOLE series comes
# from DST (1998+, matching the Eurostat window) rather than splicing, so the
# line keeps one definition and matches the dk-population page.
dk_trend <- read_csv("data/denmark/dk_trend.csv", show_col_types = FALSE) %>%
  filter(year >= 1998)
dk_data <- tibble(
  geo = "DK",
  country = "Denmark",
  year = dk_trend$year,
  value = dk_trend$gen1,
  source = "Statistics Denmark (DST) population register"
)

# Switzerland (BFS STATPOP register) ----------------------------------------
# Eurostat's Switzerland series ends at 2021 and is the SAME series as BFS
# STATPOP with labels shifted one year — Eurostat's 1-Jan-Y figure equals the
# BFS end-of-(Y-1) stock, digit-for-digit in all 11 overlap years (verified
# 2026-08-02: join of data/switzerland/ch_trend.csv vs the old Eurostat CH
# rows; e.g. BFS 2020 = Eurostat 2021 = 14,241). Using the full ch_trend.csv series
# keeps one labeling convention, matches the ch-population page, and makes the
# time-chart BFS attribution true — the footnote already
# claimed BFS while the data was still the stale Eurostat rows.
ch_trend <- read_csv("data/switzerland/ch_trend.csv", show_col_types = FALSE)
ch_data <- tibble(
  geo = "CH",
  country = "Switzerland",
  year = ch_trend$year,
  value = ch_trend$total,
  source = "BFS STATPOP register"
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
combined <- bind_rows(euro, de_data, dk_data, ch_data, uk_data, fr_data) %>%
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

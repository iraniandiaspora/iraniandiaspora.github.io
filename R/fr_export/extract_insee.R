# Extract France Iran-born data from INSEE published xlsx files.
# Run from deployment repo root:
#   Rscript R/fr_export/extract_insee.R
#
# Inputs (in ../_data/france/insee/):
#   pays_naissance_detaille_2017.xlsx  - annual series 2006-2017
#   asie_pays_naissance_2019.xlsx      - 2019, 2014, 2009 snapshots
#
# Outputs (written to data/france/):
#   fr_trend.csv     - year, iran_born (INSEE immigré definition)
#   fr_headline.csv  - year, count (latest reference year)
#
# Definition note: INSEE "immigré" = person born abroad with foreign
# nationality at birth. Not directly comparable with Eurostat migr_pop3ctb
# (which counts all foreign-born regardless of nationality), so we stay
# within the INSEE series for the trend.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

OUT_DIR <- "data/france"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

INSEE_2017 <- "../_data/france/insee/pays_naissance_detaille_2017.xlsx"
INSEE_2019 <- "../_data/france/insee/asie_pays_naissance_2019.xlsx"

# --- 1. Annual series 2006-2017 from INSEE 2017 file --------------------------
cat("Reading INSEE 2017 file (annual 2006-2017)...\n")
d17 <- suppressMessages(read_excel(INSEE_2017, sheet = 1, col_names = FALSE))

# Header year row is row 3, columns 2 onward. Iran row contains "Iran" in col 1.
year_row <- as.integer(unlist(d17[3, -1]))
iran_idx <- which(apply(d17, 1, function(r) any(grepl("^Iran", r, ignore.case = TRUE))))
stopifnot(length(iran_idx) == 1)
iran_vals <- suppressWarnings(as.integer(unlist(d17[iran_idx, -1])))

trend_2017 <- tibble(year = year_row, iran_born = iran_vals) %>%
  filter(!is.na(year), !is.na(iran_born)) %>%
  arrange(year)
cat(sprintf("  %d rows (%d-%d)\n", nrow(trend_2017),
            min(trend_2017$year), max(trend_2017$year)))

# --- 2. 2019 snapshot from INSEE 2019 file ------------------------------------
cat("Reading INSEE 2019 file (2019 headline + 2014, 2009)...\n")
d19 <- suppressMessages(read_excel(INSEE_2019, sheet = 1, col_names = FALSE))
year_row_19 <- as.integer(unlist(d19[3, -1]))
iran_idx_19 <- which(apply(d19, 1, function(r) any(grepl("^Iran", r, ignore.case = TRUE))))
stopifnot(length(iran_idx_19) == 1)
iran_vals_19 <- suppressWarnings(as.integer(unlist(d19[iran_idx_19, -1])))
snap_19 <- tibble(year = year_row_19, iran_born = iran_vals_19) %>%
  filter(!is.na(year), !is.na(iran_born))
print(snap_19)

# --- 3. Combine: take INSEE 2019 file's value for any overlapping year --------
# (the 2019 file reflects later INSEE revisions)
trend <- bind_rows(
  trend_2017 %>% filter(!year %in% snap_19$year),
  snap_19
) %>% arrange(year)

cat("\nFinal France trend:\n")
print(trend)

write.csv(trend, file.path(OUT_DIR, "fr_trend.csv"), row.names = FALSE)
cat(sprintf("Wrote %s (%d rows)\n",
            file.path(OUT_DIR, "fr_trend.csv"), nrow(trend)))

# --- 4. Headline ---------------------------------------------------------------
latest <- trend[which.max(trend$year), ]
headline <- tibble(category = "total",
                   year = latest$year,
                   count = latest$iran_born)
write.csv(headline, file.path(OUT_DIR, "fr_headline.csv"), row.names = FALSE)
cat(sprintf("Headline: %s Iran-born in France (%d)\n",
            format(latest$iran_born, big.mark = ","), latest$year))

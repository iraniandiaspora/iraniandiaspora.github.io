# Extract France Iran-born data from INSEE published xlsx files.
# Run from deployment repo root:
#   Rscript R/fr_export/extract_insee.R
#
# Input (in ../_data/france/insee/):
#   pays_naissance_detaille_1968_2019.xlsx  - INSEE table 6478091
#     "Séries longues depuis 1968", 2019 cycle. Two sheets:
#       - "1968-1999": 5 historical census snapshots
#       - "2006-2019": continuous annual series
#     Pre-2006 censuses (1968, 1975, 1982, 1990, 1999) were periodic;
#     INSEE switched to a rolling annual census in 2004, so the modern
#     series starts at 2006. The gap 1999–2006 is genuine — do not
#     interpolate.
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

INSEE_LONG <- "../_data/france/insee/pays_naissance_detaille_1968_2019.xlsx"

# Generic reader: row 3 has year headers in cols 2..N, Iran row starts with "Iran".
read_iran_row <- function(file, sheet) {
  d <- suppressMessages(read_excel(file, sheet = sheet, col_names = FALSE))
  year_row <- as.integer(unlist(d[3, -1]))
  iran_idx <- which(apply(d, 1, function(r) any(grepl("^Iran", r, ignore.case = TRUE))))
  stopifnot(length(iran_idx) == 1)
  iran_vals <- suppressWarnings(as.integer(unlist(d[iran_idx, -1])))
  tibble(year = year_row, iran_born = iran_vals) %>%
    filter(!is.na(year), !is.na(iran_born))
}

cat("Reading 1968-1999 snapshots...\n")
hist_snap <- read_iran_row(INSEE_LONG, "1968-1999")
print(hist_snap)

cat("Reading 2006-2019 annual series...\n")
recent <- read_iran_row(INSEE_LONG, "2006-2019")

trend <- bind_rows(hist_snap, recent) %>% arrange(year)
cat("\nFinal France trend:\n")
print(trend)

write.csv(trend, file.path(OUT_DIR, "fr_trend.csv"), row.names = FALSE)
cat(sprintf("Wrote %s (%d rows, %d-%d)\n",
            file.path(OUT_DIR, "fr_trend.csv"), nrow(trend),
            min(trend$year), max(trend$year)))

# --- Headline ----------------------------------------------------------------
latest <- trend[which.max(trend$year), ]
headline <- tibble(category = "total",
                   year = latest$year,
                   count = latest$iran_born)
write.csv(headline, file.path(OUT_DIR, "fr_headline.csv"), row.names = FALSE)
cat(sprintf("Headline: %s Iran-born in France (%d)\n",
            format(latest$iran_born, big.mark = ","), latest$year))

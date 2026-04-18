# Extract Finland Iran-born data from the combined Eurostat file.
# Run from deployment repo root:
#   Rscript R/fi_export/extract_eurostat_fi.R
#
# Input:  _data/eurostat/iran_born_eurostat.csv (already widened to include FI)
# Output: data/finland/fi_trend.csv
#         data/finland/fi_headline.csv

suppressPackageStartupMessages({
  library(dplyr)
})

OUT_DIR <- "data/finland"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

euro <- read.csv("../_data/eurostat/iran_born_eurostat.csv", stringsAsFactors = FALSE)
fi <- euro %>%
  filter(geo == "FI") %>%
  transmute(year = as.integer(year), iran_born = as.integer(value)) %>%
  arrange(year)

write.csv(fi, file.path(OUT_DIR, "fi_trend.csv"), row.names = FALSE)
latest <- fi[which.max(fi$year), ]
write.csv(data.frame(category = "total", year = latest$year, count = latest$iran_born),
          file.path(OUT_DIR, "fi_headline.csv"), row.names = FALSE)

cat(sprintf("Finland: %s Iran-born (%d), %d-year series\n",
            format(latest$iran_born, big.mark = ","),
            latest$year, nrow(fi)))

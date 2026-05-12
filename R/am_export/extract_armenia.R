# Extract Armenia Iran-born data.
# Run from deployment repo root:
#   Rscript R/am_export/extract_armenia.R
#
# Sources:
#   - UN DESA International Migrant Stock 2024 (data/global/stocks_countries.csv)
#     gives 5-yearly Armenia × Iran-origin stock 1990-2024.
#   - Armstat 2022 Population Census, Chapter 6 (Migration), Table 6.6
#     (place of birth) for the 2022 census-day Iran-born figure.
#
# Outputs (data/armenia/):
#   am_trend.csv     - year, iran_born, source (UN snapshots + Armstat 2022)
#   am_headline.csv  - category, year, count (Armstat 2022 census)
#
# Definition note: UN Migrant Stock is interpolated/estimated; Armstat is
# the official 2022 census count. The two diverge slightly (UN 2020 = 6,065;
# Armstat 2022 = 5,470). We lead the page with the Armstat census figure
# and use UN for the long-running historical series.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

OUT_DIR <- "data/armenia"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 1. UN time series 1990-2024 ----------------------------------------------
un_csv <- read.csv("data/global/stocks_countries.csv", stringsAsFactors = FALSE,
                   check.names = FALSE)
arm_row <- un_csv[un_csv$destination == "Armenia" &
                  grepl("^Iran", un_csv$origin), ]
stopifnot(nrow(arm_row) == 1)

un_trend <- arm_row %>%
  select(starts_with("X")) %>%
  pivot_longer(everything(), names_to = "year", values_to = "iran_born") %>%
  mutate(year = as.integer(sub("^X", "", year)),
         source = "UN Migrant Stock") %>%
  arrange(year)

# --- 2. Armstat 2022 census Iran-born ----------------------------------------
armstat_2022 <- tibble(year = 2022L, iran_born = 5470L,
                       source = "Armstat Census 2022")

trend <- bind_rows(un_trend, armstat_2022) %>% arrange(year, source)
write.csv(trend, file.path(OUT_DIR, "am_trend.csv"), row.names = FALSE)
cat("UN + Armstat trend:\n")
print(trend)

# --- 3. Headline (Armstat census) --------------------------------------------
# Iranian citizens (from Armstat Table 6.4) reported separately for context.
headline <- tibble(
  category = c("iran_born", "iranian_citizens"),
  year     = c(2022L, 2022L),
  count    = c(5470L, 2333L)
)
write.csv(headline, file.path(OUT_DIR, "am_headline.csv"), row.names = FALSE)
cat(sprintf("\nArmenia: %s Iran-born (Armstat 2022); UN 2024 = %s\n",
            format(5470, big.mark = ","),
            format(arm_row$X2024, big.mark = ",")))

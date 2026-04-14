# Extract Iranian-origin population data from CBS Statistical Abstract Table 2.6.
# Run from deployment repo root:
#   Rscript R/il_export/extract_cbs_israel.R
#
# Input:  ../_data/israel/cbs_statistical_abstract/st02_06x_2025.xlsx
# Output: data/israel/il_headline.csv
#         data/israel/il_age.csv
#         data/israel/il_comparison.csv

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

RAW_DIR <- "../_data/israel/cbs_statistical_abstract"
OUT_DIR <- "data/israel"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Read xlsx ----------------------------------------------------------------
f <- file.path(RAW_DIR, "st02_06x_2025.xlsx")
cat("Reading", f, "...\n")
d <- read_excel(f, sheet = 1, col_names = FALSE)

# Column mapping (from header rows 3-5):
# Col 1:  Country name
# Cols 2-9:  Born abroad age groups: 75+, 65-74, 55-64, 45-54, 35-44, 25-34, 15-24, 0-14
# Col 10: Born abroad Total
# Cols 11-22: Israeli-born age groups: 55+, 50-54, 45-49, 40-44, 35-39, 30-34, 25-29, 20-24, 15-19, 10-14, 5-9, 0-4
# Col 23: Israeli-born Total
# Col 24: Grand Total
# Col 25: Hebrew name
# Values are in THOUSANDS.

abroad_ages <- c("75+", "65-74", "55-64", "45-54", "35-44", "25-34", "15-24", "0-14")
israel_ages <- c("55+", "50-54", "45-49", "40-44", "35-39", "30-34", "25-29", "20-24", "15-19", "10-14", "5-9", "0-4")

# --- Extract Iran row (row 11) -----------------------------------------------
iran <- d[11, ]
stopifnot(grepl("Iran", as.character(iran[[1]])))

gen1_total <- as.numeric(iran[[10]]) * 1000  # born abroad total
gen2_total <- as.numeric(iran[[23]]) * 1000  # Israeli-born total
grand_total <- as.numeric(iran[[24]]) * 1000

cat(sprintf("Iran-born (1st gen): %s\n", format(gen1_total, big.mark = ",")))
cat(sprintf("Israeli-born (2nd gen): %s\n", format(gen2_total, big.mark = ",")))
cat(sprintf("Total: %s\n", format(grand_total, big.mark = ",")))

# --- il_headline.csv ----------------------------------------------------------
headline <- data.frame(
  category = c("total", "gen1", "gen2"),
  count = c(grand_total, gen1_total, gen2_total),
  year = 2024,
  source = "CBS Statistical Abstract, 76th edition, Table 2.6"
)
write.csv(headline, file.path(OUT_DIR, "il_headline.csv"), row.names = FALSE)
cat("  Wrote il_headline.csv\n")

# --- il_age.csv ---------------------------------------------------------------
# Born abroad: 8 age groups (cols 2-9)
gen1_counts <- as.numeric(iran[2:9]) * 1000
# Israeli-born: 12 age groups (cols 11-22)
gen2_counts <- as.numeric(iran[11:22]) * 1000

age_df <- rbind(
  data.frame(generation = "Iran-born", age_group = abroad_ages, count = gen1_counts,
             stringsAsFactors = FALSE),
  data.frame(generation = "Israeli-born", age_group = israel_ages, count = gen2_counts,
             stringsAsFactors = FALSE)
)

# Also create harmonized 10-year bins for the grouped chart
harmonized <- data.frame(
  age_group = c("55+", "45-54", "35-44", "25-34", "15-24", "0-14"),
  gen1 = c(
    sum(gen1_counts[abroad_ages %in% c("75+", "65-74", "55-64")]),  # 55+
    gen1_counts[abroad_ages == "45-54"],
    gen1_counts[abroad_ages == "35-44"],
    gen1_counts[abroad_ages == "25-34"],
    gen1_counts[abroad_ages == "15-24"],
    gen1_counts[abroad_ages == "0-14"]
  ),
  gen2 = c(
    gen2_counts[israel_ages == "55+"],                               # 55+
    sum(gen2_counts[israel_ages %in% c("50-54", "45-49")]),          # 45-54
    sum(gen2_counts[israel_ages %in% c("40-44", "35-39")]),          # 35-44
    sum(gen2_counts[israel_ages %in% c("30-34", "25-29")]),          # 25-34
    sum(gen2_counts[israel_ages %in% c("20-24", "15-19")]),          # 15-24
    sum(gen2_counts[israel_ages %in% c("10-14", "5-9", "0-4")])     # 0-14
  ),
  stringsAsFactors = FALSE
)

write.csv(age_df, file.path(OUT_DIR, "il_age_detail.csv"), row.names = FALSE)
write.csv(harmonized, file.path(OUT_DIR, "il_age.csv"), row.names = FALSE)
cat("  Wrote il_age_detail.csv and il_age.csv\n")

# --- il_comparison.csv --------------------------------------------------------
# Asian-origin groups from rows 8-13
comp_rows <- list(
  list(row = 9,  name = "Iraq"),
  list(row = 11, name = "Iran"),
  list(row = 10, name = "Yemen"),
  list(row = 8,  name = "T\u00fcrkiye"),
  list(row = 12, name = "India and Pakistan"),
  list(row = 13, name = "Syria and Lebanon")
)

comp_df <- do.call(rbind, lapply(comp_rows, function(r) {
  row <- d[r$row, ]
  data.frame(
    country = r$name,
    gen1 = as.numeric(row[[10]]) * 1000,
    gen2 = as.numeric(row[[23]]) * 1000,
    total = as.numeric(row[[24]]) * 1000,
    stringsAsFactors = FALSE
  )
}))
# Sort by total descending
comp_df <- comp_df[order(-comp_df$total), ]

write.csv(comp_df, file.path(OUT_DIR, "il_comparison.csv"), row.names = FALSE)
cat("  Wrote il_comparison.csv\n")

# --- Born-abroad age detail for text card -------------------------------------
pct_55plus <- round(sum(gen1_counts[abroad_ages %in% c("75+", "65-74", "55-64")]) / gen1_total * 100)
cat(sprintf("\nKey stat: %d%% of Iran-born are aged 55+ (aging-out cohort)\n", pct_55plus))

cat("\nDone. Files written to", OUT_DIR, "\n")

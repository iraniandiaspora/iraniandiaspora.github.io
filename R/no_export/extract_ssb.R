# Extract Iran-origin population data from SSB (Statistics Norway) API CSVs.
#
# Input:  ../_data/norway/ssb_api/*.csv (Dropbox raw data)
# Output: data/norway/*.csv (deployment repo, in git)
#
# Run from deployment repo root:
#   Rscript R/no_export/extract_ssb.R

SSB_DIR <- "../_data/norway/ssb_api"
OUT_DIR <- "data/norway"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Extracting Norway SSB data...\n")

# ---- 1. Headline (from iran_all.csv, latest year) ----
all_data <- read.csv(file.path(SSB_DIR, "iran_all.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE,
                     fileEncoding = "latin1")
# Columns: region, immigration category, country background, Persons YYYY...
# National row: "0 The whole country"
nat <- all_data[all_data$region == "0 The whole country", ]

year_cols <- grep("^Persons", names(nat), value = TRUE)
years <- as.integer(sub("Persons ", "", year_cols))
latest_col <- year_cols[which.max(years)]
latest_yr <- max(years)

total <- as.integer(nat[[latest_col]][nat[["immigration category"]] ==
  "Immigrants and Norwegian-born to immigrant parents"])
gen1 <- as.integer(nat[[latest_col]][nat[["immigration category"]] == "Immigrants"])
gen2 <- as.integer(nat[[latest_col]][nat[["immigration category"]] ==
  "Norwegian-born to immigrant parents"])

headline <- data.frame(
  category = c("total", "gen1", "gen2"),
  label = c("Total Iranian-origin",
            "Immigrants (born in Iran)",
            "Norwegian-born to immigrant parents"),
  count = c(total, gen1, gen2),
  year = latest_yr,
  stringsAsFactors = FALSE
)
write.csv(headline, file.path(OUT_DIR, "no_headline.csv"), row.names = FALSE)
cat("  no_headline.csv\n")

# ---- 2. Generational trend 2010-2026 (from iran_all.csv national row) ----
gen_trend <- data.frame(year = years, stringsAsFactors = FALSE)
gen_trend$gen1 <- as.integer(nat[nat[["immigration category"]] == "Immigrants", year_cols])
gen_trend$gen2 <- as.integer(nat[nat[["immigration category"]] ==
  "Norwegian-born to immigrant parents", year_cols])
gen_trend$total <- gen_trend$gen1 + gen_trend$gen2
gen_trend <- gen_trend[order(gen_trend$year), ]
row.names(gen_trend) <- NULL
write.csv(gen_trend, file.path(OUT_DIR, "no_trend.csv"), row.names = FALSE)
cat("  no_trend.csv\n")

# ---- 3. Historical sex time series 1970-2026 (from iran_sex_ts) ----
sex_ts <- read.csv(file.path(SSB_DIR, "iran_sex_ts_1970_2025.csv"),
                   stringsAsFactors = FALSE, check.names = FALSE,
                   fileEncoding = "UTF-8")
# Wide format: sex, country, Immigrants YYYY...
imm_cols <- grep("^Immigrants", names(sex_ts), value = TRUE)
ts_years <- as.integer(sub("Immigrants ", "", imm_cols))

female_row <- sex_ts$sex == "Females"
male_row <- sex_ts$sex == "Males"

hist_trend <- data.frame(
  year = ts_years,
  female = as.integer(sex_ts[female_row, imm_cols]),
  male = as.integer(sex_ts[male_row, imm_cols]),
  stringsAsFactors = FALSE
)
hist_trend$total <- hist_trend$female + hist_trend$male
hist_trend <- hist_trend[order(hist_trend$year), ]
row.names(hist_trend) <- NULL
write.csv(hist_trend, file.path(OUT_DIR, "no_hist_sex.csv"), row.names = FALSE)
cat("  no_hist_sex.csv\n")

# ---- 4. County-level counts (2026, from iran_by_region_2026.csv) ----
.reg_lines <- readLines(file.path(SSB_DIR, "iran_by_region_2026.csv"),
                        encoding = "latin1", warn = FALSE)
reg <- read.csv(textConnection(.reg_lines),
                stringsAsFactors = FALSE, check.names = FALSE)
names(reg) <- c("region", "category", "country", "count")
reg$count <- as.integer(reg$count)

# County rows: 2-digit codes (31-56, plus 03 Oslo); municipalities are 4-digit
# Only take "Immigrants and Norwegian-born to immigrant parents" (total)
reg_total <- reg[reg$category == "Immigrants and Norwegian-born to immigrant parents", ]
reg_total$code <- sub(" .*", "", reg_total$region)

# County-level: 2-digit codes (not "0" national, not 4-digit municipalities,
# not old codes with 0 count)
county_mask <- nchar(reg_total$code) == 2 & reg_total$code != "0" & reg_total$count > 0
counties <- reg_total[county_mask, ]

# Handle old county codes (pre-2020 reform): codes like "01", "02" etc. have 0 count
# Current counties: 03 (Oslo), 11, 15, 18, 31-56
counties$county_name <- sub("^[0-9]+ ", "", counties$region)
# Remove parenthetical date ranges from old county names
counties$county_name <- sub(" \\(.*\\)$", "", counties$county_name)
counties <- counties[order(-counties$count), c("code", "county_name", "count")]
names(counties)[1] <- "county_code"
row.names(counties) <- NULL
write.csv(counties, file.path(OUT_DIR, "no_county.csv"), row.names = FALSE)
cat(sprintf("  no_county.csv (%d counties)\n", nrow(counties)))

# ---- 5. Employment rate time series (2001-2025) ----
emp <- read.csv(file.path(SSB_DIR, "iran_employment_timeseries.csv"),
                stringsAsFactors = FALSE, check.names = FALSE,
                fileEncoding = "UTF-8")
# Wide: sex, age, country, years of residence, Employed YYYY..., Employed pct YYYY...

emp_cols <- grep("^Employed immigrants 20", names(emp), value = TRUE)
pct_cols <- grep("^Employed immigrants, per cent", names(emp), value = TRUE)
emp_years <- as.integer(sub("Employed immigrants ", "", emp_cols))
pct_years <- as.integer(sub(".*total ", "", pct_cols))

# Total (both sexes) row
both <- emp[emp$sex == "Both sexes", ]
female <- emp[emp$sex == "Females", ]
male <- emp[emp$sex == "Males", ]

employment <- data.frame(
  year = emp_years,
  employed_total = as.integer(both[, emp_cols]),
  pct_total = as.numeric(both[, pct_cols]),
  employed_female = as.integer(female[, emp_cols]),
  pct_female = as.numeric(female[, pct_cols]),
  employed_male = as.integer(male[, emp_cols]),
  pct_male = as.numeric(male[, pct_cols]),
  stringsAsFactors = FALSE
)
employment <- employment[order(employment$year), ]
row.names(employment) <- NULL
write.csv(employment, file.path(OUT_DIR, "no_employment.csv"), row.names = FALSE)
cat("  no_employment.csv\n")

cat(sprintf("\nNorway: %s Iranian-origin (%s immigrants + %s Norwegian-born, %d)\n",
  format(total, big.mark = ","), format(gen1, big.mark = ","),
  format(gen2, big.mark = ","), latest_yr))
cat("Done.\n")

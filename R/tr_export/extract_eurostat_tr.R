# Extract Türkiye Iran-born and Iranian-citizen data from cached Eurostat JSONs.
# Run from deployment repo root:
#   Rscript R/tr_export/extract_eurostat_tr.R
#
# Inputs (cached in _data/turkey/eurostat/):
#   iran_born_total.json       - Iran-born, T sex, TOTAL age, 2019-2025
#   iran_born_male.json        - Iran-born, M sex, TOTAL age, same years
#   iran_born_female.json      - same, F sex
#   iran_born_age_male_2025.json   - Iran-born, M sex, all age bins, 2025
#   iran_born_age_female_2025.json - same, F sex
#   iran_citizens_total.json   - Iranian passport holders, 2014-2025
#
# Outputs (to data/turkey/):
#   tr_trend.csv       - year, total, male, female
#   tr_age_2025.csv    - age_group, age_mid, male, female
#   tr_citizens.csv    - year, iranian_citizens
#   tr_headline.csv    - category, year, count (latest total + sex split)

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
})

OUT_DIR <- "data/turkey"
IN_DIR  <- "../_data/turkey/eurostat"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Helper: pull (year, value) from a simple 1-geo, 1-age, 1-sex JSON-stat.
extract_time_series <- function(path) {
  d <- read_json(path)
  time_idx <- d$dimension$time$category$index
  values <- d$value
  years <- integer(0); vals <- integer(0)
  for (yr in names(time_idx)) {
    ti <- time_idx[[yr]]
    v <- values[[as.character(ti)]]
    if (!is.null(v)) {
      years <- c(years, as.integer(yr))
      vals  <- c(vals,  as.integer(v))
    }
  }
  data.frame(year = years, value = vals)[order(years), ]
}

# Helper: pull age breakdown from a single-year (sex fixed) JSON-stat.
# Age categories are e.g. Y_LT5, Y5-9, ..., Y_GE85.
extract_age_snapshot <- function(path, age_codes) {
  d <- read_json(path)
  age_idx <- d$dimension$age$category$index
  values <- d$value
  out <- data.frame(age = character(0), value = integer(0),
                    stringsAsFactors = FALSE)
  for (ac in age_codes) {
    ai <- age_idx[[ac]]
    if (!is.null(ai)) {
      v <- values[[as.character(ai)]]
      if (!is.null(v)) {
        out <- rbind(out, data.frame(age = ac, value = as.integer(v),
                                     stringsAsFactors = FALSE))
      }
    }
  }
  out
}

# --- 1. Annual trend + sex split -----------------------------------------------
# Combines Eurostat migr_pop3ctb (2019, 2020, 2023, 2024, 2025) with a TÜİK
# Nüfus Portalı supplement for the 2021/2022 reporting gap. See
# ../_data/turkey/tuik/README.md for the year-label convention shift.
cat("Extracting Iran-born stock by sex (Eurostat + TÜİK supplement)...\n")
tot <- extract_time_series(file.path(IN_DIR, "iran_born_total.json"))
mal <- extract_time_series(file.path(IN_DIR, "iran_born_male.json"))
fem <- extract_time_series(file.path(IN_DIR, "iran_born_female.json"))
names(tot)[2] <- "total"
names(mal)[2] <- "male"
names(fem)[2] <- "female"
trend <- tot %>% left_join(mal, by = "year") %>% left_join(fem, by = "year") %>%
  mutate(source = "eurostat")

tuik_born <- read.csv("../_data/turkey/tuik/iran_born_supplement.csv",
                     stringsAsFactors = FALSE) %>%
  select(year, total, male, female) %>%
  mutate(source = "tuik")

trend <- bind_rows(trend, tuik_born) %>%
  arrange(year)
write.csv(trend, file.path(OUT_DIR, "tr_trend.csv"), row.names = FALSE)
print(trend)

# --- 2. Iranian citizens (passport holders) -----------------------------------
cat("\nExtracting Iranian citizens stock (Eurostat + TÜİK supplement)...\n")
cit <- extract_time_series(file.path(IN_DIR, "iran_citizens_total.json"))
names(cit)[2] <- "iranian_citizens"
cit <- cit %>% mutate(source = "eurostat")

tuik_cit <- read.csv("../_data/turkey/tuik/iran_citizens_supplement.csv",
                     stringsAsFactors = FALSE) %>%
  select(year, iranian_citizens) %>%
  mutate(source = "tuik")

cit <- bind_rows(cit, tuik_cit) %>% arrange(year)
write.csv(cit, file.path(OUT_DIR, "tr_citizens.csv"), row.names = FALSE)
print(cit)

# --- 3. Age × sex, 2025 --------------------------------------------------------
cat("\nExtracting age breakdown 2025...\n")
# Eurostat 5-year bins that cover the full distribution without overlap
AGE_CODES <- c("Y_LT5", "Y5-9", "Y10-14", "Y15-19", "Y20-24", "Y25-29",
               "Y30-34", "Y35-39", "Y40-44", "Y45-49", "Y50-54", "Y55-59",
               "Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y_GE85")
# Human labels for the chart axis
AGE_LABELS <- c("Under 5", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
age_mid <- c(2.5, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72,
             77, 82, 90)

mage <- extract_age_snapshot(file.path(IN_DIR, "iran_born_age_male_2025.json"),
                             AGE_CODES)
fage <- extract_age_snapshot(file.path(IN_DIR, "iran_born_age_female_2025.json"),
                             AGE_CODES)
age_df <- data.frame(
  age_code = AGE_CODES,
  age_label = AGE_LABELS,
  age_mid = age_mid,
  male = mage$value[match(AGE_CODES, mage$age)],
  female = fage$value[match(AGE_CODES, fage$age)],
  stringsAsFactors = FALSE
)
write.csv(age_df, file.path(OUT_DIR, "tr_age_2025.csv"), row.names = FALSE)
print(age_df)

# --- 4. Headline ---------------------------------------------------------------
latest <- trend[which.max(trend$year), ]
headline <- data.frame(
  category = c("total", "male", "female"),
  year = latest$year,
  count = c(latest$total, latest$male, latest$female)
)
write.csv(headline, file.path(OUT_DIR, "tr_headline.csv"), row.names = FALSE)
cat(sprintf("\nTürkiye: %s Iran-born (%d): %s male / %s female\n",
            format(latest$total, big.mark = ","),
            latest$year,
            format(latest$male, big.mark = ","),
            format(latest$female, big.mark = ",")))

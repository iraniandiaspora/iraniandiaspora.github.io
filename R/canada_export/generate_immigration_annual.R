# generate_immigration_annual.R
# Produces: output_immigration_page/immigration_annual.csv
# Annual/period immigration counts for Iranian-Canadians from PUMF.
# Pre-1995 data is grouped into 5-year periods (spread evenly across years).
# Post-1994 data is annual.

library(tidyverse)
library(here)
source(here("R/comprehensive_iranian_variables_correct_pumf.R"))

cat("=== Generating immigration_annual.csv ===\n")

vars <- c("pob", "ethder", "mtnno", "hlmostno", "weight", "yrim")
individual_data <- load_individual_pumf(vars)

iranians <- individual_data %>%
  mutate(
    born_in_iran = (pob == 19),
    iranian_ethnic = (ethder == 38),
    persian_mt = (mtnno == 11),
    persian_home = (hlmostno == 15)
  ) %>%
  filter(born_in_iran | iranian_ethnic | persian_mt | persian_home) %>%
  filter(yrim > 0, yrim < 8888)  # Valid immigration years only (exclude 8888/9999)

# PUMF yrim codes: period codes for pre-1995, actual years for 1995+
# Period codes in PUMF: 1=Before 1955, 2=1955-1959, ..., 9=1990-1994
# Annual codes: 1995, 1996, ..., 2020

# Separate period vs annual data
period_data <- iranians %>% filter(yrim <= 20)  # period codes
annual_data <- iranians %>% filter(yrim >= 1995) # actual years

# Map period codes to label, midpoint year, and span
period_map <- tribble(
  ~yrim, ~period_label, ~mid_year, ~period_years,
  1, "Before 1955", 1952, 5,
  2, "1955-1959", 1957, 5,
  3, "1960-1964", 1962, 5,
  4, "1965-1969", 1967, 5,
  5, "1970-1974", 1972, 5,
  6, "1975-1979", 1977, 5,
  7, "1980-1984", 1982, 5,
  8, "1985-1989", 1987, 5,
  9, "1990-1994", 1992, 5
)

period_counts <- period_data %>%
  group_by(yrim) %>%
  summarize(n_unweighted = n(), period_total = sum(weight), .groups = "drop") %>%
  left_join(period_map, by = "yrim") %>%
  mutate(
    count = period_total / period_years,  # average per year
    is_period = TRUE,
    reliability = case_when(
      n_unweighted >= 30 ~ "Reliable",
      n_unweighted >= 10 ~ "Use with caution",
      TRUE ~ "Suppress"
    )
  ) %>%
  select(year = mid_year, count, period_label, is_period, period_years, period_total, n_unweighted, reliability)

annual_counts <- annual_data %>%
  group_by(yrim) %>%
  summarize(n_unweighted = n(), count = sum(weight), .groups = "drop") %>%
  mutate(
    year = yrim,
    period_label = as.character(yrim),
    is_period = FALSE,
    period_years = 1,
    period_total = count,
    reliability = case_when(
      n_unweighted >= 30 ~ "Reliable",
      n_unweighted >= 10 ~ "Use with caution",
      TRUE ~ "Suppress"
    )
  ) %>%
  select(year, count, period_label, is_period, period_years, period_total, n_unweighted, reliability)

immigration_annual <- bind_rows(period_counts, annual_counts) %>%
  arrange(year)

write_csv(immigration_annual, here("output_immigration_page/immigration_annual.csv"))
cat("Saved immigration_annual.csv:", nrow(immigration_annual), "rows\n")
cat("Total weighted immigration:", sum(immigration_annual$period_total), "\n")

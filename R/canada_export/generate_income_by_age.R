# generate_income_by_age.R
# Produces: output_work_page/income_by_age_first_gen.csv
# Income distribution by age group for first-generation Iranian-Canadians.
# Uses total personal income (totinc), grouped into 5 bands, ages 25-74.

library(tidyverse)
library(here)
source(here("R/comprehensive_iranian_variables_correct_pumf.R"))

cat("=== Generating income_by_age_first_gen.csv ===\n")

vars <- c("pob", "ethder", "mtnno", "hlmostno", "weight", "genstat",
           "agegrp", "totinc")
individual_data <- load_individual_pumf(vars)

iranians <- individual_data %>%
  mutate(
    born_in_iran = (pob == 19),
    iranian_ethnic = (ethder == 38),
    persian_mt = (mtnno == 11),
    persian_home = (hlmostno == 15)
  ) %>%
  filter(born_in_iran | iranian_ethnic | persian_mt | persian_home)

# First generation, ages 25-74, valid pre-tax personal income
first_gen <- iranians %>%
  filter(genstat == 1, agegrp >= 9, agegrp <= 18, totinc < 8888888) %>%
  mutate(
    age_group = case_when(
      agegrp %in% c(9, 10) ~ "25-34",
      agegrp %in% c(11, 12) ~ "35-44",
      agegrp %in% c(13, 14) ~ "45-54",
      agegrp %in% c(15, 16) ~ "55-64",
      agegrp %in% c(17, 18) ~ "65-74",
      TRUE ~ NA_character_
    ),
    income_band = case_when(
      totinc < 20000 ~ "Under CA$20k",
      totinc < 40000 ~ "CA$20k-CA$40k",
      totinc < 60000 ~ "CA$40k-CA$60k",
      totinc < 100000 ~ "CA$60k-CA$100k",
      totinc >= 100000 ~ "CA$100k+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group), !is.na(income_band))

income_by_age <- first_gen %>%
  group_by(age_group, income_band) %>%
  summarize(count = sum(weight), .groups = "drop") %>%
  group_by(age_group) %>%
  mutate(pct = round(count / sum(count) * 100, 1)) %>%
  ungroup()

write_csv(income_by_age, here("output_work_page/income_by_age_first_gen.csv"))
cat("Saved income_by_age_first_gen.csv:", nrow(income_by_age), "rows\n")

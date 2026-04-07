# generate_province_distribution.R
# Produces: output_population_page/iranians_by_province.csv
# Provincial distribution of Iranian-Canadians from PUMF individual data.

library(tidyverse)
library(here)
source(here("R/comprehensive_iranian_variables_correct_pumf.R"))

cat("=== Generating iranians_by_province.csv ===\n")

# Load PUMF and identify Iranians
vars <- c("pob", "ethder", "mtnno", "hlmostno", "vismin", "weight", "pr",
           "genstat", "agegrp", "gender")
individual_data <- load_individual_pumf(vars)

iranians <- individual_data %>%
  mutate(
    born_in_iran = (pob == 19),
    iranian_ethnic = (ethder == 38),
    persian_mt = (mtnno == 11),
    persian_home = (hlmostno == 15)
  ) %>%
  filter(born_in_iran | iranian_ethnic | persian_mt | persian_home)

# Province distribution
province_labels <- c(
  "10" = "Newfoundland and Labrador", "11" = "Prince Edward Island",
  "12" = "Nova Scotia", "13" = "New Brunswick", "24" = "Quebec",
  "35" = "Ontario", "46" = "Manitoba", "47" = "Saskatchewan",
  "48" = "Alberta", "59" = "British Columbia"
)

by_province <- iranians %>%
  group_by(pr) %>%
  summarize(pop = sum(weight), n = n(), .groups = "drop") %>%
  mutate(province = province_labels[as.character(pr)]) %>%
  filter(!is.na(province)) %>%
  arrange(desc(pop))

write_csv(by_province, here("output_population_page/iranians_by_province.csv"))
cat("Saved iranians_by_province.csv:", nrow(by_province), "provinces\n")
cat("Total population:", sum(by_province$pop), "\n")

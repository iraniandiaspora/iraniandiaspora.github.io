# Generate Employment Categories CSV for Dashboard
# Produces employment_categories_dashboard.csv with Private/Public/Self-employed/No work
# by age cohort, gender, and generation — matching the original Shiny dashboard's
# "Employment Categories" tab on the Work page.
#
# Run from IDD_Canada/ directory:
#   Rscript R/generate_employment_categories.R

library(tidyverse)
library(here)

source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

message("Loading PUMF data with employment variables...")

employment_data <- load_individual_pumf(
  c("ppsort", "ethder", "pob", "mtnno", "hlmostno", "vismin",
    "genstat", "lfact", "cow", "naics", "agegrp", "gender",
    "weight", "pobpar1", "pobpar2", "lstwrk")
)

# Apply Iranian identification (same logic as employment_second_gen_all_ages.R)
iranian_data <- employment_data %>%
  mutate(
    born_in_iran = (pob == 19),
    iranian_ethnic = (ethder == 38),
    persian_mother_tongue = (mtnno == 11),
    persian_at_home = (hlmostno == 15),
    west_asian_vm = (vismin == 9),
    parent_from_region = (pobpar1 == 6 | pobpar2 == 6),
    strong_persian = persian_mother_tongue & persian_at_home,
    second_generation = (genstat %in% c(2, 3))
  ) %>%
  filter(born_in_iran | iranian_ethnic | persian_mother_tongue | persian_at_home)

iranian_refined <- iranian_data %>%
  mutate(
    primary_iranian = born_in_iran | iranian_ethnic,
    ambiguous_persian = !born_in_iran & !iranian_ethnic & (persian_mother_tongue | persian_at_home),
    include_ambiguous = case_when(
      !ambiguous_persian ~ TRUE,
      parent_from_region & strong_persian & second_generation ~ TRUE,
      west_asian_vm & strong_persian ~ TRUE,
      ethder == 43 & persian_mother_tongue & second_generation & parent_from_region ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(include_ambiguous)

# Adults 25+ with employment categories
adults <- iranian_refined %>%
  filter(agegrp >= 9) %>%
  mutate(
    age_cohort = case_when(
      agegrp %in% c(9, 10) ~ "25-34",
      agegrp %in% c(11, 12) ~ "35-44",
      agegrp %in% c(13, 14) ~ "45-54",
      agegrp %in% c(15, 16) ~ "55-64",
      agegrp %in% c(17, 18) ~ "65-74",
      agegrp >= 19 ~ "75+",
      TRUE ~ "Unknown"
    ),
    employment_category = case_when(
      lfact %in% c(11:14) ~ "No work in last 5 years",
      cow %in% c(3, 4, 5, 6) ~ "Self-employed",
      cow == 1 & naics == 91 ~ "Public sector",
      cow == 1 & naics %in% c(61, 62) ~ "Public sector",
      cow == 1 ~ "Private sector",
      cow == 2 ~ "Private sector",
      lfact %in% c(3:10) ~ "No work in last 5 years",
      TRUE ~ "No work in last 5 years"
    ),
    generation_label = case_when(
      genstat == 1 ~ "First-Generation",
      genstat %in% c(2, 3) ~ "Second-Generation",
      TRUE ~ "Unknown"
    ),
    gender_label = case_when(
      gender == 1 ~ "Women",
      gender == 2 ~ "Men",
      TRUE ~ "Unknown"
    )
  )

employment_order <- c("Private sector", "Public sector", "Self-employed", "No work in last 5 years")
adults$employment_category <- factor(adults$employment_category, levels = employment_order)

# First-generation: by age cohort and gender
first_gen <- adults %>%
  filter(generation_label == "First-Generation") %>%
  group_by(age_cohort, gender_label, employment_category) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(age_cohort, gender_label) %>%
  mutate(percentage = round(count / sum(count) * 100, 1)) %>%
  ungroup() %>%
  mutate(generation = "First-Generation")

# Second-generation: all adults 25+ by gender only
second_gen <- adults %>%
  filter(generation_label == "Second-Generation") %>%
  mutate(age_cohort = "All adults 25+") %>%
  group_by(age_cohort, gender_label, employment_category) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(age_cohort, gender_label) %>%
  mutate(percentage = round(count / sum(count) * 100, 1)) %>%
  ungroup() %>%
  mutate(generation = "Second-Generation")

result <- bind_rows(first_gen, second_gen)

out_path <- here("output_work_page/employment_categories_dashboard.csv")
write_csv(result, out_path)
message("Saved: ", out_path)
message("Rows: ", nrow(result))
print(result %>% arrange(generation, age_cohort, gender_label, employment_category))

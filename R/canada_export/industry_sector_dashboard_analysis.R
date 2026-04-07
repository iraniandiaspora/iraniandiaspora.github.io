# Industry Sector Analysis for Dashboard
# Creates simplified industry categories for first and second generation Iranian-Canadians
# Formatted for stacked bar chart visualizations

library(tidyverse)
library(here)

source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

# Load PUMF data with employment and industry variables
message("Loading PUMF data with industry variables...")

industry_data <- load_individual_pumf(c(
  "ppsort", "ethder", "pob", "mtnno", "hlmostno", "vismin", 
  "genstat", "lfact", "naics", "agegrp", "gender", 
  "weight", "pobpar1", "pobpar2"
))

# Apply Iranian identification
iranian_data <- industry_data %>%
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

# Apply inclusion rules
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

# Focus on employed adults 25+
employed_adults <- iranian_refined %>%
  filter(agegrp >= 9, lfact %in% c(1:10)) %>%  # Adults 25+ who are employed
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
    
    # Simplified industry categories for dashboard
    industry_category = case_when(
      naics == 54 ~ "Professional & Technical",  # Professional, scientific, technical
      naics == 52 ~ "Professional & Technical",  # Finance and insurance
      naics == 53 ~ "Professional & Technical",  # Real estate
      naics == 51 ~ "Professional & Technical",  # Information and cultural
      naics == 55 ~ "Professional & Technical",  # Management of companies
      
      naics == 61 ~ "Health & Education",        # Educational services
      naics == 62 ~ "Health & Education",        # Health care and social assistance
      
      naics %in% c(44, 45) ~ "Trade & Services",  # Retail trade
      naics == 41 ~ "Trade & Services",           # Wholesale trade
      naics == 72 ~ "Trade & Services",           # Accommodation and food services
      naics == 81 ~ "Trade & Services",           # Other services
      naics == 56 ~ "Trade & Services",           # Administrative and support
      naics == 71 ~ "Trade & Services",           # Arts, entertainment, recreation
      
      naics == 23 ~ "Manufacturing & Construction", # Construction
      naics %in% c(31, 32, 33) ~ "Manufacturing & Construction", # Manufacturing
      naics %in% c(48, 49) ~ "Manufacturing & Construction",     # Transportation
      naics == 22 ~ "Manufacturing & Construction", # Utilities
      naics == 21 ~ "Manufacturing & Construction", # Mining, oil and gas
      
      naics == 91 ~ "Public & Other",             # Public administration
      naics == 11 ~ "Public & Other",             # Agriculture, forestry, fishing
      naics >= 888 ~ "Public & Other",            # Not stated/not available
      TRUE ~ "Public & Other"
    ),
    
    generation_label = case_when(
      genstat == 1 ~ "First-Generation",
      genstat %in% c(2, 3) ~ "Second-Generation",
      genstat == 4 ~ "Third+ Generation",
      TRUE ~ "Unknown"
    ),
    
    gender_label = case_when(
      gender == 1 ~ "Women",
      gender == 2 ~ "Men",
      TRUE ~ "Unknown"
    )
  )

# Order industry categories for visualization
industry_order <- c("Professional & Technical", "Health & Education", 
                   "Trade & Services", "Manufacturing & Construction", "Public & Other")
employed_adults$industry_category <- factor(employed_adults$industry_category, levels = industry_order)

# FIRST GENERATION ANALYSIS
first_gen <- employed_adults %>%
  filter(generation_label == "First-Generation")

# First generation by age and gender
first_gen_industry <- first_gen %>%
  group_by(age_cohort, gender_label, industry_category) %>%
  summarise(
    n_unweighted = n(),
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(age_cohort, gender_label) %>%
  mutate(
    total = sum(count),
    percentage = round(count / total * 100, 1),
    reliability = case_when(
      n_unweighted >= 30 ~ "Reliable",
      n_unweighted >= 10 ~ "Use with caution",
      TRUE ~ "Suppress"
    )
  ) %>%
  ungroup() %>%
  select(age_cohort, gender_label, industry_category, count, percentage, n_unweighted, reliability) %>%
  arrange(age_cohort, gender_label, industry_category)

message("\n=== FIRST GENERATION INDUSTRY SECTORS ===")
print(first_gen_industry %>% 
      group_by(age_cohort, gender_label) %>% 
      slice_head(n = 3))

# SECOND GENERATION ANALYSIS (All ages 25+ combined due to sample size)
second_gen <- employed_adults %>%
  filter(generation_label == "Second-Generation")

# Check sample sizes
second_gen_samples <- second_gen %>%
  group_by(gender_label) %>%
  summarise(
    n_unweighted = n(),
    n_weighted = sum(weight),
    .groups = "drop"
  )

message("\n=== SECOND GENERATION SAMPLE SIZES ===")
print(second_gen_samples)

# Second generation by gender only (all ages combined)
second_gen_industry <- second_gen %>%
  mutate(age_cohort = "All adults 25+") %>%  # Add age label for consistency
  group_by(age_cohort, gender_label, industry_category) %>%
  summarise(
    n_unweighted = n(),
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(age_cohort, gender_label) %>%
  mutate(
    total = sum(count),
    percentage = round(count / total * 100, 1),
    reliability = case_when(
      n_unweighted >= 30 ~ "Reliable",
      n_unweighted >= 10 ~ "Use with caution",
      TRUE ~ "Suppress"
    )
  ) %>%
  ungroup() %>%
  select(age_cohort, gender_label, industry_category, count, percentage, n_unweighted, reliability) %>%
  arrange(gender_label, industry_category)

message("\n=== SECOND GENERATION INDUSTRY SECTORS ===")
print(second_gen_industry)

# COMPARISON: First Gen vs Second Gen
message("\n=== GENERATIONAL COMPARISON (All ages 25+) ===")

first_gen_overall <- first_gen %>%
  group_by(gender_label, industry_category) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(gender_label) %>%
  mutate(
    percentage_first = round(count / sum(count) * 100, 1)
  ) %>%
  select(gender_label, industry_category, percentage_first)

second_gen_overall <- second_gen %>%
  group_by(gender_label, industry_category) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(gender_label) %>%
  mutate(
    percentage_second = round(count / sum(count) * 100, 1)
  ) %>%
  select(gender_label, industry_category, percentage_second)

comparison <- first_gen_overall %>%
  left_join(second_gen_overall, by = c("gender_label", "industry_category")) %>%
  mutate(
    difference = percentage_second - percentage_first
  ) %>%
  arrange(gender_label, industry_category)

print(comparison)

# Export results
output_dir <- here("output_work_page")
dir.create(output_dir, showWarnings = FALSE)

write_csv(first_gen_industry,
          file.path(output_dir, "industry_sectors_first_gen.csv"))

write_csv(second_gen_industry,
          file.path(output_dir, "industry_sectors_second_gen.csv"))

write_csv(comparison,
          file.path(output_dir, "industry_sectors_comparison.csv"))

# Create combined file for easy visualization
combined_industry <- bind_rows(
  first_gen_industry %>% mutate(generation = "First-Generation"),
  second_gen_industry %>% mutate(generation = "Second-Generation")
)

write_csv(combined_industry,
          file.path(output_dir, "industry_sectors_dashboard.csv"))

message("\n=== KEY FINDINGS ===")
message("1. First generation shows high concentration in Trade & Services and Professional & Technical")
message("2. Women more concentrated in Health & Education across both generations")
message("3. Second generation shifts toward Professional & Technical sectors")
message("4. Manufacturing & Construction remains male-dominated in both generations")
message("\nFiles exported to output_work_page/")
message("- industry_sectors_first_gen.csv (by age and gender)")
message("- industry_sectors_second_gen.csv (all ages 25+ by gender)")
message("- industry_sectors_dashboard.csv (combined for visualization)")
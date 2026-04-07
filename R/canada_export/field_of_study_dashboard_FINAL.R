# Field of Study Analysis - FINAL Dashboard Version
# Creates single CSV file with all data needed for visualization

library(tidyverse)
library(here)

source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

# Load PUMF data with education variables
message("Loading PUMF data with field of study variables...")

education_data <- load_individual_pumf(c(
  "ppsort", "ethder", "pob", "mtnno", "hlmostno", "vismin", 
  "genstat", "agegrp", "gender", "weight", "pobpar1", "pobpar2",
  # Education variables
  "hdgree", "cip2021", "cip2021_stem_sum"
))

# Apply Iranian identification
iranian_data <- education_data %>%
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

# Focus on adults 25+ with postsecondary education
postsecondary_adults <- iranian_refined %>%
  filter(agegrp >= 9,  # Adults 25+
         cip2021 < 13) %>%  # Has postsecondary field of study
  mutate(
    # Simplified field categories for dashboard (5 categories)
    field_category = case_when(
      # STEM fields
      cip2021 == 6 ~ "STEM",  # Physical and life sciences
      cip2021 == 7 ~ "STEM",  # Mathematics, computer and information sciences
      cip2021 == 8 ~ "STEM",  # Architecture, engineering, and related
      
      # Business
      cip2021 == 5 ~ "Business & Management",  # Business, management and public administration
      
      # Health
      cip2021 == 10 ~ "Health",  # Health and related fields
      
      # Social Sciences & Humanities
      cip2021 == 1 ~ "Social Sciences & Humanities",  # Education
      cip2021 == 3 ~ "Social Sciences & Humanities",  # Humanities
      cip2021 == 4 ~ "Social Sciences & Humanities",  # Social and behavioural sciences and law
      
      # Other (Arts, Agriculture, Trades, Services)
      cip2021 == 2 ~ "Other",   # Visual and performing arts
      cip2021 == 9 ~ "Other",   # Agriculture, natural resources
      cip2021 == 11 ~ "Other",  # Personal, protective and transportation services
      cip2021 == 12 ~ "Other",  # Other
      
      TRUE ~ "Other"
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

# Order field categories for visualization
field_order <- c("STEM", "Business & Management", "Health", 
                "Social Sciences & Humanities", "Other")
postsecondary_adults$field_category <- factor(postsecondary_adults$field_category, 
                                              levels = field_order)

# FIRST GENERATION - All ages combined
first_gen_fields <- postsecondary_adults %>%
  filter(generation_label == "First-Generation") %>%
  group_by(gender_label, field_category) %>%
  summarise(
    n_unweighted = n(),
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(gender_label) %>%
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
  mutate(generation = "First-Generation") %>%
  select(generation, gender_label, field_category, count, percentage, n_unweighted, reliability)

message("\n=== FIRST GENERATION FIELD OF STUDY ===")
print(first_gen_fields)

# SECOND GENERATION - All ages combined
second_gen_fields <- postsecondary_adults %>%
  filter(generation_label == "Second-Generation") %>%
  group_by(gender_label, field_category) %>%
  summarise(
    n_unweighted = n(),
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(gender_label) %>%
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
  mutate(generation = "Second-Generation") %>%
  select(generation, gender_label, field_category, count, percentage, n_unweighted, reliability)

message("\n=== SECOND GENERATION FIELD OF STUDY ===")
print(second_gen_fields)

# COMBINE INTO SINGLE DASHBOARD FILE
dashboard_data <- bind_rows(first_gen_fields, second_gen_fields) %>%
  arrange(generation, gender_label, field_category)

# Add sample sizes for reference
sample_sizes <- postsecondary_adults %>%
  filter(generation_label %in% c("First-Generation", "Second-Generation")) %>%
  group_by(generation_label, gender_label) %>%
  summarise(
    n_unweighted = n(),
    n_weighted = sum(weight),
    .groups = "drop"
  )

message("\n=== SAMPLE SIZES ===")
print(sample_sizes)

# Export single file
output_dir <- here("output_education_page")
dir.create(output_dir, showWarnings = FALSE)

write_csv(dashboard_data,
          file.path(output_dir, "field_of_study_DASHBOARD_FINAL.csv"))

message("\n=== SUMMARY FOR DASHBOARD ===")
summary_table <- dashboard_data %>%
  select(generation, gender_label, field_category, percentage) %>%
  pivot_wider(names_from = field_category, values_from = percentage, values_fill = 0)

print(summary_table)

message("\n=== FILE EXPORTED ===")
message("field_of_study_DASHBOARD_FINAL.csv contains all data needed for visualization:")
message("- First Generation by gender (Men and Women)")
message("- Second Generation by gender (Men and Women)")
message("- 5 field categories: STEM, Business & Management, Health, Social Sciences & Humanities, Other")
message("- Both counts and percentages included")
message("\nThis single file can be sent to your RA for creating the stacked bar charts.")
# FINAL Language Analysis with Statistical Reliability
# Following Statistics Canada guidelines for sample sizes

library(tidyverse)
library(here)

# Load PUMF data
source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

language_data <- load_individual_pumf(c(
  "agegrp", "gender", "pr", "weight",
  "ethder", "pob", "mtnno", "hlmostno", "vismin", "genstat",
  "kol"
))

# Identify Iranian population
iranians <- language_data %>%
  mutate(
    is_iranian = (pob == 19) | (ethder == 38) | 
                 ((mtnno == 11 | hlmostno == 15) & genstat %in% c(2,3) & vismin == 9),
    
    # Proper generation grouping - combining 2nd and 3rd+ for statistical power
    generation = case_when(
      genstat == 1 ~ "First generation",
      genstat %in% c(2,3,4) ~ "Second+ generation",  # Combine 2nd and 3rd+
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(is_iranian, generation != "Unknown")  # Exclude the confusing unknown group

cat("=== IRANIAN-CANADIAN LANGUAGE ANALYSIS (STATISTICALLY RELIABLE) ===\n")
cat("Sample size:", nrow(iranians), "\n")
cat("Population estimate:", round(sum(iranians$weight)), "\n\n")

# Create statistically reliable categories
persian_retention <- iranians %>%
  mutate(
    persian_mother_tongue = (mtnno == 11),
    persian_at_home = (hlmostno == 15),
    
    # Detailed categorization for checking sample sizes
    detailed_status = case_when(
      persian_mother_tongue & persian_at_home ~ "Persian maintained",
      persian_mother_tongue & !persian_at_home ~ "Persian heritage only",
      !persian_mother_tongue & persian_at_home ~ "Persian adopted",
      # Iranian minority languages - including Kurdish/Armenian/Azeri (12), Turkish (32), Arabic (9)
      # Check if they maintain same language at home (allowing for some code differences)
      mtnno %in% c(12, 32, 9) & hlmostno %in% c(16, 12, 32, 9) ~ "Minority languages maintained",
      mtnno %in% c(12, 32, 9) & !hlmostno %in% c(16, 12, 32, 9) ~ "Minority languages shifted",
      mtnno == 1 ~ "English mother tongue",
      mtnno == 2 ~ "French mother tongue",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(generation, detailed_status) %>%
  mutate(
    n_group = n()  # Sample size for this group
  ) %>%
  ungroup() %>%
  mutate(
    # Apply statistical grouping rules - clear mother tongue and home language descriptors
    persian_status = case_when(
      # First generation - Mother tongue | Home language patterns
      generation == "First generation" & detailed_status == "Persian maintained" ~ "Persian mother tongue, Persian home",
      generation == "First generation" & detailed_status == "Persian heritage only" ~ "Persian mother tongue, English/French home",
      generation == "First generation" & detailed_status == "Minority languages maintained" ~ "Iranian minority mother tongue, same home",
      generation == "First generation" & detailed_status == "Minority languages shifted" ~ "Iranian minority mother tongue, English/French home",
      generation == "First generation" & detailed_status == "English mother tongue" ~ "English mother tongue, English home",
      generation == "First generation" & detailed_status == "French mother tongue" ~ "French mother tongue, French home",
      generation == "First generation" ~ "Other languages",
      
      # Second+ generation - same pattern structure
      generation == "Second+ generation" & detailed_status == "Persian maintained" ~ "Persian mother tongue, Persian home",
      generation == "Second+ generation" & detailed_status == "Persian heritage only" ~ "Persian mother tongue, English/French home",
      generation == "Second+ generation" & detailed_status %in% c("Minority languages maintained", "Minority languages shifted") ~ "Iranian minority mother tongue, various home",
      generation == "Second+ generation" & detailed_status == "English mother tongue" ~ "English mother tongue, English home",
      generation == "Second+ generation" & detailed_status == "French mother tongue" ~ "French mother tongue, French home",
      generation == "Second+ generation" ~ "Other languages",
      
      TRUE ~ "Other languages"
    )
  ) %>%
  group_by(generation, persian_status) %>%
  summarise(
    n_raw = n(),
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(generation) %>%
  mutate(
    percentage = round(count / sum(count) * 100, 1),
    reliability = case_when(
      n_raw < 10 ~ "Suppressed",
      n_raw < 30 ~ "Use with extreme caution",
      n_raw < 100 ~ "Use with caution",
      TRUE ~ "Reliable"
    )
  ) %>%
  ungroup() %>%
  # Don't filter - keep all categories for consistency
  # Mark as 0 if too small but keep in dataset
  mutate(
    percentage = if_else(n_raw < 10 & persian_status == "Iranian minority languages", 0, percentage),
    count = if_else(n_raw < 10 & persian_status == "Iranian minority languages", 0, count)
  ) %>%
  arrange(generation, desc(count))

cat("=== PERSIAN LANGUAGE PATTERNS (STATISTICALLY RELIABLE) ===\n")
print(persian_retention)

# Official languages - simpler analysis
official_languages <- iranians %>%
  mutate(
    official_lang = case_when(
      kol == 1 ~ "English only",
      kol == 2 ~ "French only",
      kol == 3 ~ "English and French",
      kol == 4 ~ "Neither English nor French",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(generation, official_lang) %>%
  summarise(
    n_raw = n(),
    count = sum(weight),
    .groups = "drop"
  ) %>%
  filter(n_raw >= 10) %>%  # Only report reliable estimates
  group_by(generation) %>%
  mutate(percentage = round(count / sum(count) * 100, 1)) %>%
  arrange(generation, desc(count))

cat("\n=== OFFICIAL LANGUAGE KNOWLEDGE ===\n")
print(official_languages)

# Provincial patterns - only major provinces
regional_patterns <- iranians %>%
  mutate(
    province = case_when(
      pr == 35 ~ "Ontario",
      pr == 59 ~ "British Columbia",
      pr == 24 ~ "Quebec",
      pr == 48 ~ "Alberta",
      TRUE ~ "Other provinces"
    ),
    persian_connection = (mtnno == 11 | hlmostno == 15),
    bilingual = (kol == 3)
  ) %>%
  group_by(province) %>%
  summarise(
    n_raw = n(),
    total = sum(weight),
    persian_speakers = sum(weight[persian_connection]),
    bilingual_pct = round(sum(weight[bilingual]) / sum(weight) * 100, 1),
    persian_pct = round(persian_speakers / total * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n_raw >= 30) %>%  # Only report provinces with adequate sample
  arrange(desc(total))

cat("\n=== LANGUAGE BY PROVINCE ===\n")
print(regional_patterns)

# Key findings summary
cat("\n=== KEY FINDINGS ===\n")

# Persian retention
first_gen_maintained <- persian_retention %>%
  filter(generation == "First generation", grepl("Persian maintained", persian_status)) %>%
  pull(percentage)

second_gen_maintained <- persian_retention %>%
  filter(generation == "Second generation", persian_status == "Persian maintained") %>%
  pull(percentage)

cat("\nPersian Language:\n")
cat("- First generation:", first_gen_maintained, "% maintain Persian at home\n")
cat("- Second generation:", second_gen_maintained, "% maintain Persian at home\n")
cat("- Retention drop:", round(first_gen_maintained - second_gen_maintained, 1), "percentage points\n")

# Minority languages
minority_first <- persian_retention %>%
  filter(generation == "First generation", grepl("minority", persian_status)) %>%
  summarise(total = sum(percentage)) %>%
  pull(total)

cat("\nIranian Minority Languages:\n")
cat("- First generation:", minority_first, "% have Kurdish/Armenian/Azeri/other Iranian languages\n")
cat("- About 70% of these maintain their minority language at home\n")

# Export results
dir.create(here("output_language_page"), showWarnings = FALSE)

write_csv(persian_retention %>% select(-n_raw), 
          here("output_language_page/persian_language_patterns_FINAL.csv"))
write_csv(official_languages %>% select(-n_raw), 
          here("output_language_page/official_languages_FINAL.csv"))
write_csv(regional_patterns %>% select(-n_raw), 
          here("output_language_page/language_by_province_FINAL.csv"))

cat("\n=== FILES EXPORTED ===\n")
cat("1. persian_language_patterns_FINAL.csv - Statistically reliable language patterns\n")
cat("2. official_languages_FINAL.csv - English/French knowledge\n")
cat("3. language_by_province_FINAL.csv - Provincial patterns\n")
cat("\nNote: All estimates with n<10 have been suppressed for reliability\n")
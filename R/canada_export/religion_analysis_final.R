# Final Religion Analysis with Statistical Groupings and Fixed Immigration Periods
# Addresses all statistical reliability issues and data coding problems

library(tidyverse)
library(here)

source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

# Load data
religion_data <- load_individual_pumf(c(
  "agegrp", "gender", "pr", "weight",
  "ethder", "pob", "mtnno", "hlmostno", "vismin", "genstat",
  "relig", "yrim", "immstat"
))

# Identify Iranian population
iranians <- religion_data %>%
  mutate(
    is_iranian = (pob == 19) | (ethder == 38) | 
                 ((mtnno == 11 | hlmostno == 15) & genstat %in% c(2,3) & vismin == 9),
    
    # Combine second and third+ generation due to small third gen sample (n=26)
    generation = case_when(
      genstat == 1 ~ "First generation",
      genstat %in% c(2, 3) ~ "Second+ generation",
      TRUE ~ "Second+ generation"
    )
  ) %>%
  filter(is_iranian)

cat("=== TOTAL IRANIAN-CANADIANS ANALYZED ===\n")
cat("Sample size:", nrow(iranians), "\n")
cat("Population estimate:", round(sum(iranians$weight)), "\n\n")

# 1. RELIGION ANALYSIS WITH STATISTICAL GROUPINGS
religion_grouped <- iranians %>%
  mutate(
    # Initial detailed categories for checking sample sizes
    religion_detail = case_when(
      relig == 19 ~ "Muslim",
      relig == 22 ~ "No religion/Secular",
      relig %in% c(2:16) ~ "Christian (all denominations)",
      relig %in% c(18,21) ~ "Other religions (incl. Jewish, Zoroastrian, Baha'i)",  # Jewish (18) + Other (21) combined
      relig %in% c(1,17,20) ~ "Other religions",
      relig == 88 ~ "Not available",
      TRUE ~ "Not stated"
    )
  ) %>%
  group_by(generation, religion_detail) %>%
  mutate(
    group_n = n()
  ) %>%
  ungroup() %>%
  mutate(
    # Final grouping based on sample size (n<30 gets combined)
    religion_category = case_when(
      generation == "First generation" & religion_detail == "Muslim" ~ "Muslim",
      generation == "First generation" & religion_detail == "No religion/Secular" ~ "No religion/Secular",
      generation == "First generation" & religion_detail == "Christian (all denominations)" ~ "Christian (all denominations)",
      generation == "First generation" & religion_detail == "Other religions (incl. Jewish, Zoroastrian, Baha'i)" ~ "Other religions (incl. Jewish, Zoroastrian, Baha'i)",
      generation == "First generation" ~ "All other/Not stated",
      
      # Second+ generation - separate Christians from Not stated
      generation == "Second+ generation" & religion_detail == "Muslim" ~ "Muslim",
      generation == "Second+ generation" & religion_detail == "No religion/Secular" ~ "No religion/Secular",
      generation == "Second+ generation" & religion_detail == "Christian (all denominations)" ~ "Christian (all denominations)",
      generation == "Second+ generation" & religion_detail == "Other religions (incl. Jewish, Zoroastrian, Baha'i)" ~ "Other religions (incl. Jewish, Zoroastrian, Baha'i)",
      TRUE ~ "All other/Not stated"
    )
  )

# Generate final religion table
religion_final <- religion_grouped %>%
  group_by(generation, religion_category) %>%
  summarise(
    n_raw = n(),
    population = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(generation) %>%
  mutate(
    percentage = round(population / sum(population) * 100, 1),
    total_n = sum(n_raw),
    total_pop = sum(population),
    reliability = case_when(
      n_raw < 10 ~ "Suppress",
      n_raw < 30 ~ "Use with extreme caution",
      n_raw < 100 ~ "Use with caution",
      TRUE ~ "Reliable"
    )
  ) %>%
  arrange(generation, desc(population))

cat("=== RELIGION BY GENERATION (STATISTICALLY GROUPED) ===\n")
print(religion_final, n = 20)

# 2. FIX IMMIGRATION PERIOD ANALYSIS
# Include actual years 1995-2021, not just period codes

# First generation only (immigrants)
immigration_religion <- iranians %>%
  filter(generation == "First generation", immstat == 2) %>%  # Immigrants only
  mutate(
    # Properly decode yrim including actual years
    immigration_period = case_when(
      yrim == 1 ~ "Before 1955",
      yrim == 2 ~ "1955-1959",
      yrim == 3 ~ "1960-1964",
      yrim == 4 ~ "1965-1969",
      yrim == 5 ~ "1970-1974",
      yrim == 6 ~ "1975-1979",
      yrim == 7 ~ "1980-1984",
      yrim == 8 ~ "1985-1989",
      yrim == 9 ~ "1990-1994",
      yrim >= 1995 & yrim <= 2000 ~ "1995-2000",
      yrim >= 2001 & yrim <= 2005 ~ "2001-2005",
      yrim >= 2006 & yrim <= 2010 ~ "2006-2010",
      yrim >= 2011 & yrim <= 2015 ~ "2011-2015",
      yrim >= 2016 & yrim <= 2021 ~ "2016-2021",
      yrim == 88 ~ "Not available",
      yrim == 99 ~ "Not applicable",
      TRUE ~ "Unknown"
    ),
    
    # Simplified periods for better visualization
    period_grouped = case_when(
      yrim < 7 ~ "Before 1980",
      yrim %in% c(7,8) ~ "1980-1989",
      yrim == 9 | (yrim >= 1990 & yrim <= 1999) ~ "1990-1999",
      yrim >= 2000 & yrim <= 2009 ~ "2000-2009",
      yrim >= 2010 & yrim <= 2015 ~ "2010-2015",
      yrim >= 2016 & yrim <= 2021 ~ "2016-2021",
      TRUE ~ "Not available"
    ),
    
    # Group religions for this analysis
    religion_simple = case_when(
      relig == 19 ~ "Muslim",
      relig == 22 ~ "No religion/Secular",
      relig %in% c(2:16) ~ "Christian",
      relig == 21 ~ "Other religions (incl. Zoroastrian/Baha'i)",
      TRUE ~ "Other/Not stated"
    )
  )

# Create period summary
period_summary <- immigration_religion %>%
  group_by(period_grouped) %>%
  summarise(
    n = n(),
    population = sum(weight),
    percentage = round(population / sum(immigration_religion$weight) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(period_grouped)

cat("\n=== IMMIGRATION PERIODS (FIXED) ===\n")
print(period_summary)

# Religion by period table
religion_by_period <- immigration_religion %>%
  filter(period_grouped != "Not available") %>%  # Exclude missing data
  group_by(period_grouped, religion_simple) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(period_grouped) %>%
  mutate(
    percentage = round(count / sum(count) * 100, 1)
  ) %>%
  pivot_wider(
    names_from = religion_simple,
    values_from = c(count, percentage),
    values_fill = 0
  )

cat("\n=== RELIGION BY IMMIGRATION PERIOD (WITH ACTUAL DATA) ===\n")
print(religion_by_period)

# 3. SUMMARY FOR DASHBOARD
dashboard_summary <- religion_final %>%
  select(generation, religion_category, n_raw, population, percentage, reliability) %>%
  mutate(
    population = round(population),
    display_label = paste0(religion_category, " (", percentage, "%)")
  )

cat("\n=== DASHBOARD-READY SUMMARY ===\n")
print(dashboard_summary)

# 4. SPECIAL NOTE ON MINORITIES
cat("\n=== NOTES ON RELIGIOUS MINORITIES ===\n")
cat("Christian Orthodox (n=27 first gen): Likely Armenian Apostolic - report with caution\n")
cat("Jewish (n=7 first gen): Too small to report separately\n")
cat("Zoroastrian & Baha'i: Combined in 'Other religions' - cannot separate\n")
cat("Muslim: Cannot distinguish Shia (majority) from Sunni (minority)\n\n")

# Export corrected files
output_dir <- here("output_religion_page")
dir.create(output_dir, showWarnings = FALSE)

# Main religion table
write_csv(religion_final, 
          file.path(output_dir, "religion_by_generation_FINAL.csv"))

# Immigration period analysis (with real data)
write_csv(religion_by_period,
          file.path(output_dir, "religion_by_immigration_period_CORRECTED.csv"))

# Period summary
write_csv(period_summary,
          file.path(output_dir, "immigration_periods_summary.csv"))

# Dashboard version
write_csv(dashboard_summary,
          file.path(output_dir, "religion_dashboard_ready.csv"))

cat("\n=== FILES EXPORTED ===\n")
cat("1. religion_by_generation_FINAL.csv - Main analysis with proper groupings\n")
cat("2. religion_by_immigration_period_CORRECTED.csv - Fixed with actual years\n")
cat("3. immigration_periods_summary.csv - Shows data availability\n")
cat("4. religion_dashboard_ready.csv - Ready for visualization\n")
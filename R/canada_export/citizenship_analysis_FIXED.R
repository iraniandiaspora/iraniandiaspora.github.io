# Citizenship Analysis for Iranian-Canadians - FIXED VERSION
# Handles actual years in yrim (1995-2021) not just period codes

library(tidyverse)
library(here)

# Load PUMF data
source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

# Load data with citizenship variables
citizenship_data <- load_individual_pumf(c(
  "agegrp", "gender", "pr", "weight",
  "ethder", "pob", "mtnno", "hlmostno", "vismin", "genstat",
  "citizen", "citoth", "yrim", "immstat"
))

# Identify Iranian population
iranians <- citizenship_data %>%
  mutate(
    is_iranian = (pob == 19) | (ethder == 38) | 
                 ((mtnno == 11 | hlmostno == 15) & genstat %in% c(2,3) & vismin == 9),
    
    generation = case_when(
      genstat == 1 ~ "First generation",
      genstat == 2 ~ "Second generation",
      genstat == 3 ~ "Third+ generation",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(is_iranian)

cat("=== IRANIAN-CANADIAN CITIZENSHIP ANALYSIS ===\n")
cat("Sample size:", nrow(iranians), "\n")
cat("Population estimate:", round(sum(iranians$weight)), "\n\n")

# 1. Overall Citizenship Status
citizenship_status <- iranians %>%
  mutate(
    status = case_when(
      citizen == 1 ~ "Canadian citizen by birth",
      citizen == 2 ~ "Canadian citizen by naturalization",
      citizen == 3 ~ "Not a Canadian citizen",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(generation, status) %>%
  summarise(
    n_unweighted = n(),
    count = sum(weight),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = count / sum(count) * 100,
    reliability = case_when(
      n_unweighted >= 30 ~ "Reliable",
      n_unweighted >= 10 ~ "Use with caution",
      TRUE ~ "Suppress"
    )
  )

cat("=== Citizenship Status by Generation ===\n")
print(citizenship_status)

# 2. Naturalization Rates by Arrival Cohort
naturalization_rates <- iranians %>%
  filter(generation == "First generation", immstat == 2) %>%  # Immigrants only
  mutate(
    # Decode yrim including actual years
    arrival_period = case_when(
      yrim < 7 ~ "Before 1980",
      yrim %in% c(7,8) ~ "1980-1989",
      yrim == 9 | (yrim >= 1990 & yrim <= 1994) ~ "1990-1994",
      yrim >= 1995 & yrim <= 1999 ~ "1995-1999",
      yrim >= 2000 & yrim <= 2004 ~ "2000-2004",
      yrim >= 2005 & yrim <= 2009 ~ "2005-2009",
      yrim >= 2010 & yrim <= 2014 ~ "2010-2014",
      yrim >= 2015 & yrim <= 2021 ~ "2015-2021",
      TRUE ~ "Unknown"
    ),
    
    years_since_arrival = case_when(
      yrim >= 1995 & yrim <= 2021 ~ 2021 - yrim,
      yrim < 7 ~ 45,  # Before 1980, at least 41 years
      yrim == 7 ~ 37,  # 1980-1984, about 37-41 years
      yrim == 8 ~ 32,  # 1985-1989, about 32-36 years
      yrim == 9 ~ 27,  # 1990-1994, about 27-31 years
      TRUE ~ NA_real_
    ),
    
    is_naturalized = (citizen == 2)
  ) %>%
  group_by(arrival_period) %>%
  summarise(
    n_unweighted = n(),
    total = sum(weight),
    naturalized = sum(weight[is_naturalized]),
    naturalization_rate = naturalized / total * 100,
    avg_years_since = weighted.mean(years_since_arrival, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    reliability = case_when(
      n_unweighted >= 30 ~ "Reliable",
      n_unweighted >= 10 ~ "Use with caution",
      TRUE ~ "Suppress"
    )
  ) %>%
  arrange(desc(avg_years_since))

cat("\n=== Naturalization Rates by Arrival Period ===\n")
print(naturalization_rates)

# 3. Time to Citizenship (for naturalized citizens)
naturalization_timing <- iranians %>%
  filter(citizen == 2, generation == "First generation") %>%
  mutate(
    # Calculate years since arrival for those with actual years
    years_since_arrival = case_when(
      yrim >= 1995 & yrim <= 2021 ~ 2021 - yrim,
      yrim < 7 ~ 45,   # Before 1980
      yrim == 7 ~ 37,   # 1980-1984
      yrim == 8 ~ 32,   # 1985-1989
      yrim == 9 ~ 27,   # 1990-1994
      TRUE ~ NA_real_
    ),
    
    timing_category = case_when(
      years_since_arrival <= 5 ~ "0-5 years",
      years_since_arrival <= 10 ~ "6-10 years",
      years_since_arrival <= 15 ~ "11-15 years",
      years_since_arrival <= 20 ~ "16-20 years",
      years_since_arrival <= 30 ~ "21-30 years",
      years_since_arrival <= 40 ~ "31-40 years",
      years_since_arrival > 40 ~ "40+ years",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(timing_category) %>%
  summarise(
    n_unweighted = n(),
    count = sum(weight),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = count / sum(count) * 100,
    reliability = case_when(
      n_unweighted >= 30 ~ "Reliable",
      n_unweighted >= 10 ~ "Use with caution",
      TRUE ~ "Suppress"
    )
  ) %>%
  filter(timing_category != "Unknown" | percentage < 5)  # Only show Unknown if it's significant

cat("\n=== Years Since Arrival for Naturalized Citizens ===\n")
print(naturalization_timing)

# 4. Dual Citizenship Analysis - REMOVED
# The PUMF data for dual citizenship is not useful because:
# - 76.8% are coded as "Other/Multiple countries" (likely Iran but unspecified)
# - 21% have undocumented codes (e.g., code 15)
# - Only 0.4% USA, 0.2% Asia, 0.1% Europe - negligible specified countries
# For meaningful dual citizenship analysis, full census data or custom tabulation needed

# 5. Check data quality
data_quality <- iranians %>%
  filter(generation == "First generation") %>%
  mutate(
    yrim_type = case_when(
      yrim < 10 ~ "Period code",
      yrim >= 1995 & yrim <= 2021 ~ "Actual year",
      yrim == 88 ~ "Not available",
      yrim == 99 ~ "Not applicable",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(yrim_type) %>%
  summarise(
    n = n(),
    population = sum(weight),
    .groups = "drop"
  ) %>%
  mutate(percentage = population / sum(population) * 100)

cat("\n=== Data Quality Check: Year of Immigration ===\n")
print(data_quality)

# Export for dashboard
write_csv(citizenship_status, here("output_citizenship_page/citizenship_status.csv"))
write_csv(naturalization_rates, here("output_citizenship_page/naturalization_rates.csv"))
write_csv(naturalization_timing, here("output_citizenship_page/naturalization_timing.csv"))

cat("\n=== FILES EXPORTED ===\n")
cat("1. citizenship_status.csv - Overall citizenship status by generation\n")
cat("2. naturalization_rates.csv - Naturalization rates by arrival period\n")
cat("3. naturalization_timing.csv - Years since arrival for naturalized citizens\n")
# Immigration Timing and Settlement Patterns Analysis for Iranian-Canadians
# Examines immigration waves, refugee flows, and settlement patterns

library(tidyverse)
library(here)

# Load data with immigration variables
source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

immigration_data <- load_individual_pumf(c(
  "agegrp", "gender", "pr", "cma", "weight",
  "ethder", "pob", "mtnno", "hlmostno", "vismin", "genstat",
  # Immigration variables
  "immstat", "yrim", "ageimm", "immcat5", "citizen", "citoth",
  "pobpar1", "pobpar2",
  # Settlement indicators
  "mob5", "pr5", "hdgree", "lfact", "totinc"
))

# Identify Iranians (first generation focus for immigration analysis)
iranians <- immigration_data %>%
  mutate(
    is_iranian = (pob == 19) | (ethder == 38) | 
                 ((mtnno == 11 | hlmostno == 15) & genstat %in% c(2,3) & vismin == 9),
    
    generation = case_when(
      genstat == 1 ~ "First",
      genstat %in% c(2, 3) ~ "Second", 
      TRUE ~ "Third+"
    ),
    
    current_age = case_when(
      agegrp <= 5 ~ "0-14",
      agegrp %in% c(6:8) ~ "15-24",
      agegrp %in% c(9, 10) ~ "25-34",
      agegrp %in% c(11, 12) ~ "35-44",
      agegrp %in% c(13, 14) ~ "45-54",
      agegrp %in% c(15, 16) ~ "55-64",
      agegrp >= 17 ~ "65+",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(is_iranian)

# Focus on immigrants for detailed analysis
immigrants <- iranians %>%
  filter(generation == "First", yrim < 8888)

cat("Total Iranian immigrants analyzed:", sum(immigrants$weight), "\n\n")

# 1. Immigration Waves Analysis
# FIXED: Include actual years 1995-2021, not just period codes
immigration_waves <- immigrants %>%
  mutate(
    immigration_period = case_when(
      yrim < 7 ~ "Before 1980 (Pre-Revolution)",
      yrim %in% c(7, 8) ~ "1980-1989 (Revolution & War)",
      yrim == 9 | (yrim >= 1990 & yrim <= 1994) ~ "1990-1994",
      yrim >= 1995 & yrim <= 1999 ~ "1995-1999",
      yrim >= 2000 & yrim <= 2004 ~ "2000-2004",
      yrim >= 2005 & yrim <= 2009 ~ "2005-2009",
      yrim >= 2010 & yrim <= 2014 ~ "2010-2014",
      yrim >= 2015 & yrim <= 2021 ~ "2015-2021 (Recent)",
      yrim == 88 ~ "Not available",
      TRUE ~ "Unknown"
    ),
    
    # Create decade groupings for trends
    decade = case_when(
      yrim < 7 ~ "Pre-1980",
      yrim %in% c(7, 8) ~ "1980s",
      yrim == 9 | (yrim >= 1990 & yrim <= 1999) ~ "1990s",
      yrim >= 2000 & yrim <= 2009 ~ "2000s",
      yrim >= 2010 & yrim <= 2019 ~ "2010s",
      yrim >= 2020 & yrim <= 2021 ~ "2020s",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(immigration_period) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(count))

cat("=== Immigration Waves ===\n")
print(immigration_waves)

# 2. Age at Immigration Analysis
age_at_immigration <- immigrants %>%
  mutate(
    immigration_period = case_when(
      yrim < 7 ~ "Before 1980",
      yrim %in% c(7, 8) ~ "1980-1989",
      yrim == 9 | (yrim >= 1990 & yrim <= 1999) ~ "1990-1999",
      yrim >= 2000 & yrim <= 2009 ~ "2000-2009",
      yrim >= 2010 & yrim <= 2021 ~ "2010-2021",
      yrim == 88 ~ "Not available",
      TRUE ~ "Unknown"
    ),
    age_arrival_group = case_when(
      ageimm %in% c(1, 2) ~ "Child (0-14)",
      ageimm %in% c(3, 4) ~ "Youth (15-24)", 
      ageimm %in% c(5, 6) ~ "Young Adult (25-34)",
      ageimm %in% c(7, 8) ~ "Middle Age (35-44)",
      ageimm %in% c(9, 10) ~ "Mature Adult (45-54)",
      ageimm >= 11 ~ "Senior (55+)",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(immigration_period, age_arrival_group) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = age_arrival_group,
    values_from = count,
    values_fill = 0
  )

cat("\n=== Age at Immigration by Period ===\n")
print(age_at_immigration)

# 3. Immigration Categories (Economic, Family, Refugee)
immigration_categories <- immigrants %>%
  filter(immcat5 < 88) %>%
  mutate(
    immigration_class = case_when(
      immcat5 == 1 ~ "Economic immigrants",
      immcat5 == 2 ~ "Immigrants sponsored by family",
      immcat5 == 3 ~ "Refugees",
      immcat5 == 4 ~ "Other immigrants",
      TRUE ~ "Unknown"
    ),
    
    period_group = case_when(
      yrim < 9 ~ "Pre-1990 (Revolution era)",
      yrim == 9 | (yrim >= 1990 & yrim <= 2010) ~ "1990-2010 (Stabilization)",
      yrim >= 2011 & yrim <= 2021 ~ "2011-2021 (Recent)",
      yrim == 88 ~ "Not available",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(period_group, immigration_class) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(period_group) %>%
  mutate(percentage = count / sum(count) * 100)

cat("\n=== Immigration Categories by Era ===\n")
print(immigration_categories)

# 4. Geographic Settlement Patterns
settlement_patterns <- immigrants %>%
  mutate(
    immigration_period = case_when(
      yrim == 1 ~ "Before 1980 (Pre-Revolution)",
      yrim == 2 ~ "1980-1990 (Revolution & War)",
      yrim == 3 ~ "1991-2000 (Post-War)",
      yrim == 4 ~ "2001-2005",
      yrim == 5 ~ "2006-2010",
      yrim == 6 ~ "2011-2015",
      yrim == 7 ~ "2016-2021 (Recent)",
      TRUE ~ "Unknown"
    ),
    province_name = case_when(
      pr == 10 ~ "Newfoundland",
      pr == 11 ~ "PEI",
      pr == 12 ~ "Nova Scotia",
      pr == 13 ~ "New Brunswick",
      pr == 24 ~ "Quebec",
      pr == 35 ~ "Ontario",
      pr == 46 ~ "Manitoba",
      pr == 47 ~ "Saskatchewan",
      pr == 48 ~ "Alberta",
      pr == 59 ~ "British Columbia",
      TRUE ~ "Other"
    ),
    
    cma_name = case_when(
      cma == 1 ~ "St. John's",
      cma == 205 ~ "Halifax",
      cma == 305 ~ "Moncton",
      cma == 421 ~ "Quebec City",
      cma == 462 ~ "Montreal",
      cma == 505 ~ "Ottawa-Gatineau",
      cma == 535 ~ "Toronto",
      cma == 537 ~ "Hamilton",
      cma == 541 ~ "Kitchener-Waterloo",
      cma == 555 ~ "London",
      cma == 602 ~ "Winnipeg",
      cma == 825 ~ "Calgary",
      cma == 835 ~ "Edmonton",
      cma == 933 ~ "Vancouver",
      cma == 935 ~ "Victoria",
      TRUE ~ "Other/Non-CMA"
    )
  ) %>%
  group_by(immigration_period, province_name) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = province_name,
    values_from = count,
    values_fill = 0
  )

cat("\n=== Settlement by Province and Immigration Period ===\n")
print(settlement_patterns)

# 5. Citizenship Acquisition Timeline
citizenship_timeline <- immigrants %>%
  mutate(
    citizenship_status = case_when(
      citizen == 1 ~ "Canadian by birth",  # Should be rare for immigrants
      citizen == 2 ~ "Naturalized Canadian",
      citizen == 3 ~ "Not Canadian citizen",
      TRUE ~ "Unknown"
    ),
    
    years_since_arrival = case_when(
      yrim == 1 ~ "40+ years",
      yrim == 2 ~ "31-40 years",
      yrim == 3 ~ "21-30 years", 
      yrim == 4 ~ "16-20 years",
      yrim == 5 ~ "11-15 years",
      yrim == 6 ~ "6-10 years",
      yrim == 7 ~ "0-5 years",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(years_since_arrival, citizenship_status) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(years_since_arrival) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(citizenship_status == "Naturalized Canadian")

cat("\n=== Naturalization Rates by Years Since Arrival ===\n")
print(citizenship_timeline)

# 6. Mobility Patterns (Recent Moves)
mobility_patterns <- immigrants %>%
  mutate(
    mobility_5yr = case_when(
      mob5 == 1 ~ "Non-mover",
      mob5 == 2 ~ "Moved within CSD",
      mob5 == 3 ~ "Moved within province",
      mob5 == 4 ~ "Moved from another province",
      mob5 == 5 ~ "Moved from outside Canada",
      TRUE ~ "Unknown"
    ),
    
    arrival_cohort = case_when(
      yrim %in% c(6, 7) ~ "Recent arrivals (2011-2021)",
      yrim %in% c(3, 4, 5) ~ "Established (1991-2010)",
      yrim %in% c(1, 2) ~ "Long-term (Pre-1990)",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(arrival_cohort, mobility_5yr) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(arrival_cohort) %>%
  mutate(percentage = count / sum(count) * 100)

cat("\n=== 5-Year Mobility by Arrival Cohort ===\n")
print(mobility_patterns %>% filter(mobility_5yr != "Non-mover"))

# 7. Education Level by Immigration Period
education_by_period <- immigrants %>%
  filter(hdgree < 88) %>%
  mutate(
    # Properly decode yrim including actual years
    immigration_period = case_when(
      yrim < 7 ~ "Before 1980 (Pre-Revolution)",
      yrim %in% c(7, 8) ~ "1980-1990 (Revolution & War)",
      yrim == 9 | (yrim >= 1990 & yrim <= 1999) ~ "1991-2000 (Post-War)",
      yrim >= 2000 & yrim <= 2005 ~ "2001-2005",
      yrim >= 2006 & yrim <= 2010 ~ "2006-2010",
      yrim >= 2011 & yrim <= 2015 ~ "2011-2015",
      yrim >= 2016 & yrim <= 2021 ~ "2016-2021 (Recent)",
      TRUE ~ "Unknown"
    ),
    education_level = case_when(
      hdgree <= 5 ~ "High school or less",
      hdgree %in% c(6, 7) ~ "Some post-secondary",
      hdgree %in% c(8, 9) ~ "Bachelor's degree",
      hdgree >= 10 ~ "Graduate degree",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(immigration_period, education_level) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(immigration_period) %>%
  mutate(percentage = count / sum(count) * 100)  # Removed the filter for graduate degree only

cat("\n=== Education Levels by Immigration Period ===\n")
print(education_by_period)

# 8. Parent Birthplace for Second Generation
second_gen_parents <- iranians %>%
  filter(generation == "Second") %>%
  mutate(
    parent_origin = case_when(
      pobpar1 == 4 & pobpar2 == 4 ~ "Both parents from Asia",
      pobpar1 == 4 | pobpar2 == 4 ~ "One parent from Asia",
      pobpar1 == 1 & pobpar2 == 1 ~ "Both parents from Canada",
      pobpar1 == 1 | pobpar2 == 1 ~ "One parent from Canada",
      TRUE ~ "Other combination"
    )
  ) %>%
  group_by(parent_origin) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  mutate(percentage = count / sum(count) * 100)

cat("\n=== Second Generation Parent Origins ===\n")
print(second_gen_parents)

# Export results
dir.create(here("output_immigration_page"), showWarnings = FALSE)

write_csv(immigration_waves, here("output_immigration_page/immigration_waves.csv"))
write_csv(age_at_immigration, here("output_immigration_page/age_at_immigration.csv"))
write_csv(immigration_categories, here("output_immigration_page/immigration_categories.csv"))
write_csv(settlement_patterns, here("output_immigration_page/settlement_by_province.csv"))
write_csv(citizenship_timeline, here("output_immigration_page/naturalization_rates.csv"))
write_csv(mobility_patterns, here("output_immigration_page/mobility_patterns.csv"))
write_csv(education_by_period, here("output_immigration_page/education_by_immigration_period.csv"))
write_csv(second_gen_parents, here("output_immigration_page/second_generation_parents.csv"))

# Summary statistics
cat("\n=== Key Immigration Findings ===\n")

# Peak immigration period
peak_period <- immigration_waves %>%
  filter(immigration_period != "Unknown") %>%
  arrange(desc(count)) %>%
  head(1)

cat("\nPeak Immigration Period:", peak_period$immigration_period, 
    "with", round(peak_period$count), "immigrants (", 
    round(peak_period$percentage, 1), "%)\n")

# Current citizenship rate
citizenship_rate <- immigrants %>%
  summarise(
    total = sum(weight),
    citizens = sum(weight[citizen == 2]),
    rate = citizens / total * 100
  )

cat("\nOverall Naturalization Rate:", round(citizenship_rate$rate, 1), "%\n")

# Main settlement provinces
main_provinces <- immigrants %>%
  mutate(
    province_name = case_when(
      pr == 35 ~ "Ontario",
      pr == 59 ~ "British Columbia", 
      pr == 48 ~ "Alberta",
      pr == 24 ~ "Quebec",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(province_name) %>%
  summarise(
    count = sum(weight),
    percentage = count / sum(immigrants$weight) * 100
  ) %>%
  arrange(desc(count))

cat("\nTop Settlement Provinces:\n")
print(main_provinces)
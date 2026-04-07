# Immigration Categories Analysis for Iranian-Canadians
# Analyzes Economic/Family/Refugee admission categories over time
# Created: June 27, 2025

library(tidyverse)
library(here)

# Source the comprehensive variable system
source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

# Main analysis function
analyze_immigration_categories <- function() {
  
  # Load immigration category variables
  message("Loading PUMF data with immigration category variables...")
  
  category_data <- load_individual_pumf(
    c("ppsort", "ethder", "pob", "mtnno", "hlmostno", "vismin", 
      "genstat", "immcat5", "immstat", "yrim", "weight", 
      "pobpar1", "pobpar2", "agegrp", "gender", "hdgree", "totinc")
  )
  
  # Apply Iranian identification criteria
  message("Identifying Iranian population...")
  
  iranian_data <- category_data %>%
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
  
  message("Total Iranian population: ", format(sum(iranian_refined$weight), big.mark = ","))
  
  # Focus on immigrants with category data
  immigrants_with_category <- iranian_refined %>%
    filter(immstat == 2) %>%  # Immigrants only
    mutate(
      category_label = case_when(
        immcat5 == 21 ~ "Economic immigrants",
        immcat5 == 22 ~ "Family-sponsored immigrants", 
        immcat5 == 23 ~ "Refugees",
        immcat5 == 3 ~ "Pre-1980 immigrants",
        immcat5 == 88 ~ "Not available",
        immcat5 == 1 ~ "Non-immigrants (error)",
        immcat5 == 2 ~ "Non-permanent residents (error)",
        TRUE ~ "Unknown"
      ),
      # Create arrival period for time analysis
      arrival_period = case_when(
        yrim < 1980 | immcat5 == 3 ~ "Before 1980",
        yrim >= 1980 & yrim <= 1989 ~ "1980-1989",
        yrim >= 1990 & yrim <= 1999 ~ "1990-1999",
        yrim >= 2000 & yrim <= 2009 ~ "2000-2009",
        yrim >= 2010 & yrim <= 2019 ~ "2010-2019",
        yrim >= 2020 ~ "2020-2021",
        TRUE ~ "Unknown"
      )
    )
  
  # 1. OVERALL IMMIGRATION CATEGORY BREAKDOWN
  category_breakdown <- immigrants_with_category %>%
    filter(immcat5 %in% c(3, 21, 22, 23)) %>%  # Valid categories only
    group_by(immcat5, category_label) %>%
    summarise(
      n_unweighted = n(),
      count = sum(weight),
      .groups = "drop"
    ) %>%
    mutate(
      percentage = round(count / sum(count) * 100, 1),
      reliability = case_when(
        n_unweighted >= 30 ~ "Reliable",
        n_unweighted >= 10 ~ "Use with caution",
        TRUE ~ "Suppress"
      )
    ) %>%
    arrange(desc(count))
  
  # 2. CATEGORIES BY TIME PERIOD
  categories_by_period <- immigrants_with_category %>%
    filter(immcat5 %in% c(21, 22, 23) & arrival_period != "Unknown" & arrival_period != "Before 1980") %>%
    group_by(arrival_period, immcat5, category_label) %>%
    summarise(
      n_unweighted = n(),
      count = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(arrival_period) %>%
    mutate(
      total_period = sum(count),
      percentage = round(count / total_period * 100, 1),
      reliability = case_when(
        n_unweighted >= 30 ~ "Reliable",
        n_unweighted >= 10 ~ "Use with caution",
        TRUE ~ "Suppress"
      )
    ) %>%
    arrange(arrival_period, desc(count))
  
  # 3. CATEGORY TRENDS OVER TIME
  category_trends <- categories_by_period %>%
    select(arrival_period, category_label, percentage, n_unweighted, reliability) %>%
    pivot_wider(names_from = category_label, values_from = c(percentage, n_unweighted, reliability), values_fill = list(percentage = 0, n_unweighted = 0L, reliability = "Suppress"))
  
  # 4. SOCIOECONOMIC OUTCOMES BY CATEGORY
  # For post-1980 immigrants only (who have category data)
  socioeconomic_outcomes <- immigrants_with_category %>%
    filter(immcat5 %in% c(21, 22, 23) & agegrp >= 9) %>%  # Adults 25+
    mutate(
      # Education level
      education_level = case_when(
        hdgree <= 7 ~ "Less than Bachelor's",
        hdgree %in% c(8, 9) ~ "Bachelor's degree",
        hdgree >= 10 & hdgree < 88 ~ "Graduate degree",
        TRUE ~ "Unknown"
      ),
      # Income categories
      income_category = case_when(
        totinc < 20000 ~ "Under $20K",
        totinc < 40000 ~ "$20K-$40K",
        totinc < 60000 ~ "$40K-$60K",
        totinc < 80000 ~ "$60K-$80K",
        totinc < 100000 ~ "$80K-$100K",
        totinc >= 100000 ~ "$100K+",
        TRUE ~ "Unknown"
      )
    ) %>%
    group_by(immcat5, category_label) %>%
    summarise(
      total = sum(weight),
      # Education
      bachelors_plus = sum(weight[hdgree >= 8], na.rm = TRUE),
      graduate_degree = sum(weight[hdgree >= 10], na.rm = TRUE),
      # Income
      median_income = median(totinc[totinc > 0], na.rm = TRUE),
      high_income = sum(weight[totinc >= 100000], na.rm = TRUE),
      # Percentages
      pct_bachelors_plus = round(bachelors_plus / total * 100, 1),
      pct_graduate = round(graduate_degree / total * 100, 1),
      pct_high_income = round(high_income / total * 100, 1),
      .groups = "drop"
    )
  
  # 5. GENDER DISTRIBUTION BY CATEGORY
  gender_by_category <- immigrants_with_category %>%
    filter(immcat5 %in% c(21, 22, 23)) %>%
    mutate(
      gender_label = case_when(
        gender == 1 ~ "Women+",
        gender == 2 ~ "Men+",
        TRUE ~ "Unknown"
      )
    ) %>%
    group_by(immcat5, category_label, gender, gender_label) %>%
    summarise(
      count = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(immcat5, category_label) %>%
    mutate(percentage = round(count / sum(count) * 100, 1))
  
  # 6. AGE AT ARRIVAL BY CATEGORY
  age_arrival_by_category <- immigrants_with_category %>%
    filter(immcat5 %in% c(21, 22, 23)) %>%
    mutate(
      age_arrival_group = case_when(
        agegrp <= 3 ~ "0-9 years",
        agegrp <= 7 ~ "10-19 years",
        agegrp <= 9 ~ "20-29 years",
        agegrp <= 11 ~ "30-39 years",
        agegrp <= 13 ~ "40-49 years",
        agegrp >= 14 ~ "50+ years",
        TRUE ~ "Unknown"
      )
    ) %>%
    group_by(immcat5, category_label, age_arrival_group) %>%
    summarise(
      count = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(immcat5, category_label) %>%
    mutate(percentage = round(count / sum(count) * 100, 1))
  
  # Export results
  output_dir <- here("output_immigration_page")
  
  # Overall breakdown
  write_csv(category_breakdown,
            file.path(output_dir, "iranian_immigration_categories.csv"))
  
  # Categories by period
  write_csv(categories_by_period,
            file.path(output_dir, "iranian_immigration_categories_by_period.csv"))
  
  # Category trends
  write_csv(category_trends,
            file.path(output_dir, "iranian_immigration_category_trends.csv"))
  
  # Socioeconomic outcomes
  write_csv(socioeconomic_outcomes,
            file.path(output_dir, "iranian_immigration_socioeconomic_by_category.csv"))
  
  # Gender distribution
  write_csv(gender_by_category,
            file.path(output_dir, "iranian_immigration_gender_by_category.csv"))
  
  # Age at arrival
  write_csv(age_arrival_by_category,
            file.path(output_dir, "iranian_immigration_age_arrival_by_category.csv"))
  
  message("\nImmigration categories analysis complete!")
  message("Files saved to: ", output_dir)
  
  return(list(
    categories = category_breakdown,
    by_period = categories_by_period,
    trends = category_trends,
    outcomes = socioeconomic_outcomes,
    gender = gender_by_category,
    age_arrival = age_arrival_by_category
  ))
}

# Run the analysis
results <- analyze_immigration_categories()

# Display key results
cat("\n=== OVERALL IMMIGRATION CATEGORIES ===\n")
print(results$categories)

cat("\n=== CATEGORY TRENDS OVER TIME ===\n")
print(results$trends)

cat("\n=== SOCIOECONOMIC OUTCOMES BY CATEGORY ===\n")
print(results$outcomes %>% select(category_label, pct_bachelors_plus, pct_graduate, median_income, pct_high_income))
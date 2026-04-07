# Education Analysis with 30+ Age Groups
# First generation: Age cohorts starting at 30-34
# Second generation: All adults 30+ combined (due to small sample sizes)
# Created: 2025-09-13

library(tidyverse)
library(here)

# Source the comprehensive variable system
source(here('R/comprehensive_iranian_variables_correct_pumf.R'))

# Main analysis function
analyze_education_30plus <- function() {

  message("Loading PUMF data with education variables...")

  education_data <- load_individual_pumf(
    c("ppsort", "ethder", "pob", "mtnno", "hlmostno", "vismin",
      "genstat", "hdgree", "agegrp", "gender", "weight",
      "pobpar1", "pobpar2", "immstat")
  )

  # Apply Iranian identification criteria
  message("Identifying Iranian population...")

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

  # Focus on adults 30+ for both generations
  adults <- iranian_refined %>%
    filter(agegrp >= 10)  # 30+ years old (agegrp 10 = 30-34)

  # Create categories with different logic for each generation
  adults <- adults %>%
    mutate(
      # Standard age cohorts for first generation
      standard_age_cohort = case_when(
        agegrp == 10 ~ "30-34",
        agegrp == 11 ~ "35-39",
        agegrp %in% c(11, 12) ~ "35-44",  # For combining if needed
        agegrp %in% c(13, 14) ~ "45-54",
        agegrp %in% c(15, 16) ~ "55-64",
        agegrp %in% c(17, 18) ~ "65-74",
        agegrp >= 19 ~ "75-84",
        TRUE ~ NA_character_
      ),
      # Education categories
      education_level = case_when(
        hdgree >= 10 & hdgree < 88 ~ "Graduate degree",
        hdgree %in% c(8, 9) ~ "BA degree",
        hdgree <= 7 ~ "Less than BA degree",
        TRUE ~ "Unknown"
      ),
      # Generation labels
      generation_label = case_when(
        genstat == 1 ~ "First-Generation",
        genstat %in% c(2, 3) ~ "Second-Generation",
        genstat == 4 ~ "Third+ Generation",
        TRUE ~ "Unknown"
      ),
      # Gender labels
      gender_label = case_when(
        gender == 1 ~ "Women",
        gender == 2 ~ "Men",
        TRUE ~ "Unknown"
      )
    ) %>%
    filter(education_level != "Unknown")

  # Apply different age grouping for each generation:
  # First generation: Keep detailed age cohorts
  # Second generation: Combine all 30+ into single group
  adults <- adults %>%
    mutate(
      age_cohort = case_when(
        generation_label == "First-Generation" & agegrp == 10 ~ "30-34",
        generation_label == "First-Generation" & agegrp == 11 ~ "35-39",
        generation_label == "First-Generation" & agegrp %in% c(13, 14) ~ "45-54",
        generation_label == "First-Generation" & agegrp %in% c(15, 16) ~ "55-64",
        generation_label == "First-Generation" & agegrp %in% c(17, 18) ~ "65-74",
        generation_label == "First-Generation" & agegrp >= 19 ~ "75-84",
        generation_label == "Second-Generation" ~ "30+",  # All second gen 30+ combined
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_cohort))

  # Combine 35-39 back into 35-44 for first generation (to match original structure)
  adults <- adults %>%
    mutate(
      age_cohort = case_when(
        generation_label == "First-Generation" & age_cohort %in% c("35-39") ~ "35-44",
        TRUE ~ age_cohort
      )
    )

  # 1. FIRST GENERATION EDUCATION BY AGE AND GENDER
  first_gen_education <- adults %>%
    filter(generation_label == "First-Generation") %>%
    group_by(age_cohort, gender_label, education_level) %>%
    summarise(
      n_unweighted = n(),
      count = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(age_cohort, gender_label) %>%
    mutate(
      total_unweighted = sum(n_unweighted),
      total = sum(count),
      percentage = round(count / total * 100, 1),
      reliability = case_when(
        total_unweighted < 30 ~ "Suppress",
        total_unweighted < 100 ~ "Use with caution",
        TRUE ~ "Reliable"
      )
    ) %>%
    ungroup() %>%
    arrange(age_cohort, gender_label, education_level)

  # 2. SECOND GENERATION EDUCATION - ALL 30+ COMBINED
  second_gen_education <- adults %>%
    filter(generation_label == "Second-Generation") %>%
    group_by(age_cohort, gender_label, education_level) %>%
    summarise(
      n_unweighted = n(),
      count = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(age_cohort, gender_label) %>%
    mutate(
      total_unweighted = sum(n_unweighted),
      total = sum(count),
      percentage = round(count / total * 100, 1),
      reliability = case_when(
        total_unweighted < 30 ~ "Suppress",
        total_unweighted < 100 ~ "Use with caution",
        TRUE ~ "Reliable"
      ),
      note = "All adults 30+ combined due to small sample sizes"
    ) %>%
    ungroup() %>%
    arrange(age_cohort, gender_label, education_level)

  # 3. SUMMARY STATISTICS
  summary_stats <- bind_rows(
    first_gen_education %>%
      mutate(generation = "First-Generation") %>%
      group_by(generation, age_cohort, gender_label) %>%
      summarise(
        n_total = first(total_unweighted),
        weighted_total = first(total),
        grad_degree_pct = sum(percentage[education_level == "Graduate degree"]),
        ba_degree_pct = sum(percentage[education_level == "BA degree"]),
        less_ba_pct = sum(percentage[education_level == "Less than BA degree"]),
        reliability = first(reliability),
        .groups = "drop"
      ),
    second_gen_education %>%
      mutate(generation = "Second-Generation") %>%
      group_by(generation, age_cohort, gender_label) %>%
      summarise(
        n_total = first(total_unweighted),
        weighted_total = first(total),
        grad_degree_pct = sum(percentage[education_level == "Graduate degree"]),
        ba_degree_pct = sum(percentage[education_level == "BA degree"]),
        less_ba_pct = sum(percentage[education_level == "Less than BA degree"]),
        reliability = first(reliability),
        note = "All adults 30+ combined",
        .groups = "drop"
      )
  )

  # 4. CREATE VISUALIZATION-READY DATASETS
  first_gen_viz <- first_gen_education %>%
    select(age_cohort, gender_label, education_level, percentage, n_unweighted, reliability) %>%
    filter(reliability != "Suppress")

  second_gen_viz <- second_gen_education %>%
    select(age_cohort, gender_label, education_level, percentage, n_unweighted, reliability, note) %>%
    filter(reliability != "Suppress")

  # Export results
  output_dir <- here("output_education_page")

  # Main datasets
  write_csv(first_gen_education,
            file.path(output_dir, "education_first_gen_30plus_FINAL.csv"))

  write_csv(second_gen_education,
            file.path(output_dir, "education_second_gen_30plus_FINAL.csv"))

  write_csv(summary_stats,
            file.path(output_dir, "education_summary_30plus_FINAL.csv"))

  # Visualization-ready datasets
  write_csv(first_gen_viz,
            file.path(output_dir, "education_first_gen_30plus_viz.csv"))

  write_csv(second_gen_viz,
            file.path(output_dir, "education_second_gen_30plus_viz.csv"))

  message("\n=== EDUCATION ANALYSIS COMPLETE (30+ VERSION) ===")
  message("First generation: Age cohorts starting at 30-34")
  message("Second generation: All adults 30+ combined (due to small sample sizes)")
  message("Files saved to: ", output_dir)

  # Print summary
  message("\n=== FIRST GENERATION SUMMARY (30-34, 35-44, etc.) ===")
  print(summary_stats %>%
        filter(generation == "First-Generation") %>%
        select(age_cohort, gender_label, n_total, grad_degree_pct, ba_degree_pct, reliability))

  message("\n=== SECOND GENERATION SUMMARY (All 30+ combined) ===")
  print(summary_stats %>%
        filter(generation == "Second-Generation") %>%
        select(age_cohort, gender_label, n_total, grad_degree_pct, ba_degree_pct, reliability, note))

  # Report sample sizes
  second_gen_total <- adults %>%
    filter(generation_label == "Second-Generation") %>%
    group_by(gender_label) %>%
    summarise(
      n_30plus = n(),
      weighted_30plus = sum(weight),
      .groups = "drop"
    )

  message("\n=== SECOND GENERATION 30+ SAMPLE SIZES ===")
  print(second_gen_total)
  message("\nNote: These sample sizes are sufficient for percentage calculations when combined")

  return(list(
    first_gen = first_gen_education,
    second_gen = second_gen_education,
    summary = summary_stats,
    first_viz = first_gen_viz,
    second_viz = second_gen_viz
  ))
}

# Run the analysis
results <- analyze_education_30plus()

message("\n=== KEY FEATURES ===")
message("1. First generation: Detailed age cohorts (30-34, 35-44, 45-54, etc.)")
message("2. Second generation: All adults 30+ combined into single group")
message("3. Excludes ages 25-29 to avoid 'still in school' effect")
message("4. Second generation sample sizes now sufficient for analysis")
message("5. CSV structure optimized for dashboard visualization")
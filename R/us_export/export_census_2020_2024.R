# Export Dashboard .Rda Files from Census 2020-2024 5-Year PUMS
# Created: 2026-03-11
#
# Input:  data/usa/acs5/iranians_acs5_2020_2024.rds (34,114 records, 754,595 weighted)
#         data/usa/acs5/iranian_households_acs5_2020_2024.rds (42,804 records)
#         data/usa/acs5/national_reference_acs5_2020_2024.rds
# Output: .Rda/.xlsx files in ~/Dropbox/10_Coding/IDD/data/
#
# Census PUMS → IPUMS variable name mapping done inline.
# Usage:
#   source("scripts/01_export/export_census_2020_2024.R")
#   export_all()

library(tidyverse)

# --- Paths ---
IRANIANS_FILE   <- "data/usa/acs5/iranians_acs5_2020_2024.rds"
HOUSEHOLDS_FILE <- "data/usa/acs5/iranian_households_acs5_2020_2024.rds"
NATL_REF_FILE   <- "data/usa/acs5/national_reference_acs5_2020_2024.rds"
DASHBOARD_DIR   <- "output/dashboard_us"

# --- Codes ---
IRAN_BPL <- 212   # POBP
IRAN_ANC <- 416   # ANC1P/ANC2P
MENA_ANC_RANGE <- 400:496
MENA_BPL_RANGE <- c(200:299)  # SW Asia FIPS birthplace codes

# --- SCHL (Census PUMS) to education categories ---
# Census PUMS SCHL codes: 0=N/A, 1-15=below HS, 16=regular HS, 17=GED,
# 18=some college <1yr, 19=some college no degree, 20=associate's,
# 21=bachelor's, 22=master's, 23=professional, 24=doctorate
schl_to_educ <- function(schl) {
  s <- as.numeric(schl)
  case_when(
    is.na(s) | s == 0        ~ "No schooling or N/A",
    s >= 1  & s <= 11        ~ "Less than high school",
    s >= 12 & s <= 15        ~ "Some high school",
    s >= 16 & s <= 17        ~ "High school degree",
    s >= 18 & s <= 20        ~ "Some college",
    s == 21                  ~ "Bachelors degree",
    s >= 22 & s <= 24        ~ "Graduate degree",
    TRUE                     ~ "No schooling or N/A"
  )
}

EDUC_LEVELS <- c("No schooling or N/A", "Less than high school", "Some high school",
                 "High school degree", "Some college", "Bachelors degree", "Graduate degree")

# --- COW (Census PUMS) to class of worker ---
cow_to_class <- function(cow) {
  c <- as.numeric(cow)
  case_when(
    is.na(c) | c == 0       ~ "N/A",
    c == 1 | c == 2          ~ "Private sector employee",  # for-profit + not-for-profit
    c == 3                   ~ "Non-profit employee",       # actually c==2 is nonprofit
    c >= 4 & c <= 5          ~ "Public sector employee",    # local + state govt
    c == 6 | c == 7          ~ "Self-employed",             # self-emp incorporated + not
    c == 8                   ~ "Unpaid family member",
    c == 9                   ~ "N/A",                       # unemployed
    TRUE                     ~ "N/A"
  )
}

# Fix: COW 2 is actually nonprofit, COW 1 is for-profit private
cow_to_class2 <- function(cow) {
  c <- as.numeric(cow)
  case_when(
    is.na(c) | c == 0       ~ "N/A",
    c == 1                   ~ "Private sector employee",
    c == 2                   ~ "Non-profit employee",
    c == 3 | c == 4 | c == 5 ~ "Public sector employee",
    c == 6 | c == 7          ~ "Self-employed",
    c == 8                   ~ "Unpaid family member",
    c == 9                   ~ "N/A",
    TRUE                     ~ "N/A"
  )
}

# --- ESR to employment status ---
esr_to_emp <- function(esr) {
  e <- as.numeric(esr)
  case_when(
    is.na(e) | e == 0       ~ "N/A",
    e == 1 | e == 2 | e == 4 | e == 5 ~ "Employed",
    e == 3                   ~ "Unemployed",
    e == 6                   ~ "Not in labor force",
    TRUE                     ~ "N/A"
  )
}

# --- CIT to citizenship ---
# Census PUMS CIT: 1=Born in US, 2=Born in PR/territories,
# 3=Born abroad of US parents, 4=Naturalized, 5=Not a citizen
cit_to_citizen <- function(cit) {
  c <- as.numeric(cit)
  case_when(
    c >= 1 & c <= 3          ~ "US-born",
    c == 4                   ~ "Naturalized citizen",
    c == 5                   ~ "Not a citizen",
    TRUE                     ~ "Unknown"
  )
}

# --- Load and prepare ---
load_iranians <- function() {
  d <- readRDS(IRANIANS_FILE)

  d <- d %>%
    mutate(
      # Convert character columns to numeric for computation
      age = as.numeric(AGEP),
      sex_num = as.numeric(SEX),

      # Derived columns matching dashboard expectations
      gen = factor(ifelse(born_in_iran, "1st gen", "2nd gen")),
      gender = factor(sex_num, levels = c(1, 2), labels = c("Male", "Female")),

      educ_factor = factor(schl_to_educ(SCHL), levels = EDUC_LEVELS),

      age_group = cut(age, breaks = seq(24, 85, by = 10),
                      labels = c("25-34", "35-44", "45-54", "55-64", "65-74", "75-84"),
                      include.lowest = FALSE),

      class_wkrd = cow_to_class2(COW),
      emp_stat = esr_to_emp(ESR),
      CITIZEN2 = cit_to_citizen(CIT),

      # Rename for IPUMS compatibility
      PERWT = PWGTP,
      YRIMMIG = as.numeric(YOEP),
      HHINCOME = as.numeric(HINCP)
    )

  return(d)
}


# --- Export functions ---

export_iran_data <- function(d) {
  # Main individual-level dataset
  # Map Census PUMS → IPUMS names the dashboard expects
  iran_data <- d %>%
    transmute(
      YEAR = as.integer(substr(SERIALNO, 1, 4)),
      SERIAL = SERIALNO,
      HHWT = as.numeric(WGTP),
      STATEFIP = as.numeric(STATE),
      HHINCOME = HHINCOME,
      PERWT = PERWT,
      SEX = sex_num,
      AGE = age,
      BPL = as.numeric(POBP),
      ANCESTR1 = as.numeric(ANC1P),
      ANCESTR2 = as.numeric(ANC2P),
      CITIZEN = as.numeric(CIT),
      YRIMMIG = YRIMMIG,
      SCHL = as.numeric(SCHL),
      EMPSTAT = as.numeric(ESR),
      CLASSWKR = as.numeric(COW),
      OCC = OCCP,
      WKHP = as.numeric(WKHP),
      INCTOT = as.numeric(PINCP),
      INCWAGE = as.numeric(WAGP),
      POVERTY = as.numeric(POVPIP),
      REGION = as.numeric(REGION),
      PUMA = PUMA,
      educ_factor = educ_factor,
      age = age,
      gender = gender,
      bpl = as.numeric(POBP),
      gen = gen,
      class_wkrd = class_wkrd,
      emp_stat = emp_stat,
      CITIZEN2 = CITIZEN2,
      born_in_iran = born_in_iran,
      any_iranian_anc = any_iranian_anc,
      generation = generation,
      is_child_of_iranian = is_child_of_iranian
    )

  f <- file.path(DASHBOARD_DIR, "iran_data.Rda")
  save(iran_data, file = f)
  cat(sprintf("  iran_data.Rda: %s rows, %.1f KB\n",
              format(nrow(iran_data), big.mark = ","), file.size(f) / 1e3))
}


export_acs_stacked <- function(d) {
  acs_stacked <- d %>%
    mutate(
      component = case_when(
        born_in_iran & any_iranian_anc  ~ "ancestry_bpl",
        born_in_iran & !any_iranian_anc ~ "bpl_only",
        !born_in_iran & any_iranian_anc ~ "ancestry_only",
        is_child_of_iranian             ~ "child_of_iranian",
        TRUE                            ~ "other"
      )
    ) %>%
    group_by(component) %>%
    summarise(n = sum(PERWT), .groups = "drop") %>%
    filter(component != "other") %>%
    pivot_wider(names_from = component, values_from = n, values_fill = 0)

  f <- file.path(DASHBOARD_DIR, "acs_stacked.Rda")
  save(acs_stacked, file = f)
  cat(sprintf("  acs_stacked.Rda: "))
  print(as.data.frame(acs_stacked))
}


export_education <- function(d) {
  d_edu <- d %>% filter(age >= 25, !is.na(age_group))

  make_wide <- function(data, gen_label) {
    data %>%
      filter(gen == gen_label) %>%
      group_by(educ_factor, age_group, gender) %>%
      summarise(n = sum(PERWT), .groups = "drop") %>%
      pivot_wider(names_from = gender, values_from = n, values_fill = 0) %>%
      mutate(ratio = Female / Male) %>%
      arrange(educ_factor, age_group)
  }

  gen_1_wide <- make_wide(d_edu, "1st gen")
  gen_2_wide <- make_wide(d_edu, "2nd gen")

  f1 <- file.path(DASHBOARD_DIR, "gen_1_wide.Rda")
  f2 <- file.path(DASHBOARD_DIR, "gen_2_wide.Rda")
  save(gen_1_wide, file = f1)
  save(gen_2_wide, file = f2)
  cat(sprintf("  gen_1_wide.Rda: %d rows\n", nrow(gen_1_wide)))
  cat(sprintf("  gen_2_wide.Rda: %d rows\n", nrow(gen_2_wide)))
}


export_employment <- function(d) {
  class <- d %>%
    filter(age >= 25, !is.na(age_group)) %>%
    group_by(gender, gen, age_group, class_wkrd) %>%
    summarise(n = sum(PERWT), .groups = "drop")

  emp <- d %>%
    group_by(gender, gen, emp_stat) %>%
    summarise(n = sum(PERWT), .groups = "drop")

  f1 <- file.path(DASHBOARD_DIR, "class.Rda")
  f2 <- file.path(DASHBOARD_DIR, "emp.Rda")
  save(class, file = f1)
  save(emp, file = f2)
  cat(sprintf("  class.Rda: %d rows\n", nrow(class)))
  cat(sprintf("  emp.Rda: %d rows\n", nrow(emp)))
}


export_citizenship <- function(d) {
  by_citizen <- d %>%
    filter(born_in_iran) %>%
    group_by(CITIZEN2) %>%
    summarise(n = sum(PERWT), .groups = "drop")

  f <- file.path(DASHBOARD_DIR, "by_citizen.xlsx")
  writexl::write_xlsx(by_citizen, f)
  cat(sprintf("  by_citizen.xlsx: %d rows\n", nrow(by_citizen)))
  print(as.data.frame(by_citizen))
}


export_immigration <- function(d) {
  by_yrimmig <- d %>%
    filter(born_in_iran, YRIMMIG > 0) %>%
    group_by(YRIMMIG) %>%
    summarise(n = sum(PERWT), .groups = "drop") %>%
    arrange(YRIMMIG) %>%
    mutate(cumulative_immigrants = cumsum(n))

  f <- file.path(DASHBOARD_DIR, "by_yrimmig.xlsx")
  writexl::write_xlsx(by_yrimmig, f)
  cat(sprintf("  by_yrimmig.xlsx: %d rows\n", nrow(by_yrimmig)))
}


export_geography <- function(d) {
  if (!requireNamespace("tigris", quietly = TRUE)) {
    cat("  NOTE: tigris needed for geography. Skipping.\n")
    return(invisible(NULL))
  }
  library(tigris)
  library(sf)
  options(tigris_use_cache = TRUE)

  state_pop <- d %>%
    group_by(STATEFIP = as.numeric(STATE)) %>%
    summarise(population_est = sum(PERWT), .groups = "drop")

  states_sf <- states(cb = TRUE) %>%
    filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>%
    mutate(STATEFIP = as.numeric(STATEFP))

  merged_data <- states_sf %>%
    left_join(state_pop, by = "STATEFIP") %>%
    filter(!is.na(population_est)) %>%
    mutate(state = tolower(NAME)) %>%
    select(state, population_est, geometry)

  f <- file.path(DASHBOARD_DIR, "merged_data.Rda")
  save(merged_data, file = f)
  cat(sprintf("  merged_data.Rda: %d states\n", nrow(merged_data)))
}


export_spouse <- function() {
  cat("  Loading household members...\n")
  hh <- readRDS(HOUSEHOLDS_FILE)

  # Householders (RELSHIPP=20) and spouses (RELSHIPP=21)
  heads <- hh %>%
    filter(RELSHIPP == "20") %>%
    select(SERIALNO, POBP, ANC1P, ANC2P, RAC1P, HISP, SEX, AGEP, PWGTP) %>%
    rename_with(~ paste0(.x, "_HEAD"), -SERIALNO)

  spouses <- hh %>%
    filter(RELSHIPP == "21") %>%
    select(SERIALNO, POBP, ANC1P, ANC2P, RAC1P, HISP, SEX, AGEP, PWGTP) %>%
    rename_with(~ paste0(.x, "_SP"), -SERIALNO)

  paired <- inner_join(heads, spouses, by = "SERIALNO")
  cat(sprintf("  Paired households: %s\n", format(nrow(paired), big.mark = ",")))

  # Bidirectional: head perspective + spouse perspective
  head_view <- paired %>%
    transmute(
      SERIALNO,
      POBP = POBP_HEAD, ANC1P = ANC1P_HEAD, ANC2P = ANC2P_HEAD,
      RAC1P = RAC1P_HEAD, HISP = HISP_HEAD,
      SEX = SEX_HEAD, AGEP = AGEP_HEAD, PWGTP = PWGTP_HEAD,
      POBP_SP = POBP_SP, ANC1P_SP = ANC1P_SP, ANC2P_SP = ANC2P_SP,
      RAC1P_SP = RAC1P_SP, HISP_SP = HISP_SP
    )

  sp_view <- paired %>%
    transmute(
      SERIALNO,
      POBP = POBP_SP, ANC1P = ANC1P_SP, ANC2P = ANC2P_SP,
      RAC1P = RAC1P_SP, HISP = HISP_SP,
      SEX = SEX_SP, AGEP = AGEP_SP, PWGTP = PWGTP_SP,
      POBP_SP = POBP_HEAD, ANC1P_SP = ANC1P_HEAD, ANC2P_SP = ANC2P_HEAD,
      RAC1P_SP = RAC1P_HEAD, HISP_SP = HISP_HEAD
    )

  all_married <- bind_rows(head_view, sp_view) %>%
    mutate(across(c(POBP, ANC1P, ANC2P, RAC1P, HISP, POBP_SP, ANC1P_SP, ANC2P_SP, RAC1P_SP, HISP_SP, AGEP, SEX),
                  as.numeric)) %>%
    filter(
      POBP == IRAN_BPL | ANC1P == IRAN_ANC | ANC2P == IRAN_ANC
    )

  cat(sprintf("  Married Iranians: %s\n", format(nrow(all_married), big.mark = ",")))

  # Classify
  all_married <- all_married %>%
    mutate(
      gen = factor(ifelse(POBP == IRAN_BPL, "1st gen", "2nd gen")),
      gender = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
      age_group = cut(AGEP, breaks = seq(19, 90, by = 10),
                      labels = paste0(seq(20, 80, by = 10), "-", seq(29, 89, by = 10)),
                      include.lowest = FALSE),

      spouse_iran = ifelse(
        ANC1P_SP == IRAN_ANC | ANC2P_SP == IRAN_ANC | POBP_SP == IRAN_BPL,
        "Yes", "No"),

      spouse_mena = ifelse(
        (ANC1P_SP >= 400 & ANC1P_SP <= 496) |
        (ANC2P_SP >= 400 & ANC2P_SP <= 496),
        "Yes", "No"),

      spouse_white = ifelse(RAC1P_SP == 1 & HISP_SP %in% c(0, 1), "Yes", "No"),
      # Census PUMS HISP: 01=Not Hispanic, 02-24=Hispanic
      # Fix: HISP==1 means "Not Hispanic" in Census PUMS
      spouse_white = ifelse(RAC1P_SP == 1 & HISP_SP == 1, "Yes", "No"),

      spouse_hsp = ifelse(HISP_SP > 1, "Yes", "No"),

      spouse = case_when(
        spouse_iran == "Yes"  ~ "Iranian",
        spouse_mena == "Yes"  ~ "Other MENA",
        spouse_white == "Yes" ~ "Other White",
        spouse_hsp == "Yes"   ~ "Hispanic",
        TRUE                  ~ "Other"
      ),
      spouse = ordered(spouse, levels = c("Iranian", "Other MENA", "Other White", "Hispanic", "Other")),

      # Rename for dashboard compatibility
      PERWT = PWGTP,
      AGE = AGEP,
      BPL = POBP,
      ANCESTR1 = ANC1P,
      ANCESTR2 = ANC2P,
      RACE = RAC1P,
      HISPAN = HISP,
      BPL_SP = POBP_SP,
      ANCESTR1_SP = ANC1P_SP,
      ANCESTR2_SP = ANC2P_SP,
      RACE_SP = RAC1P_SP,
      HISPAN_SP = HISP_SP
    )

  # spouse.Rda
  spouse <- all_married %>%
    group_by(spouse_iran) %>%
    summarise(n = sum(PERWT), .groups = "drop")
  f <- file.path(DASHBOARD_DIR, "spouse.Rda")
  save(spouse, file = f)
  cat(sprintf("  spouse.Rda\n"))
  print(as.data.frame(spouse))

  # spouse_gen.Rda
  spouse_gen <- all_married %>%
    group_by(gen, spouse_iran) %>%
    summarise(n = sum(PERWT), .groups = "drop")
  f <- file.path(DASHBOARD_DIR, "spouse_gen.Rda")
  save(spouse_gen, file = f)
  cat(sprintf("  spouse_gen.Rda\n"))

  # spouse_gen2.Rda
  spouse_gen2 <- all_married %>%
    group_by(gen, spouse) %>%
    summarise(n = sum(PERWT), .groups = "drop")
  f <- file.path(DASHBOARD_DIR, "spouse_gen2.Rda")
  save(spouse_gen2, file = f)
  cat(sprintf("  spouse_gen2.Rda\n"))
  print(as.data.frame(spouse_gen2))

  # spouse_gender.Rda (individual-level)
  spouse_gender <- all_married
  f <- file.path(DASHBOARD_DIR, "spouse_gender.Rda")
  save(spouse_gender, file = f)
  cat(sprintf("  spouse_gender.Rda: %s rows\n", format(nrow(spouse_gender), big.mark = ",")))
}


export_all <- function() {
  cat("=== Exporting Dashboard Data (Census 2020-2024 5-Year PUMS) ===\n")
  cat(sprintf("Source: %s\n", IRANIANS_FILE))
  cat(sprintf("Target: %s\n\n", DASHBOARD_DIR))

  d <- load_iranians()
  cat(sprintf("Loaded: %s records, weighted: %s\n\n",
              format(nrow(d), big.mark = ","),
              format(sum(d$PERWT), big.mark = ",")))

  cat("[1] iran_data.Rda\n")
  export_iran_data(d)

  cat("\n[2] acs_stacked.Rda\n")
  export_acs_stacked(d)

  cat("\n[3] Education: gen_1_wide.Rda + gen_2_wide.Rda\n")
  export_education(d)

  cat("\n[4] Employment: class.Rda + emp.Rda\n")
  export_employment(d)

  cat("\n[5] by_citizen.xlsx\n")
  export_citizenship(d)

  cat("\n[6] by_yrimmig.xlsx\n")
  export_immigration(d)

  cat("\n[7] merged_data.Rda (state choropleth)\n")
  export_geography(d)

  cat("\n[8] Spouse data\n")
  export_spouse()

  cat("\n=== All exports complete ===\n")
}

cat("=== Census 2020-2024 Dashboard Export Script ===\n")
cat("Run: export_all()\n")

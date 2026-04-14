# Extract Iran-born rows from ONS "Create a custom dataset" CSVs and write
# tidy files for the UK page builder.
#
# Run from the deployment repo root:
#   Rscript R/uk_export/clean_ons_custom.R

suppressPackageStartupMessages(library(dplyr))

RAW <- file.path(dirname(dirname(dirname(getwd()))), "_data/uk/ons_custom")
OUT <- "data/uk"
if (!dir.exists(RAW)) RAW <- "../_data/uk/ons_custom"
cat(sprintf("Reading from: %s\n", RAW))

# Helper: find the label column (not the Code column) matching a pattern
find_col <- function(nms, pat) {
  hits <- grep(pat, nms, value = TRUE)
  hits <- hits[!grepl("Code$", hits)]
  stopifnot(length(hits) == 1)
  hits
}

filter_iran <- function(df, cob_col) {
  df[grepl("Iran", df[[cob_col]], fixed = TRUE), ]
}

# --- Age x Sex ---------------------------------------------------------------
cat("Cleaning age x sex...\n")
d <- read.csv(file.path(RAW, "iran_age_sex_ew_2021.csv"), stringsAsFactors = FALSE)
cob <- find_col(names(d), "Country.of.birth")
age <- find_col(names(d), "Age")
sex <- find_col(names(d), "Sex")

out <- filter_iran(d, cob) %>%
  transmute(age_band = gsub("Aged ", "", .data[[age]]),
            sex = .data[[sex]], count = as.integer(Observation)) %>%
  filter(count > 0)
write.csv(out, file.path(OUT, "uk_age_sex.csv"), row.names = FALSE)
cat(sprintf("  %d rows, total: %s\n", nrow(out), format(sum(out$count), big.mark = ",")))

# --- Economic activity -------------------------------------------------------
cat("Cleaning economic activity...\n")
d <- read.csv(file.path(RAW, "iran_economic_activity_ew_2021.csv"), stringsAsFactors = FALSE)
cob <- find_col(names(d), "Country.of.birth")
ea  <- find_col(names(d), "Economic")

out <- filter_iran(d, cob) %>%
  transmute(
    category = case_when(
      grepl("excluding.*In employment$", .data[[ea]]) ~ "Employed",
      grepl("excluding.*Unemployed", .data[[ea]]) ~ "Unemployed",
      grepl("and a full.time student.*In employment", .data[[ea]]) ~ "Student (employed)",
      grepl("and a full.time student.*Unemployed", .data[[ea]]) ~ "Student (unemployed)",
      grepl("inactive.*excluding", .data[[ea]]) ~ "Inactive",
      grepl("inactive and a full.time student", .data[[ea]]) ~ "Student (inactive)",
      TRUE ~ "Other"
    ),
    count = as.integer(Observation)
  ) %>% filter(count > 0, category != "Other")
write.csv(out, file.path(OUT, "uk_economic_activity.csv"), row.names = FALSE)
cat(sprintf("  %d rows, total: %s\n", nrow(out), format(sum(out$count), big.mark = ",")))

# --- Qualification -----------------------------------------------------------
cat("Cleaning qualification...\n")
d <- read.csv(file.path(RAW, "iran_qualification_ew_2021.csv"), stringsAsFactors = FALSE)
cob  <- find_col(names(d), "Country.of.birth")
qual <- find_col(names(d), "Highest")

out <- filter_iran(d, cob) %>%
  transmute(
    category = case_when(
      grepl("^Does not apply", .data[[qual]]) ~ NA_character_,
      grepl("^No qualifications", .data[[qual]]) ~ "No qualifications",
      grepl("^Level 1", .data[[qual]]) ~ "Level 1 (GCSEs)",
      grepl("^Level 2", .data[[qual]]) ~ "Level 2 (5+ GCSEs / O levels)",
      grepl("^Level 3", .data[[qual]]) ~ "Level 3 (A levels)",
      grepl("^Level 4", .data[[qual]]) ~ "Level 4+ (Degree or higher)",
      grepl("^Other", .data[[qual]]) ~ "Other qualifications",
      TRUE ~ .data[[qual]]
    ),
    count = as.integer(Observation)
  ) %>% filter(!is.na(category), count > 0)
write.csv(out, file.path(OUT, "uk_qualification.csv"), row.names = FALSE)
cat(sprintf("  %d rows, total: %s\n", nrow(out), format(sum(out$count), big.mark = ",")))

# --- Year of arrival ---------------------------------------------------------
cat("Cleaning year of arrival...\n")
d <- read.csv(file.path(RAW, "iran_year_of_arrival_ew_2021.csv"), stringsAsFactors = FALSE)
cob <- find_col(names(d), "Country.of.birth")
arr <- find_col(names(d), "Year.of.arrival")

out <- filter_iran(d, cob) %>%
  transmute(period = .data[[arr]], count = as.integer(Observation)) %>%
  filter(count > 0, !grepl("Does not apply|Born in the UK", period)) %>%
  mutate(
    mid_year = case_when(
      grepl("before 1951", period) ~ 1945L,
      grepl("1951 to 1960", period) ~ 1955L,
      grepl("1961 to 1970", period) ~ 1965L,
      grepl("1971 to 1980", period) ~ 1975L,
      grepl("1981 to 1990", period) ~ 1985L,
      grepl("1991 to 2000", period) ~ 1995L,
      grepl("2001 to 2010", period) ~ 2005L,
      grepl("2011 to 2013", period) ~ 2012L,
      grepl("2014 to 2016", period) ~ 2015L,
      grepl("2017 to 2019", period) ~ 2018L,
      grepl("2020 to 2021", period) ~ 2020L
    ),
    label = gsub("Arrived ", "", period)
  ) %>% arrange(mid_year) %>%
  mutate(cum_count = cumsum(count), cum_pct = round(cum_count / sum(count) * 100, 1))
write.csv(out, file.path(OUT, "uk_year_of_arrival.csv"), row.names = FALSE)
cat(sprintf("  %d rows, total: %s\n", nrow(out), format(sum(out$count), big.mark = ",")))

# --- Religion ----------------------------------------------------------------
cat("Cleaning religion...\n")
d <- read.csv(file.path(RAW, "iran_religion_ew_2021.csv"), stringsAsFactors = FALSE)
cob <- find_col(names(d), "Country.of.birth")
rel <- find_col(names(d), "Religion")

out <- filter_iran(d, cob) %>%
  transmute(religion = .data[[rel]], count = as.integer(Observation)) %>%
  filter(count > 0, !grepl("Does not apply", religion))
write.csv(out, file.path(OUT, "uk_religion.csv"), row.names = FALSE)
cat(sprintf("  %d rows, total: %s\n", nrow(out), format(sum(out$count), big.mark = ",")))

cat("\nAll UK custom dataset files cleaned.\n")

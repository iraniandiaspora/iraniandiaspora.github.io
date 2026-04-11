# Extract Iran rows from Mikrozensus 2024 Erstergebnisse .xlsx into tidy CSVs
# Run from the deployment repo root:
#   Rscript R/germany_export/extract_mikrozensus.R
#
# Input:  ../_data/germany/mikrozensus_2024/mikrozensus_2024_erstergebnisse.xlsx
#         (11 MB xlsx from Destatis, stored in the shared Dropbox _data dir)
# Output: data/germany/*.csv (one per dashboard chart, checked into this repo)
#
# Mikrozensus CSV-sheet schema:
#   Statistik_Code | Statistik_Label | VARIABLE_0 | VALUE_0 | VARIABLE_1 | VALUE_1 |
#   VARIABLE_2 | VALUE_2 | [VARIABLE_3 | VALUE_3] | value_col | Qualitaetskennzeichen
# Values are in thousands of persons. "/" = suppressed (<5k). "()" flag = low reliability.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})

f <- "../_data/germany/mikrozensus_2024/mikrozensus_2024_erstergebnisse.xlsx"
out_dir <- "data/germany"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Helper: read a csv-12211-NN sheet into a tidy tibble ---
# 12-col tables have VAR_3/VAL_3; 10-col tables do not.
read_mz_sheet <- function(sheet) {
  d <- suppressMessages(suppressWarnings(
    read_excel(f, sheet = sheet, col_names = FALSE, progress = FALSE)
  ))
  # Drop header row
  hdr <- as.character(unlist(d[1, ]))
  d <- d[-1, ]

  # Last two cols are always value + Qualitaetskennzeichen
  nc <- ncol(d)
  value_col <- hdr[nc - 1]
  # Normalize column names
  base_names <- c("code", "stat", "v0", "val0", "v1", "val1", "v2", "val2")
  if (nc == 12) {
    names(d) <- c(base_names, "v3", "val3", "value", "qk")
  } else if (nc == 10) {
    names(d) <- c(base_names, "value", "qk")
    d$v3 <- NA_character_
    d$val3 <- NA_character_
  } else stop("Unexpected col count ", nc, " in sheet ", sheet)

  # Drop "Ende der Tabelle." and any blank rows
  d <- d[!is.na(d$code), ]
  d <- d[d$code != "Ende der Tabelle.", ]

  # Parse value: "/" -> NA, "-" -> 0, strip parentheses markers, convert to integer (thousands)
  d$value_raw <- d$value
  d$value_k <- suppressWarnings(as.numeric(d$value))  # thousands
  d$suppressed <- d$value_raw == "/"
  d$low_rel <- !is.na(d$qk) & grepl("\\(\\)", d$qk)

  d$value_col <- value_col
  d$sheet <- sheet
  d
}

# Filter to Iran rows. Iran can appear in val2 (10-col tables) or val3 (12-col tables).
iran_only <- function(d) {
  d %>% filter(val2 == "Iran" | val3 == "Iran")
}

# Gen label helper: the migration-status split lives in val2 (12-col tables).
# 1st gen: "Bevölkerung mit eigener Migrationserfahrung"
# Total (1st+2nd): "Bevölkerung mit Migrationshintergrund"
gen_label <- function(val2) {
  case_when(
    val2 == "Bevölkerung mit eigener Migrationserfahrung" ~ "first_gen",
    val2 == "Bevölkerung mit Migrationshintergrund"       ~ "all_gens",
    TRUE ~ NA_character_
  )
}

cat("Reading Mikrozensus xlsx ...\n")

# ---------------------------------------------------------
# 1. HEADLINE: total + gen split + citizenship (from 12211-53)
# ---------------------------------------------------------
# Table 53 gives us total pop by (migration-status × Geburtsland).
# For Iran: Bevölkerung mit Migrationshintergrund zusammen = 319, Deutsche = 167,
# Ausländer = 152, mit eigener Migrationserfahrung = 250, ohne = 69.
t53 <- read_mz_sheet("csv-12211-53") %>% iran_only()
headline <- t53 %>%
  mutate(category = case_when(
    val1 == "Bevölkerung mit Migrationshintergrund zusammen" ~ "total",
    val1 == "Deutsche" ~ "german_citizens",
    val1 == "Ausländer/-innen" ~ "foreign_citizens",
    val1 == "mit eigener Migrationserfahrung" ~ "first_gen",
    val1 == "ohne Migrationserfahrung" ~ "second_gen",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(category)) %>%
  select(category, value_thousands = value_k)
write.csv(headline, file.path(out_dir, "de_headline.csv"), row.names = FALSE)
cat("  de_headline.csv (", nrow(headline), " rows)\n", sep = "")

# ---------------------------------------------------------
# 2. AGE GROUPS by gender (pyramid)
#    From 12211-06 (female) and 12211-07 (male).
# ---------------------------------------------------------
# "Davon im Alter von ... bis unter ... Jahren unter 5" etc. are the 5-year bins.
# We want both gen levels so the page can show the pyramid for all + 1st gen.
parse_pyramid <- function(sheet, sex) {
  d <- read_mz_sheet(sheet) %>% iran_only()
  d %>%
    mutate(
      gen = gen_label(val2),
      age_raw = val1,
      # Extract bin label: "Davon im Alter von ... bis unter ... Jahren unter 5" -> "under 5"
      age_bin = case_when(
        age_raw == "Insgesamt" ~ "all",
        grepl("^Nachrichtlich:", age_raw) ~ NA_character_,  # memo rows
        age_raw == "unter 15 Jahre" ~ NA_character_,
        age_raw == "18 Jahre und mehr" ~ NA_character_,
        age_raw == "15 bis unter 65 Jahre" ~ NA_character_,
        age_raw == "65 Jahre und mehr" ~ NA_character_,
        grepl("unter 5$", age_raw) ~ "0-4",
        grepl("^5 bis unter 10", age_raw) ~ "5-9",
        grepl("^10 bis unter 15", age_raw) ~ "10-14",
        grepl("^15 bis unter 20", age_raw) ~ "15-19",
        grepl("^20 bis unter 25", age_raw) ~ "20-24",
        grepl("^25 bis unter 30", age_raw) ~ "25-29",
        grepl("^30 bis unter 35", age_raw) ~ "30-34",
        grepl("^35 bis unter 40", age_raw) ~ "35-39",
        grepl("^40 bis unter 45", age_raw) ~ "40-44",
        grepl("^45 bis unter 50", age_raw) ~ "45-49",
        grepl("^50 bis unter 55", age_raw) ~ "50-54",
        grepl("^55 bis unter 60", age_raw) ~ "55-59",
        grepl("^60 bis unter 65", age_raw) ~ "60-64",
        grepl("^65 bis unter 70", age_raw) ~ "65-69",
        grepl("^70 bis unter 75", age_raw) ~ "70-74",
        grepl("^75 bis unter 80", age_raw) ~ "75-79",
        grepl("^80 bis unter 85", age_raw) ~ "80-84",
        grepl("^85 bis unter 90", age_raw) ~ "85-89",
        grepl("^90 bis unter 95", age_raw) ~ "90-94",
        grepl("95 und mehr", age_raw) ~ "95+",
        TRUE ~ NA_character_
      ),
      sex = sex
    ) %>%
    filter(!is.na(age_bin), !is.na(gen)) %>%
    select(sex, gen, age_bin, value_k, suppressed, low_rel)
}
pyr_f <- parse_pyramid("csv-12211-06", "female")
pyr_m <- parse_pyramid("csv-12211-07", "male")
pyramid <- bind_rows(pyr_f, pyr_m)
write.csv(pyramid, file.path(out_dir, "de_pyramid.csv"), row.names = FALSE)
cat("  de_pyramid.csv (", nrow(pyramid), " rows)\n", sep = "")

# ---------------------------------------------------------
# 3. BUNDESLAND map (16 states) from 12211-14
# ---------------------------------------------------------
# val1 = admin unit, but includes "Deutschland", "Früheres Bundesgebiet mit Berlin",
# "Neue Länder ohne Berlin" -- filter those out.
laender <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg",
  "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen",
  "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt",
  "Schleswig-Holstein", "Thüringen"
)
bund <- read_mz_sheet("csv-12211-14") %>% iran_only()
bund_out <- bund %>%
  mutate(gen = gen_label(val2)) %>%
  filter(val1 %in% laender, !is.na(gen)) %>%
  select(land = val1, gen, value_k, suppressed, low_rel)
write.csv(bund_out, file.path(out_dir, "de_bundesland.csv"), row.names = FALSE)
cat("  de_bundesland.csv (", nrow(bund_out), " rows)\n", sep = "")

# ---------------------------------------------------------
# 4. SCHOOL QUALIFICATION (Abitur etc.) from 12211-21
# ---------------------------------------------------------
school <- read_mz_sheet("csv-12211-21") %>% iran_only()
school_out <- school %>%
  mutate(gen = gen_label(val2)) %>%
  filter(!is.na(gen)) %>%
  select(school_level = val1, gen, value_k, suppressed, low_rel)
write.csv(school_out, file.path(out_dir, "de_school.csv"), row.names = FALSE)
cat("  de_school.csv (", nrow(school_out), " rows)\n", sep = "")

# ---------------------------------------------------------
# 5. PROFESSIONAL/ACADEMIC QUALIFICATION from 12211-24
# ---------------------------------------------------------
prof <- read_mz_sheet("csv-12211-24") %>% iran_only()
prof_out <- prof %>%
  mutate(gen = gen_label(val2)) %>%
  filter(!is.na(gen)) %>%
  select(prof_level = val1, gen, value_k, suppressed, low_rel)
write.csv(prof_out, file.path(out_dir, "de_profqual.csv"), row.names = FALSE)
cat("  de_profqual.csv (", nrow(prof_out), " rows)\n", sep = "")

# ---------------------------------------------------------
# 6. EMPLOYMENT / INDUSTRY / INCOME from 12211-38
# ---------------------------------------------------------
# val1 has all employment dimensions mashed into one column. Tag each row by
# logical section so the builder can pick out what it wants.
emp <- read_mz_sheet("csv-12211-38") %>% iran_only()
emp_out <- emp %>%
  mutate(
    gen = gen_label(val2),
    section = case_when(
      val1 %in% c("Bevölkerung insgesamt", "Erwerbspersonen zusammen",
                  "Erwerbstätige", "Erwerbslose", "Nichterwerbspersonen") ~ "status",
      val1 %in% c("Erwerbstätige nach Stellung im Beruf Selbstständige",
                  "mithelfende Familienangehörige", "Beamte/-innen",
                  "Angestellte, Arbeiter/-innen", "Auszubildende") ~ "stellung",
      val1 %in% c("Land- und Forstwirtschaft, Fischerei",
                  "Produzierendes Gewerbe, Baugewerbe",
                  "Handel, Gastgewerbe und Verkehr",
                  "Öffentliche Verwaltung",
                  "Sonstige Dienstleistungen") ~ "industry",
      val1 %in% c("Erwerbstätige mit Angabe zum Nettoeinkommmen von ... bis unter ... Euro unter 500",
                  "500 - 1 000", "1 000 - 1 500", "1 500 - 2 000", "2 000 - 2 500",
                  "2 500 - 3 000", "3 000 - 3 500", "3 500 und mehr", "Kein Einkommen") ~ "income",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(gen), !is.na(section)) %>%
  select(section, category = val1, gen, value_k, suppressed, low_rel)
write.csv(emp_out, file.path(out_dir, "de_employment.csv"), row.names = FALSE)
cat("  de_employment.csv (", nrow(emp_out), " rows)\n", sep = "")

# (Table 30 household income skipped: its Iran breakout does not split by generation
#  — the dashboard's income chart uses individual-level data from table 38 instead.)

# ---------------------------------------------------------
# 7. LANGUAGE AT HOME from 12211-47
# ---------------------------------------------------------
lang <- read_mz_sheet("csv-12211-47") %>% iran_only()
lang_out <- lang %>%
  mutate(gen = gen_label(val2)) %>%
  filter(!is.na(gen)) %>%
  select(language = val1, gen, value_k, suppressed, low_rel)
write.csv(lang_out, file.path(out_dir, "de_language.csv"), row.names = FALSE)
cat("  de_language.csv (", nrow(lang_out), " rows)\n", sep = "")

# ---------------------------------------------------------
# 8. IMMIGRATION MOTIVE from 12211-50 (1st-gen immigrants only)
# ---------------------------------------------------------
# 10-col table: val1 = motive, val2 = Iran. Only 1st gen since Zugewanderte.
motive <- read_mz_sheet("csv-12211-50") %>% iran_only()
motive_out <- motive %>%
  select(motive = val1, value_k, suppressed, low_rel)
write.csv(motive_out, file.path(out_dir, "de_motive.csv"), row.names = FALSE)
cat("  de_motive.csv (", nrow(motive_out), " rows)\n", sep = "")

# ---------------------------------------------------------
# 9. RESIDENCE DURATION + AGE AT ENTRY from 12211-08
# ---------------------------------------------------------
# 10-col table: val1 has both "Alter bei Einreise" bins (age at arrival) and
# "mit einer Aufenthaltsdauer..." bins (years of residence in Germany), val2 = Iran.
# Zugewanderte (first-gen immigrants) only. Used to build the duration chart
# on the immigration page, replacing the IW/BA third-party source.
t08 <- read_mz_sheet("csv-12211-08") %>% iran_only()
dur_bins_map <- c(
  "mit einer Aufenthaltsdauer von ... bis unter ... Jahren  unter 5" = "under_5",
  "5 bis unter 10" = "5_to_10",
  "10 bis unter 15" = "10_to_15",
  "15 bis unter 20" = "15_to_20",
  "20 bis unter 25" = "20_to_25",
  "25 bis unter 30" = "25_to_30",
  "30 bis unter 40" = "30_to_40",
  "40 und mehr" = "40_plus"
)
# The 5_to_10..40_plus labels are reused by age-at-entry rows (same text).
# Duration rows come AFTER the "mit einer Aufenthaltsdauer..." anchor row; age rows
# come after "Alter bei Einreise von ... Insgesamt". Walk rows in order to tag.
dur_rows <- t08 %>% arrange(as.integer(rownames(t08)))
section <- NA_character_
tagged <- character(nrow(dur_rows))
for (i in seq_len(nrow(dur_rows))) {
  v1 <- dur_rows$val1[i]
  if (grepl("^Alter bei Einreise", v1)) {
    section <- "age_at_entry"
  } else if (grepl("^mit einer Aufenthaltsdauer", v1)) {
    section <- "duration"
  }
  tagged[i] <- section
}
dur_rows$section <- tagged

duration_out <- dur_rows %>%
  filter(section == "duration") %>%
  mutate(bucket = case_when(
    grepl("^mit einer Aufenthaltsdauer.*unter 5$", val1) ~ "under_5",
    val1 == "5 bis unter 10" ~ "5_to_10",
    val1 == "10 bis unter 15" ~ "10_to_15",
    val1 == "15 bis unter 20" ~ "15_to_20",
    val1 == "20 bis unter 25" ~ "20_to_25",
    val1 == "25 bis unter 30" ~ "25_to_30",
    val1 == "30 bis unter 40" ~ "30_to_40",
    val1 == "40 und mehr" ~ "40_plus",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(bucket)) %>%
  select(bucket, value_k, suppressed, low_rel)
write.csv(duration_out, file.path(out_dir, "de_duration.csv"), row.names = FALSE)
cat("  de_duration.csv (", nrow(duration_out), " rows)\n", sep = "")

age_entry_out <- dur_rows %>%
  filter(section == "age_at_entry") %>%
  mutate(bucket = case_when(
    val1 == "unter 5" ~ "under_5",
    val1 == "5 bis unter 10" ~ "5_to_10",
    val1 == "10 bis unter 15" ~ "10_to_15",
    val1 == "15 bis unter 18" ~ "15_to_18",
    val1 == "18 bis unter 25" ~ "18_to_25",
    val1 == "25 bis unter 65" ~ "25_to_65",
    val1 == "65 und mehr" ~ "65_plus",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(bucket)) %>%
  select(bucket, value_k, suppressed, low_rel)
write.csv(age_entry_out, file.path(out_dir, "de_age_at_entry.csv"), row.names = FALSE)
cat("  de_age_at_entry.csv (", nrow(age_entry_out), " rows)\n", sep = "")

# ---------------------------------------------------------
# 10. NATURALIZATION FLOW from 12211-19
# ---------------------------------------------------------
# 10-col table: val1 = year of naturalization, val2 = prior citizenship = Iran.
nat <- read_mz_sheet("csv-12211-19") %>% iran_only()
nat_out <- nat %>%
  select(year_band = val1, value_k, suppressed, low_rel)
write.csv(nat_out, file.path(out_dir, "de_naturalization.csv"), row.names = FALSE)
cat("  de_naturalization.csv (", nrow(nat_out), " rows)\n", sep = "")

cat("\nDone. All outputs in ", out_dir, "\n", sep = "")

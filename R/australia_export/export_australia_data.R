# export_australia_data.R
# Reads ABS Census 2021 .rds files from the shared _data directory
# and writes CSVs to data/australia/ in the deployment repo.
#
# Run from the deployment repo root:
#   Rscript R/australia_export/export_australia_data.R
#
# Requires: _data/ path (Dropbox) — adjust DATA_SRC below.

library(dplyr)
library(readxl)

DATA_SRC <- "~/Library/CloudStorage/Dropbox/10_Coding/2025_06_IDD/_data/australia/abs_census_2021"
OUT_DIR  <- "data/australia"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== Exporting Australia data ===\n")

# --- 1. Age × Sex (national) ---
age_sex <- readRDS(file.path(DATA_SRC, "g09_iran_age_sex_national.rds")) %>%
  filter(agep != "Total") %>%
  select(sex = sexp, age_group = agep, count = obs_value)
write.csv(age_sex, file.path(OUT_DIR, "age_sex.csv"), row.names = FALSE)
cat("  age_sex.csv:", nrow(age_sex), "rows\n")

# --- 2. State population totals ---
state_pop <- readRDS(file.path(DATA_SRC, "g09_iran_age_sex_state.rds")) %>%
  filter(sexp == "Persons", agep == "Total") %>%
  select(state, count = obs_value) %>%
  arrange(desc(count))
write.csv(state_pop, file.path(OUT_DIR, "state_population.csv"), row.names = FALSE)
cat("  state_population.csv:", nrow(state_pop), "rows\n")

# --- 3. Year of arrival (national) ---
arrival <- readRDS(file.path(DATA_SRC, "g10_iran_arrival_national.rds")) %>%
  filter(yarp != "Total", yarp != "Not stated") %>%
  select(period = yarp, count = obs_value) %>%
  # Create sort order: decades first, then individual years
  mutate(sort_key = case_when(
    grepl("Before", period) ~ 1940,
    grepl("^\\d{4} - \\d{4}$", period) ~ as.numeric(sub(" -.*", "", period)),
    grepl("^Arrived (\\d{4})$", period) ~ as.numeric(sub("Arrived ", "", period)),
    TRUE ~ 9999
  )) %>%
  arrange(sort_key) %>%
  select(-sort_key)
write.csv(arrival, file.path(OUT_DIR, "year_of_arrival.csv"), row.names = FALSE)
cat("  year_of_arrival.csv:", nrow(arrival), "rows\n")

# --- 4. LGA-level population ---
lga <- readRDS(file.path(DATA_SRC, "g09_iran_lga_totals.rds")) %>%
  filter(obs_value > 0) %>%
  select(lga_name = region, state, count = obs_value) %>%
  arrange(desc(count))
write.csv(lga, file.path(OUT_DIR, "lga_population.csv"), row.names = FALSE)
cat("  lga_population.csv:", nrow(lga), "rows (non-zero LGAs)\n")

# --- 5. Citizenship (from cultural diversity Excel, Table 2) ---
xlsx <- file.path(DATA_SRC, "cultural_diversity_data_summary.xlsx")
t2 <- read_excel(xlsx, sheet = "Table 2", skip = 5)
iran_cit <- t2[grep("^Iran$", t2[[1]]), ]
citizenship <- data.frame(
  category = c("Australian citizen", "Not Australian citizen", "Not stated"),
  count = as.numeric(c(iran_cit[[2]], iran_cit[[3]], iran_cit[[4]])),
  stringsAsFactors = FALSE
)
write.csv(citizenship, file.path(OUT_DIR, "citizenship.csv"), row.names = FALSE)
cat("  citizenship.csv: 3 rows\n")

# --- 6. Iranian ancestry by state (Table 4) ---
# Column order: NSW, Vic, Qld, SA, WA, Tas, NT, ACT, then total
t4 <- read_excel(xlsx, sheet = "Table 4", skip = 5)
iran_anc <- t4[grep("^Iranian$", t4[[1]]), ]
state_names <- c("New South Wales", "Victoria", "Queensland", "South Australia",
                 "Western Australia", "Tasmania", "Northern Territory",
                 "Australian Capital Territory")
ancestry_state <- data.frame(
  state = state_names,
  ancestry_count = as.numeric(iran_anc[2:9]),
  stringsAsFactors = FALSE
)
write.csv(ancestry_state, file.path(OUT_DIR, "ancestry_by_state.csv"), row.names = FALSE)
cat("  ancestry_by_state.csv:", nrow(ancestry_state), "rows\n")

# --- 7. Persian language speakers by state (Table 5) ---
t5 <- read_excel(xlsx, sheet = "Table 5", skip = 5)
persian <- t5[grep("Persian \\(excluding Dari\\)", t5[[1]]), ]
persian_state <- data.frame(
  state = state_names,
  persian_speakers = as.numeric(persian[2:9]),
  stringsAsFactors = FALSE
)
write.csv(persian_state, file.path(OUT_DIR, "persian_speakers_by_state.csv"), row.names = FALSE)
cat("  persian_speakers_by_state.csv:", nrow(persian_state), "rows\n")

cat("=== Done ===\n")

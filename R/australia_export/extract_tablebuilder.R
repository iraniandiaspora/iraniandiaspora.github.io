# extract_tablebuilder.R
# Parse ABS Census 2021 TableBuilder cross-tab exports into tidy CSVs.
# Run from deployment repo root:
#   Rscript R/australia_export/extract_tablebuilder.R
#
# Input:  _data/australia/abs_census_2021/tablebuilder/*.csv (9 files)
# Output: data/australia/au_education.csv
#         data/australia/au_religion.csv
#         data/australia/au_income_weekly.csv
#         data/australia/au_labourforce.csv
#         data/australia/au_occupation.csv
#         data/australia/au_industry.csv
#         data/australia/au_second_gen.csv

TB_DIR <- "~/Library/CloudStorage/Dropbox/10_Coding/2025_06_IDD/_data/australia/abs_census_2021/tablebuilder"
OUT_DIR <- "data/australia"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== Extracting ABS TableBuilder cross-tabs ===\n")

# --- 1. Education (already clean) ---
edu <- read.csv(file.path(TB_DIR, "iran_education_heap.csv"),
                stringsAsFactors = FALSE)
edu <- edu[edu$education_level != "Total", ]
write.csv(edu, file.path(OUT_DIR, "au_education.csv"), row.names = FALSE)
cat("  au_education.csv:", nrow(edu), "rows\n")

# --- 2. Religion (already clean) ---
rel <- read.csv(file.path(TB_DIR, "iran_religion.csv"),
                stringsAsFactors = FALSE)
rel <- rel[rel$religion != "Total", ]
write.csv(rel, file.path(OUT_DIR, "au_religion.csv"), row.names = FALSE)
cat("  au_religion.csv:", nrow(rel), "rows\n")

# --- 3. Income weekly (already clean) ---
inc <- read.csv(file.path(TB_DIR, "iran_income_weekly.csv"),
                stringsAsFactors = FALSE)
inc <- inc[inc$income_band_weekly != "Total", ]
write.csv(inc, file.path(OUT_DIR, "au_income_weekly.csv"), row.names = FALSE)
cat("  au_income_weekly.csv:", nrow(inc), "rows\n")

# --- 4. Labour force status (parsed from cross-tab) ---
# The raw file has repeated section blocks. Extract Iran row totals per status.
lf_lines <- readLines(file.path(TB_DIR, "iran_income_x_labourforce_raw.csv"))

lf_rows <- list()
current_status <- NA
for (line in lf_lines) {
  trimmed <- trimws(line)
  # Section headers look like: " Employed, worked full-time"
  if (grepl("^\"? +(Employed|Unemployed|Not in the labour force|Not stated|Not applicable|Total)", trimmed)) {
    current_status <- gsub("^\"? +|\"$", "", trimmed)
  }
  # Iran data rows
  if (!is.na(current_status) && grepl("^\"Iran\"", trimmed)) {
    vals <- strsplit(gsub("\"", "", trimmed), ",")[[1]]
    # R strsplit drops trailing empty strings, so Total is the last element
    total_val <- as.integer(vals[length(vals)])
    if (!is.na(total_val) && current_status != "Total") {
      lf_rows[[length(lf_rows) + 1]] <- data.frame(
        status = current_status, count = total_val,
        stringsAsFactors = FALSE)
    }
  }
}
lf_df <- do.call(rbind, lf_rows)
write.csv(lf_df, file.path(OUT_DIR, "au_labourforce.csv"), row.names = FALSE)
cat("  au_labourforce.csv:", nrow(lf_df), "rows\n")

# --- 5. Occupation totals (from occupation × industry Total section) ---
occ_lines <- readLines(file.path(TB_DIR, "iran_occupation_x_industry_raw.csv"))

# Find the Total section and extract Iran row
in_total_section <- FALSE
occ_header <- NULL
occ_iran <- NULL
for (i in seq_along(occ_lines)) {
  trimmed <- trimws(occ_lines[i])
  if (trimmed == "\" Total\"") {
    in_total_section <- TRUE
    next
  }
  if (in_total_section && grepl("^\"1-digit level OCCP", trimmed)) {
    occ_header <- strsplit(gsub("\"", "", trimmed), ",")[[1]]
    occ_header <- trimws(occ_header[occ_header != ""])
    next
  }
  if (in_total_section && grepl("^\"Iran\"", trimmed)) {
    occ_iran <- strsplit(gsub("\"", "", trimmed), ",")[[1]]
    occ_iran <- trimws(occ_iran[occ_iran != ""])
    break
  }
}

# Build occupation data frame (skip the first element "Iran" and last "Total")
occ_names <- occ_header[2:(length(occ_header) - 1)]
occ_vals  <- as.integer(occ_iran[2:(length(occ_iran) - 1)])
occ_df <- data.frame(occupation = occ_names, count = occ_vals,
                     stringsAsFactors = FALSE)
write.csv(occ_df, file.path(OUT_DIR, "au_occupation.csv"), row.names = FALSE)
cat("  au_occupation.csv:", nrow(occ_df), "rows\n")

# --- 6. Industry totals (from each section's Iran row Total column) ---
ind_rows <- list()
current_industry <- NA
for (line in occ_lines) {
  trimmed <- trimws(line)
  # Industry section headers
  if (grepl("^\"? +[A-Z]", trimmed) && !grepl("^\"1-digit|^\"4-digit|^\"Iran|^\"Total|^\"Data|^\"INFO|^\"Copyright|^\"ABS|^\"Counting|^\"Filters|^\"Default", trimmed)) {
    current_industry <- gsub("^\"? +|\"$", "", trimmed)
  }
  if (!is.na(current_industry) && grepl("^\"Iran\"", trimmed)) {
    vals <- strsplit(gsub("\"", "", trimmed), ",")[[1]]
    total_val <- as.integer(vals[length(vals)])
    if (!is.na(total_val) && current_industry != "Total") {
      ind_rows[[length(ind_rows) + 1]] <- data.frame(
        industry = current_industry, count = total_val,
        stringsAsFactors = FALSE)
    }
    current_industry <- NA
  }
}
ind_df <- do.call(rbind, ind_rows)
write.csv(ind_df, file.path(OUT_DIR, "au_industry.csv"), row.names = FALSE)
cat("  au_industry.csv:", nrow(ind_df), "rows\n")

# --- 7. Second generation (Australian-born with Iran-born mother) ---
mother <- read.csv(file.path(TB_DIR, "iran_mother_birthplace_x_person_birthplace.csv"),
                   stringsAsFactors = FALSE)
au_born <- mother$count[mother$person_birthplace_region == "Oceania and Antarctica"]
second_gen <- data.frame(measure = "australian_born_iran_mother",
                         count = au_born, stringsAsFactors = FALSE)
write.csv(second_gen, file.path(OUT_DIR, "au_second_gen.csv"), row.names = FALSE)
cat("  au_second_gen.csv: second-gen count =", au_born, "\n")

cat("=== Done ===\n")

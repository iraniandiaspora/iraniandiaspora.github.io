# Extract Iran-origin population data from CBS StatLine OData JSONs.
#
# Input:  ../_data/netherlands/cbs_api/*.json (Dropbox raw data)
# Output: data/netherlands/*.csv + GeoJSON (deployment repo, in git)
#
# Run from deployment repo root:
#   Rscript R/nl_export/extract_cbs.R
#
# Source tables:
#   85384NED — Population by sex, age, marital status, herkomstland
#   85458NED — Population by sex, age, herkomstland, region
#   84729NED — Labour force by sex, characteristics, herkomstland

suppressPackageStartupMessages(library(jsonlite))

CBS_DIR <- "../_data/netherlands/cbs_api"
OUT_DIR <- "data/netherlands"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Extracting Netherlands CBS data...\n")

# Helper: trim CBS whitespace-padded numeric strings and convert to integer
cbs_int <- function(x) {
  x <- trimws(x)
  x[x == "" | x == "."] <- NA
  as.integer(x)
}

# ---- 1. Headline (85384NED, 2025) ----
raw_hl <- fromJSON(file.path(CBS_DIR, "iran_85384_2025.json"))$value
# Geboorteland: T001638=Total, A051735=NL-born (2nd gen), A051736=Foreign-born (1st gen)
# GeboortelandOuders: T001638=Total
total_v <- cbs_int(raw_hl$Bevolking_1[raw_hl$Geboorteland == "T001638" &
                                       raw_hl$GeboortelandOuders == "T001638"])
gen1_v  <- cbs_int(raw_hl$Bevolking_1[raw_hl$Geboorteland == "A051736" &
                                       raw_hl$GeboortelandOuders == "T001638"])
gen2_v  <- cbs_int(raw_hl$Bevolking_1[raw_hl$Geboorteland == "A051735" &
                                       raw_hl$GeboortelandOuders == "T001638"])

headline <- data.frame(
  category = c("total", "gen1", "gen2"),
  label = c("Total Iranian-origin",
            "1st generation (born in Iran)",
            "2nd generation (born in Netherlands)"),
  count = c(total_v, gen1_v, gen2_v),
  stringsAsFactors = FALSE
)
write.csv(headline, file.path(OUT_DIR, "nl_headline.csv"), row.names = FALSE)
cat("  nl_headline.csv\n")

# ---- 2. Population trend 2022-2025 (85384NED) ----
raw_ts <- fromJSON(file.path(CBS_DIR, "iran_ts_gens.json"))$value
ts_total <- raw_ts[raw_ts$Geboorteland == "T001638", ]
ts_gen1  <- raw_ts[raw_ts$Geboorteland == "A051736", ]
ts_gen2  <- raw_ts[raw_ts$Geboorteland == "A051735", ]

trend <- data.frame(
  year = as.integer(substr(ts_total$Perioden, 1, 4)),
  total = cbs_int(ts_total$Bevolking_1),
  gen1 = cbs_int(ts_gen1$Bevolking_1),
  gen2 = cbs_int(ts_gen2$Bevolking_1),
  stringsAsFactors = FALSE
)
write.csv(trend, file.path(OUT_DIR, "nl_trend.csv"), row.names = FALSE)
cat("  nl_trend.csv\n")

# ---- 3. Province-level counts (85458NED, 2025) ----
raw_reg <- fromJSON(file.path(CBS_DIR, "iran_by_region_2025.json"))$value
raw_reg$RegioS   <- trimws(raw_reg$RegioS)
raw_reg$Geslacht <- trimws(raw_reg$Geslacht)

# Total sex, total age, province level (PV prefix)
prov_rows <- raw_reg[raw_reg$Geslacht == "T001038" &
                     raw_reg$Leeftijd == "10000" &
                     grepl("^PV", raw_reg$RegioS), ]

# Province name lookup from regios metadata
regios <- fromJSON(file.path(CBS_DIR, "regios_85458.json"))$value
regios$Key <- trimws(regios$Key)
prov_names <- setNames(
  sub(" \\(PV\\)$", "", regios$Title[grepl("^PV", regios$Key)]),
  regios$Key[grepl("^PV", regios$Key)]
)

province <- data.frame(
  province_code = prov_rows$RegioS,
  province_name = as.character(prov_names[prov_rows$RegioS]),
  count = cbs_int(prov_rows$Bevolking_1),
  stringsAsFactors = FALSE
)
province <- province[order(-province$count), ]
write.csv(province, file.path(OUT_DIR, "nl_province.csv"), row.names = FALSE)
cat("  nl_province.csv\n")

# ---- 4. Age-sex pyramid (85458NED, 2025, national) ----
raw_age <- fromJSON(file.path(CBS_DIR, "iran_by_age_sex_2025.json"))$value
raw_age$RegioS   <- trimws(raw_age$RegioS)
raw_age$Geslacht <- trimws(raw_age$Geslacht)
raw_age$Leeftijd <- trimws(raw_age$Leeftijd)

# National only, male/female only, exclude total age and 100+
nat_age <- raw_age[raw_age$RegioS == "NL01" &
                   raw_age$Geslacht %in% c("3000", "4000") &
                   !raw_age$Leeftijd %in% c("10000", "22200"), ]

age_lookup <- c(
  "70100" = "0-4",   "70200" = "5-9",   "70300" = "10-14", "70400" = "15-19",
  "70500" = "20-24", "70600" = "25-29", "70700" = "30-34", "70800" = "35-39",
  "70900" = "40-44", "71000" = "45-49", "71100" = "50-54", "71200" = "55-59",
  "71300" = "60-64", "71400" = "65-69", "71500" = "70-74", "71600" = "75-79",
  "71700" = "80-84", "71800" = "85-89", "71900" = "90-94", "72000" = "95-99")
sex_lookup <- c("3000" = "Male", "4000" = "Female")

pyramid <- data.frame(
  age_group = age_lookup[nat_age$Leeftijd],
  sex = sex_lookup[nat_age$Geslacht],
  count = cbs_int(nat_age$Bevolking_1),
  age_lower = as.integer(sub("-.*", "", age_lookup[nat_age$Leeftijd])),
  stringsAsFactors = FALSE
)
pyramid <- pyramid[order(pyramid$age_lower, pyramid$sex), ]
pyramid$age_lower <- NULL
write.csv(pyramid, file.path(OUT_DIR, "nl_pyramid.csv"), row.names = FALSE)
cat("  nl_pyramid.csv\n")

# ---- 5. Labour force (84729NED, 2021-2024) ----
raw_lf <- fromJSON(file.path(CBS_DIR, "iran_labourforce.json"))$value

labourforce <- data.frame(
  year = as.integer(substr(raw_lf$Perioden, 1, 4)),
  total_15_74 = cbs_int(raw_lf$BeroepsEnNietBeroepsbevolking_1),
  employed = cbs_int(raw_lf$WerkzameBeroepsbevolking_2),
  participation_rate = cbs_int(raw_lf$NettoArbeidsparticipatie_3),
  employee_pct = cbs_int(raw_lf$Werknemer_4),
  permanent_pct = cbs_int(raw_lf$WerknemerMetVasteArbeidsrelatie_5),
  flexible_pct = cbs_int(raw_lf$WerknemerMetFlexibeleArbeidsrelatie_6),
  selfemployed_pct = cbs_int(raw_lf$Zelfstandige_7),
  stringsAsFactors = FALSE
)
write.csv(labourforce, file.path(OUT_DIR, "nl_labourforce.csv"), row.names = FALSE)
cat("  nl_labourforce.csv\n")

# ---- 6. Province GeoJSON (cartomap, downloaded once) ----
geojson_path <- file.path(OUT_DIR, "nl_provinces.geojson")
if (!file.exists(geojson_path)) {
  geojson_url <- "https://cartomap.github.io/nl/wgs84/provincie_2024.geojson"
  cat("  Downloading province GeoJSON from cartomap...")
  tryCatch({
    download.file(geojson_url, geojson_path, quiet = TRUE, method = "auto")
    cat(" done\n")
  }, error = function(e) {
    cat(" FAILED (", conditionMessage(e), ")\n")
    cat("  Province map will use bar chart fallback in the builder.\n")
  })
} else {
  cat("  nl_provinces.geojson (already exists)\n")
}

# ---- 7. Residence duration → implied arrival year (85371NED, 2025) ----
raw_rd <- fromJSON(file.path(CBS_DIR, "iran_residence_duration.json"))
rd_rows <- raw_rd$value

# Duration code → years mapping
dur_codes <- fromJSON(file.path(CBS_DIR, "verblijfsduur_codes.json"))$value
dur_map <- setNames(
  sapply(dur_codes$Title, function(t) {
    if (grepl("Minder dan 1", t)) return(0L)
    if (grepl("jaar of meer", t)) return(as.integer(sub(" .*", "", t)))
    if (grepl("Onbekend", t)) return(-1L)
    if (grepl("Totaal", t)) return(-99L)
    as.integer(sub(" .*", "", t))
  }),
  trimws(dur_codes$Key)
)

# Filter: 2025, total sex/age, exclude total and unknown
rd_2025 <- rd_rows[grepl("2025", rd_rows$Perioden), ]
rd_2025$dur_code <- trimws(rd_2025$VerblijfsduurInNederland)
rd_2025 <- rd_2025[!rd_2025$dur_code %in% c("T001129", "A028024"), ]
rd_2025$dur_years <- dur_map[rd_2025$dur_code]
rd_2025$arrival_year <- 2025L - rd_2025$dur_years
rd_2025$count <- as.integer(rd_2025$BevolkingGeborenBuitenNederland_1)

# The 60+ bucket maps to <=1965; label as 1965
arrival <- data.frame(
  arrival_year = rd_2025$arrival_year,
  count = rd_2025$count,
  stringsAsFactors = FALSE
)
arrival <- arrival[order(arrival$arrival_year), ]
write.csv(arrival, file.path(OUT_DIR, "nl_arrival_year.csv"), row.names = FALSE)
cat("  nl_arrival_year.csv\n")

# ---- 8. Income comparison (85397NED, 2011-2024) ----
raw_inc <- fromJSON(file.path(CBS_DIR, "iran_wealth_income.json"))
inc_rows <- raw_inc$value
# Filter to total parental origin (T001638)
iran_inc <- inc_rows[inc_rows$GeboortelandOudersHoofdkostwinner == "T001638", ]

raw_nl_inc <- fromJSON(file.path(CBS_DIR, "nl_total_wealth_income.json"))
nl_inc <- raw_nl_inc$value

cbs_num <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "." | x == "None"] <- NA
  as.numeric(x)
}

income <- data.frame(
  year = as.integer(substr(iran_inc$Perioden, 1, 4)),
  iran_households_k = cbs_num(iran_inc$ParticuliereHuishoudens_1),
  iran_avg_income_k = cbs_num(iran_inc$GemiddeldBesteedbaarInkomen_2),
  iran_std_income_k = cbs_num(iran_inc$GemiddeldGestandaardiseerdInkomen_3),
  iran_med_wealth_k = cbs_num(iran_inc$MediaanVermogen_4),
  iran_low_income_pct = cbs_num(iran_inc$HuishoudensMetLaagInkomen_5),
  stringsAsFactors = FALSE
)
# Merge NL totals
nl_df <- data.frame(
  year = as.integer(substr(nl_inc$Perioden, 1, 4)),
  nl_avg_income_k = cbs_num(nl_inc$GemiddeldBesteedbaarInkomen_2),
  nl_std_income_k = cbs_num(nl_inc$GemiddeldGestandaardiseerdInkomen_3),
  nl_med_wealth_k = cbs_num(nl_inc$MediaanVermogen_4),
  nl_low_income_pct = cbs_num(nl_inc$HuishoudensMetLaagInkomen_5),
  stringsAsFactors = FALSE
)
income <- merge(income, nl_df, by = "year", all.x = TRUE)
income <- income[order(income$year), ]
write.csv(income, file.path(OUT_DIR, "nl_income.csv"), row.names = FALSE)
cat("  nl_income.csv\n")

cat("Done.\n")

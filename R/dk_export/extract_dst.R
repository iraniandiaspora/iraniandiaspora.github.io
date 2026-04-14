# Extract Iran-origin population data from DST (Statistics Denmark) API CSVs.
#
# Input:  ../_data/denmark/dst_api/*.csv (Dropbox raw data, semicolon-delimited)
# Output: data/denmark/*.csv (deployment repo, in git)
#
# Run from deployment repo root:
#   Rscript R/dk_export/extract_dst.R

DST_DIR <- "../_data/denmark/dst_api"
OUT_DIR <- "data/denmark"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Extracting Denmark DST data...\n")

# ---- 1. Headline (from ancestry time series, latest year) ----
ts <- read.csv(file.path(DST_DIR, "iran_ancestry_sex_1980_2026.csv"),
               sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
# Columns: IELAND;HERKOMST;TID;INDHOLD
# HERKOMST: "3 Efterkommere" = descendants (2nd gen), "4 Indvandrere" = immigrants (1st gen)
ts$year <- as.integer(sub(" .*", "", ts$TID))
ts$count <- as.integer(ts$INDHOLD)
ts$gen <- ifelse(grepl("Efterkommere", ts$HERKOMST), "gen2", "gen1")

latest_yr <- max(ts$year)
gen1 <- sum(ts$count[ts$year == latest_yr & ts$gen == "gen1"])
gen2 <- sum(ts$count[ts$year == latest_yr & ts$gen == "gen2"])
total <- gen1 + gen2

headline <- data.frame(
  category = c("total", "gen1", "gen2"),
  label = c("Total Iranian-origin",
            "Immigrants (born in Iran)",
            "Descendants (born in Denmark)"),
  count = c(total, gen1, gen2),
  year = latest_yr,
  stringsAsFactors = FALSE
)
write.csv(headline, file.path(OUT_DIR, "dk_headline.csv"), row.names = FALSE)
cat("  dk_headline.csv\n")

# ---- 2. Historical trend 1980-2026 (total by gen) ----
trend <- aggregate(count ~ year + gen, data = ts, FUN = sum)
trend_wide <- reshape(trend, idvar = "year", timevar = "gen",
                      direction = "wide")
names(trend_wide) <- sub("count\\.", "", names(trend_wide))
trend_wide$total <- trend_wide$gen1 + trend_wide$gen2
trend_wide <- trend_wide[order(trend_wide$year), ]
row.names(trend_wide) <- NULL
write.csv(trend_wide, file.path(OUT_DIR, "dk_trend.csv"), row.names = FALSE)
cat("  dk_trend.csv\n")

# ---- 3. Population pyramid (2026, single-year age x sex x gen) ----
pyr_raw <- read.csv(file.path(DST_DIR, "iran_age_sex_2026.csv"),
                    sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
# ALDER;KØN;HERKOMST;IELAND;TID;INDHOLD
pyr_raw$age_num <- as.integer(sub(" .*", "", pyr_raw$ALDER))
pyr_raw$count <- as.integer(pyr_raw$INDHOLD)
pyr_raw$sex <- ifelse(grepl("^M ", pyr_raw[["KØN"]]), "Male", "Female")

# Aggregate across HERKOMST (immigrants + descendants combined)
pyr_all <- aggregate(count ~ age_num + sex, data = pyr_raw, FUN = sum)
pyr_all <- pyr_all[!is.na(pyr_all$age_num), ]

# 5-year age groups
breaks <- c(seq(0, 95, 5), Inf)
labels <- c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"), "95+")
pyr_all$age_group <- cut(pyr_all$age_num, breaks = breaks, right = FALSE, labels = labels)

pyramid <- aggregate(count ~ age_group + sex, data = pyr_all, FUN = sum)
pyramid$age_lower <- as.integer(sub("-.*|\\+", "", pyramid$age_group))
pyramid <- pyramid[order(pyramid$age_lower, pyramid$sex), ]
pyramid$age_lower <- NULL
row.names(pyramid) <- NULL
write.csv(pyramid, file.path(OUT_DIR, "dk_pyramid.csv"), row.names = FALSE)
cat("  dk_pyramid.csv\n")

# ---- 4. Region-level counts (5 regions, 2026) ----
reg_raw <- read.csv(file.path(DST_DIR, "iran_by_region_2026_folk1c.csv"),
                    sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
# OMRÅDE;HERKOMST;IELAND;KØN;ALDER;TID;INDHOLD
# Region rows have 3-digit codes 08x (Hovedstaden=084, Midtjylland=082, etc.)
# National total is "000 Hele landet"
reg_raw$count <- as.integer(reg_raw$INDHOLD)
reg_raw$code <- sub(" .*", "", reg_raw[["OMRÅDE"]])
reg_raw$name <- sub("^[0-9]+ ", "", reg_raw[["OMRÅDE"]])

# Filter to region-level (codes 081-085) and sum across HERKOMST
region_codes <- c("081", "082", "083", "084", "085")
regions <- reg_raw[reg_raw$code %in% region_codes, ]
region_agg <- aggregate(count ~ code + name, data = regions, FUN = sum)
region_agg <- region_agg[order(-region_agg$count), ]
row.names(region_agg) <- NULL
names(region_agg) <- c("region_code", "region_name", "count")
write.csv(region_agg, file.path(OUT_DIR, "dk_region.csv"), row.names = FALSE)
cat(sprintf("  dk_region.csv (%d regions)\n", nrow(region_agg)))

# ---- 5. Employment by industry (Q4 2024) ----
ind_raw <- read.csv(file.path(DST_DIR, "iran_industry_2024q4.csv"),
                    sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
# TAL;BRANCHEDB0738;OPRINDLAND;KØN;TID;INDHOLD
ind_raw$count <- as.integer(ind_raw$INDHOLD)
ind_raw$sector_code <- sub(" .*", "", ind_raw$BRANCHEDB0738)
ind_raw$sector_name <- sub("^[A-Z] ", "", ind_raw$BRANCHEDB0738)

# Skip total row (TOT) and unknown (X)
sectors <- ind_raw[!ind_raw$sector_code %in% c("TOT", "X") & ind_raw$count > 0, ]
sectors <- sectors[order(-sectors$count), c("sector_code", "sector_name", "count")]
row.names(sectors) <- NULL

# Add total for percentage computation
sectors$total_employed <- sum(sectors$count)
sectors$pct <- round(sectors$count / sectors$total_employed * 100, 1)

write.csv(sectors, file.path(OUT_DIR, "dk_industry.csv"), row.names = FALSE)
cat(sprintf("  dk_industry.csv (%d sectors)\n", nrow(sectors)))

# ---- 6. Sex-split time series for trend detail ----
herkomst <- read.csv(file.path(DST_DIR, "iran_herkomst_ts.csv"),
                     sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
# IELAND;HERKOMST;KØN;TID;INDHOLD
herkomst$year <- as.integer(sub(" .*", "", herkomst$TID))
herkomst$count <- as.integer(herkomst$INDHOLD)
herkomst$sex <- ifelse(grepl("^M ", herkomst[["KØN"]]), "Male", "Female")
herkomst$gen <- ifelse(grepl("Efterkommere", herkomst$HERKOMST), "gen2", "gen1")

# Wide format: year, gen1_male, gen1_female, gen2_male, gen2_female
sex_ts <- aggregate(count ~ year + gen + sex, data = herkomst, FUN = sum)
sex_ts$key <- paste0(sex_ts$gen, "_", tolower(sex_ts$sex))
sex_wide <- reshape(sex_ts[, c("year", "key", "count")],
                    idvar = "year", timevar = "key", direction = "wide")
names(sex_wide) <- sub("count\\.", "", names(sex_wide))
sex_wide <- sex_wide[order(sex_wide$year), ]
row.names(sex_wide) <- NULL
write.csv(sex_wide, file.path(OUT_DIR, "dk_sex_trend.csv"), row.names = FALSE)
cat("  dk_sex_trend.csv\n")

cat(sprintf("\nDenmark: %s Iranian-origin (%s immigrants + %s descendants, %d)\n",
  format(total, big.mark = ","), format(gen1, big.mark = ","),
  format(gen2, big.mark = ","), latest_yr))
cat("Done.\n")

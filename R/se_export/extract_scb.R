# Extract Iran-origin population data from SCB (Statistics Sweden) API CSVs.
#
# Input:  ../_data/sweden/scb_api/*.csv (Dropbox raw data)
# Output: data/sweden/*.csv (deployment repo, in git)
#
# Run from deployment repo root:
#   Rscript R/se_export/extract_scb.R

SCB_DIR <- "../_data/sweden/scb_api"
OUT_DIR <- "data/sweden"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Extracting Sweden SCB data...\n")

# ---- 1. Headline + generational trend (from ursprung 2018-2025) ----
ursprung <- read.csv(file.path(SCB_DIR, "iran_ursprung_2018_2025.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE,
                     fileEncoding = "latin1")

# Identify year columns
year_cols <- grep("^Number of persons", names(ursprung), value = TRUE)
years <- as.integer(sub("Number of persons ", "", year_cols))

# gen1 = foreign-born; gen2 = all native-born categories
gen1_row <- which(ursprung$origin == "foreign-born")
gen2_rows <- which(ursprung$origin != "foreign-born")

# Latest year for headline
latest_col <- year_cols[which.max(years)]
latest_yr <- max(years)
gen1 <- as.integer(ursprung[[latest_col]][gen1_row])
gen2 <- sum(as.integer(ursprung[[latest_col]][gen2_rows]))
total <- gen1 + gen2

headline <- data.frame(
  category = c("total", "gen1", "gen2"),
  label = c("Total Iranian-origin",
            "1st generation (born in Iran)",
            "2nd generation (born in Sweden)"),
  count = c(total, gen1, gen2),
  year = latest_yr,
  stringsAsFactors = FALSE
)
write.csv(headline, file.path(OUT_DIR, "se_headline.csv"), row.names = FALSE)
cat("  se_headline.csv\n")

# Build trend from ursprung (2018-2025 generational split)
trend_gen1 <- as.integer(ursprung[gen1_row, year_cols])
trend_gen2 <- sapply(year_cols, function(yc) sum(as.integer(ursprung[gen2_rows, yc])))
trend <- data.frame(year = years, gen1 = trend_gen1, gen2 = as.integer(trend_gen2),
                    stringsAsFactors = FALSE)
trend$total <- trend$gen1 + trend$gen2
write.csv(trend, file.path(OUT_DIR, "se_trend.csv"), row.names = FALSE)
cat("  se_trend.csv\n")

# ---- 2. Population pyramid (2024, aggregate single-year to 5-year groups) ----
agesex <- read.csv(file.path(SCB_DIR, "iran_agesex_2000_2024.csv"),
                   stringsAsFactors = FALSE, check.names = FALSE,
                   fileEncoding = "UTF-8")

agesex$count <- as.integer(agesex[["Number of persons 2024"]])
agesex$age_num <- suppressWarnings(as.integer(sub(" .*", "", agesex$age)))
# Drop rows where age_num is NA (header/total rows)
agesex <- agesex[!is.na(agesex$age_num) & !is.na(agesex$count), ]
agesex$sex_clean <- ifelse(agesex$sex == "men", "Male", "Female")

# 5-year age groups: 0-4, 5-9, ..., 90-94, 95+
breaks <- c(seq(0, 95, 5), Inf)
labels <- c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"), "95+")
agesex$age_group <- cut(agesex$age_num, breaks = breaks, right = FALSE, labels = labels)

pyramid <- aggregate(count ~ age_group + sex_clean, data = agesex, FUN = sum)
names(pyramid) <- c("age_group", "sex", "count")
pyramid$age_lower <- as.integer(sub("-.*|\\+", "", pyramid$age_group))
pyramid <- pyramid[order(pyramid$age_lower, pyramid$sex), ]
pyramid$age_lower <- NULL
write.csv(pyramid, file.path(OUT_DIR, "se_pyramid.csv"), row.names = FALSE)
cat("  se_pyramid.csv\n")

# ---- 3. County-level counts (2024, Iran-born only) ----
.reg_lines <- readLines(file.path(SCB_DIR, "iran_by_region_2024.csv"),
                        encoding = "latin1", warn = FALSE)
regions <- read.csv(textConnection(.reg_lines),
                    stringsAsFactors = FALSE, check.names = FALSE)
names(regions) <- c("region", "country", "sex", "count")
regions$count <- as.integer(regions$count)

# County rows have 2-digit code prefix (01-25); municipalities have 4-digit
county_mask <- grepl("^[0-9]{2} ", regions$region) &
               !grepl("^[0-9]{4}", regions$region) &
               regions$sex == "total"
counties <- regions[county_mask, ]
counties$county_code <- sub(" .*", "", counties$region)
counties$county_name <- sub("^[0-9]+ ", "", counties$region)
counties <- counties[order(-counties$count), c("county_code", "county_name", "count")]
row.names(counties) <- NULL
write.csv(counties, file.path(OUT_DIR, "se_county.csv"), row.names = FALSE)
cat(sprintf("  se_county.csv (%d counties)\n", nrow(counties)))

# ---- 4. Years since immigration (2024, aggregate across age/sex) ----
yrssince <- read.csv(file.path(SCB_DIR, "iran_yrssince_agesex_2024.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE,
                     fileEncoding = "latin1")
names(yrssince) <- c("country", "duration", "age", "sex", "count")
yrssince$count <- as.integer(yrssince$count)

dur_agg <- aggregate(count ~ duration, data = yrssince, FUN = sum)
dur_agg <- dur_agg[!dur_agg$duration %in% c("unknown"), ]

# Assign sort order and implied midpoint arrival year
dur_order <- c("0 years", "1 year", "2 years", "3 years", "4 years",
               "5 years", "6 years", "7 years", "8 years", "9 years",
               "10-14 years", "15-19 years", "20-24 years", "25-29 years",
               "30-34 years", "35-39 years", "40-44 years", "45-49 years",
               "50 + years")
dur_agg <- dur_agg[match(dur_order, dur_agg$duration), ]
dur_agg <- dur_agg[!is.na(dur_agg$duration), ]
row.names(dur_agg) <- NULL

# Midpoint arrival year for chart x-axis
dur_agg$arrival_mid <- NA_integer_
for (i in seq_len(nrow(dur_agg))) {
  d <- dur_agg$duration[i]
  if (grepl("^[0-9]+ year", d)) {
    dur_agg$arrival_mid[i] <- 2024L - as.integer(sub(" .*", "", d))
  } else if (grepl("^[0-9]+-[0-9]+", d)) {
    bounds <- as.integer(strsplit(sub(" years", "", d), "-")[[1]])
    dur_agg$arrival_mid[i] <- 2024L - as.integer(mean(bounds))
  } else if (d == "50 + years") {
    dur_agg$arrival_mid[i] <- 1971L
  }
}

write.csv(dur_agg, file.path(OUT_DIR, "se_yrssince.csv"), row.names = FALSE)
cat("  se_yrssince.csv\n")

# ---- 5. Historical trend from agesex totals (2000-2024 Iran-born only) ----
# Complement the 2018-2025 ursprung trend with longer Iran-born series
hist_year_cols <- grep("^Number of persons", names(agesex), value = TRUE)
# Already aggregated agesex to pyramid, so re-read for yearly totals
agesex_full <- read.csv(file.path(SCB_DIR, "iran_agesex_2000_2024.csv"),
                        stringsAsFactors = FALSE, check.names = FALSE,
                        fileEncoding = "latin1")
hist_years <- as.integer(sub("Number of persons ", "",
                             grep("^Number of persons", names(agesex_full), value = TRUE)))
hist_cols <- grep("^Number of persons", names(agesex_full), value = TRUE)

hist_totals <- sapply(hist_cols, function(col) sum(as.integer(agesex_full[[col]]), na.rm = TRUE))
hist_trend <- data.frame(year = hist_years, iran_born = as.integer(hist_totals),
                         stringsAsFactors = FALSE)
hist_trend <- hist_trend[order(hist_trend$year), ]
row.names(hist_trend) <- NULL
write.csv(hist_trend, file.path(OUT_DIR, "se_hist_iran_born.csv"), row.names = FALSE)
cat("  se_hist_iran_born.csv\n")

cat(sprintf("\nSweden: %s Iranian-origin (%s 1st gen + %s 2nd gen, %d)\n",
  format(total, big.mark = ","), format(gen1, big.mark = ","),
  format(gen2, big.mark = ","), latest_yr))
cat("Done.\n")

# export_un_migrant_stock.R
# Rebuilds data/global/stocks_countries.csv (the file R/build_global.R reads)
# from the UN International Migrant Stock 2024 workbook, Table 1.
#
# Input (data store, original as released by UN DESA, CC BY 3.0 IGO):
#   _data/United_Nations_Department_of_Economic_and_Social_Affairs_UN_DESA/
#     2024_UNDESA_International_Migrant_Stock_by_Sex_Destination_Origin.xlsx
#   Source file: undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx,
#   https://www.un.org/development/desa/pd/content/international-migrant-stock
#   (6,005,287 bytes, SHA-256 0e10179d...7316, last modified 2025-01-27 per the UN server).
#   Override the path with env var UN_XLSX (e.g. to test from a scratch copy).
# Output: data/global/stocks_countries.csv
#
# Run from the iraniandiaspora.github.io/ directory:
#   Rscript R/global_export/export_un_migrant_stock.R
#
# Selection rule (verified 2026-11-23: reproduces the committed 70-row file
# exactly, apart from the two documented overrides below): origin = Iran
# (location code 364); country-level destinations only (code < 900; the UN
# aggregate rows are 900 and up); destinations with a non-missing, positive 2024
# stock. Columns: destination, destination_code, origin, origin_code, X1990 ...
# X2024 (both sexes).
#
# Two rows are NOT the UN value, on purpose (see R/build_global.R header and
# .claude/rules/israel-pipeline.md):
#   - Israel X2024 = 38,200: CBS register count (76th Statistical Abstract, avg
#     2024) in place of the UN 2024 estimate (61,572); the Israel population-over-
#     time chart restores the UN figure separately (UN_ISRAEL_2024 in build_israel.R).
#   - France: the UN workbook has no Iran-origin row for France; the row is
#     Eurostat/INSEE-sourced (2000, 2005, 2015 and 2020/2024 carried from 2018).

library(readxl)
library(readr)

DATA_DIR <- file.path(here::here(), "data/global")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

UN_XLSX <- Sys.getenv("UN_XLSX", unset = file.path(
  here::here(), "..", "_data",
  "United_Nations_Department_of_Economic_and_Social_Affairs_UN_DESA",
  "2024_UNDESA_International_Migrant_Stock_by_Sex_Destination_Origin.xlsx"))
if (!file.exists(UN_XLSX)) {
  stop(sprintf("UN migrant stock workbook not found at %s\nStage the original from the UN page named in this script's header, or set UN_XLSX.",
               UN_XLSX))
}

YEARS <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024)

# Table 1 sits under a banner; find the header row (first column "Index") rather
# than counting rows. Columns: 1 index, 2 destination, 3 notes, 4 data type,
# 5 destination code, 6 origin, 7 origin code, 8-15 stock at mid-year for
# 1990 ... 2024, both sexes (checked against the header row).
raw <- suppressMessages(read_excel(UN_XLSX, sheet = "Table 1", col_names = FALSE))
h <- which(as.character(raw[[1]]) == "Index")[1]
stopifnot(!is.na(h))
hdr <- as.character(unlist(raw[h, ])); raw <- raw[-seq_len(h), ]
stopifnot(identical(suppressWarnings(as.numeric(hdr[8:15])), YEARS))

iran <- raw[!is.na(raw[[7]]) & as.numeric(raw[[7]]) == 364, ]
out <- data.frame(destination = iran[[2]], destination_code = as.integer(iran[[5]]),
                  origin = iran[[6]], origin_code = as.integer(iran[[7]]),
                  stringsAsFactors = FALSE)
for (i in seq_along(YEARS)) out[[paste0("X", YEARS[i])]] <- as.numeric(iran[[7 + i]])
out <- out[out$destination_code < 900 & !is.na(out$X2024) & out$X2024 > 0, ]

# --- documented overrides ----------------------------------------------------
ISRAEL_CBS_2024 <- 38200
out$X2024[out$destination_code == 376] <- ISRAEL_CBS_2024
france <- data.frame(destination = "France", destination_code = 250L,
                     origin = "Iran (Islamic Republic of)", origin_code = 364L,
                     X1990 = NA, X1995 = NA, X2000 = 18376, X2005 = 20772,
                     X2010 = NA, X2015 = 22543, X2020 = 24178, X2024 = 24178)
out <- rbind(out[out$destination_code != 250, ], france)
out <- out[order(match(out$destination_code, c(out$destination_code[out$destination_code != 250], 250))), ]

write_csv(out, file.path(DATA_DIR, "stocks_countries.csv"), na = "")
cat("Wrote", file.path(DATA_DIR, "stocks_countries.csv"), "-", nrow(out), "rows; total 2024:",
    sum(out$X2024, na.rm = TRUE), "\n")

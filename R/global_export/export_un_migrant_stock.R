# export_un_migrant_stock.R
# Reads UN International Migrant Stock 2024 Excel file, filters to Iran origin,
# and saves stocks_countries.Rda for the global dashboard page.
#
# Input: ../ID_Analysis/output/dashboard_us/un_migrant_stock_2024_by_dest_origin.xlsx
#   (the raw UN xlsx is pulled and staged by the upstream ID_Analysis pipeline)
# Output: data/global/stocks_countries.Rda  (consumed by R/build_global.R)
#
# Run from the iraniandiaspora.github.io/ directory:
#   Rscript R/global_export/export_un_migrant_stock.R

library(readxl)

OUT_DIR <- file.path(here::here(), "data/global")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# The raw UN xlsx is not committed in the deployment repo (too large) and
# is staged by ID_Analysis for its own pipeline. Look for it in the expected
# upstream location first.
UN_XLSX <- file.path(here::here(), "..", "ID_Analysis", "output", "dashboard_us",
                     "un_migrant_stock_2024_by_dest_origin.xlsx")
if (!file.exists(UN_XLSX)) {
  stop(sprintf(
    "UN migrant stock xlsx not found at %s\nDownload it from the UN (International Migrant Stock, 2024 revision) and stage it at that path, or update UN_XLSX in this script.",
    UN_XLSX))
}

# Read sheet with origin=Iran (origin_code 364)
un_raw <- read_excel(UN_XLSX, sheet = "Table 1")

# Filter to Iran as origin and select destination + year columns
# The exact sheet structure varies by UN revision; adjust column names if needed
iran_rows <- un_raw[un_raw$origin_code == 364 | un_raw$`Origin code` == 364, ]

# If the above fails, the downstream build will already have a usable
# stocks_countries.Rda committed under data/global/; point maintainers there.
if (nrow(iran_rows) == 0) {
  message("Could not filter by origin_code in Excel. Loading committed stocks_countries.Rda to verify structure.")
  load(file.path(OUT_DIR, "stocks_countries.Rda"))
  cat("stocks_countries has", nrow(stocks_countries), "rows\n")
  cat("Total 2024:", sum(stocks_countries$X2024, na.rm = TRUE), "\n")
  stop("Manual intervention needed: Excel sheet structure does not match expected format.")
}

stocks_countries <- iran_rows
save(stocks_countries, file = file.path(OUT_DIR, "stocks_countries.Rda"))
cat("Saved", file.path(OUT_DIR, "stocks_countries.Rda"), "-",
    nrow(stocks_countries), "destination countries\n")
cat("Total 2024:", sum(stocks_countries$X2024, na.rm = TRUE), "\n")

# export_un_migrant_stock.R
# Reads UN International Migrant Stock 2024 Excel file, filters to Iran origin,
# and saves stocks_countries.Rda for the global dashboard page.
#
# Input: output/dashboard_us/un_migrant_stock_2024_by_dest_origin.xlsx
# Output: output/dashboard_us/stocks_countries.Rda

library(readxl)

DATA_DIR <- file.path(here::here(), "output/dashboard_us")

# Read sheet with origin=Iran (origin_code 364)
un_raw <- read_excel(
  file.path(DATA_DIR, "un_migrant_stock_2024_by_dest_origin.xlsx"),
  sheet = "Table 1"
)

# Filter to Iran as origin and select destination + year columns
# The exact sheet structure varies by UN revision; adjust column names if needed
iran_rows <- un_raw[un_raw$origin_code == 364 | un_raw$`Origin code` == 364, ]

# If the above fails, try the stocks_countries structure directly:
# The existing .Rda has columns: destination, origin_code, X1990..X2024
if (nrow(iran_rows) == 0) {
  message("Could not filter by origin_code in Excel. Loading existing stocks_countries.Rda to verify structure.")
  load(file.path(DATA_DIR, "stocks_countries.Rda"))
  cat("stocks_countries has", nrow(stocks_countries), "rows\n")
  cat("Total 2024:", sum(stocks_countries$X2024, na.rm = TRUE), "\n")
  stop("Manual intervention needed: Excel sheet structure does not match expected format.")
}

stocks_countries <- iran_rows
save(stocks_countries, file = file.path(DATA_DIR, "stocks_countries.Rda"))
cat("Saved stocks_countries.Rda:", nrow(stocks_countries), "destination countries\n")
cat("Total 2024:", sum(stocks_countries$X2024, na.rm = TRUE), "\n")

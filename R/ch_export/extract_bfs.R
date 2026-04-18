# Extract Iran-born population data from BFS (Swiss Federal Statistical Office)
# PX-Web API.
#
# Run from deployment repo root:
#   Rscript R/ch_export/extract_bfs.R
#
# Output: data/switzerland/
#   ch_headline.csv  - total, male, female, swiss_citizen, iranian_citizen, foreign
#   ch_canton.csv    - canton_name, canton_code, iran_born
#   ch_trend.csv     - year, total (2010-2024)
#   ch_arrivals.csv  - year, count (2011-2024 immigration flow)
#
# Data source: BFS PX-Web API (no authentication needed)
#   Base URL: https://www.pxweb.bfs.admin.ch/api/v1/en/
#
# Iran codes: 8513 (country of birth), 8100 (Switzerland citizenship).
#
# Ported from extract_bfs.py on 2026-04-19 to keep the project R-only.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

BASE    <- "https://www.pxweb.bfs.admin.ch/api/v1/en"
OUT_DIR <- "data/switzerland"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# BFS full names -> short names (for matching with GeoJSON)
BFS_SHORT_NAME <- c(
  "Z\u00fcrich" = "Z\u00fcrich",
  "Bern / Berne" = "Bern",
  "Luzern" = "Luzern",
  "Uri" = "Uri",
  "Schwyz" = "Schwyz",
  "Obwalden" = "Obwalden",
  "Nidwalden" = "Nidwalden",
  "Glarus" = "Glarus",
  "Zug" = "Zug",
  "Fribourg / Freiburg" = "Fribourg",
  "Solothurn" = "Solothurn",
  "Basel-Stadt" = "Basel-Stadt",
  "Basel-Landschaft" = "Basel-Landschaft",
  "Schaffhausen" = "Schaffhausen",
  "Appenzell Ausserrhoden" = "Appenzell Ausserrhoden",
  "Appenzell Innerrhoden" = "Appenzell Innerrhoden",
  "St. Gallen" = "St. Gallen",
  "Graub\u00fcnden / Grigioni / Grischun" = "Graub\u00fcnden",
  "Aargau" = "Aargau",
  "Thurgau" = "Thurgau",
  "Ticino" = "Ticino",
  "Vaud" = "Vaud",
  "Valais / Wallis" = "Valais",
  "Neuch\u00e2tel" = "Neuch\u00e2tel",
  "Gen\u00e8ve" = "Gen\u00e8ve",
  "Jura" = "Jura"
)

# POST a PX-Web query and return parsed JSON-stat2 response as a list.
post_json <- function(table_id, query) {
  url <- sprintf("%s/%s/%s.px", BASE, table_id, table_id)
  body <- toJSON(query, auto_unbox = TRUE)
  resp <- POST(url, body = body, encode = "raw",
               add_headers(`Content-Type` = "application/json"),
               timeout(60))
  stop_for_status(resp)
  fromJSON(content(resp, as = "text", encoding = "UTF-8"),
           simplifyVector = FALSE)
}

# Return a list mapping index (as character) -> label for a given dimension.
# The JSON-stat2 "index" object maps code -> position; "label" maps code -> name.
# We return a named list where names are positions (0, 1, 2...) and values are
# the human-readable labels, ordered by position.
dim_labels <- function(d, dim_name) {
  cats <- d$dimension[[dim_name]]$category
  idx  <- cats$index   # code -> position
  lbl  <- cats$label   # code -> label
  # Reverse: position -> label
  positions <- unlist(idx)
  out <- character(length(positions))
  for (code in names(idx)) {
    out[idx[[code]] + 1L] <- lbl[[code]]
  }
  names(out) <- as.character(seq_along(out) - 1L)
  out
}

# Same as dim_labels but returns position -> code (the original key).
dim_codes <- function(d, dim_name) {
  idx <- d$dimension[[dim_name]]$category$index
  out <- character(length(idx))
  for (code in names(idx)) {
    out[idx[[code]] + 1L] <- code
  }
  names(out) <- as.character(seq_along(out) - 1L)
  out
}

write_csv <- function(filename, df) {
  path <- file.path(OUT_DIR, filename)
  # quote = FALSE matches the Python csv.DictWriter output byte-for-byte; none
  # of the fields contain commas so this is safe.
  write.csv(df, path, row.names = FALSE, quote = FALSE)
  cat(sprintf("  %s: %d rows\n", filename, nrow(df)))
}

# =========================================================================
# 1. Headline: total, male, female, Swiss citizen, Iranian citizen (2024)
# =========================================================================
cat("Pulling headline data...\n")
d <- post_json("px-x-0103010000_499", list(
  query = list(
    list(code = "Jahr",               selection = list(filter = "item", values = list("2024"))),
    list(code = "Kanton",              selection = list(filter = "item", values = list("8100"))),
    list(code = "Bev\u00f6lkerungstyp", selection = list(filter = "item", values = list("1"))),
    list(code = "Geburtsstaat",        selection = list(filter = "item", values = list("8513"))),
    list(code = "Staatsangeh\u00f6rigkeit",
         selection = list(filter = "item", values = list("-99999", "8100", "8513"))),
    list(code = "Geschlecht",          selection = list(filter = "item", values = list("-99999", "1", "2")))
  ),
  response = list(format = "json-stat2")
))

# Dimensions: Citizenship(3) x Sex(3) -> 9 values
# Order: [total/total, total/male, total/female,
#         swiss/total, swiss/male, swiss/female,
#         iranian/total, iranian/male, iranian/female]
v <- unlist(d$value)
total          <- v[1]
male           <- v[2]
female         <- v[3]
swiss_citizen  <- v[4]
iranian_citizen <- v[7]
foreign        <- total - swiss_citizen   # includes Iranian + other foreign

headline <- data.frame(
  category = c("total", "male", "female", "swiss_citizen",
               "iranian_citizen", "foreign"),
  count = c(total, male, female, swiss_citizen, iranian_citizen, foreign),
  year = 2024
)
write_csv("ch_headline.csv", headline)
cat(sprintf("  Total: %s (M: %s, F: %s)\n",
            format(total, big.mark = ","),
            format(male, big.mark = ","),
            format(female, big.mark = ",")))
cat(sprintf("  Swiss citizens: %s, Iranian citizens: %s, Other foreign: %s\n",
            format(swiss_citizen, big.mark = ","),
            format(iranian_citizen, big.mark = ","),
            format(total - swiss_citizen - iranian_citizen, big.mark = ",")))

# =========================================================================
# 2. Canton breakdown (2024)
# =========================================================================
cat("Pulling canton data...\n")
d <- post_json("px-x-0103010000_499", list(
  query = list(
    list(code = "Jahr",               selection = list(filter = "item", values = list("2024"))),
    list(code = "Kanton",              selection = list(filter = "all",  values = list("*"))),
    list(code = "Bev\u00f6lkerungstyp", selection = list(filter = "item", values = list("1"))),
    list(code = "Geburtsstaat",        selection = list(filter = "item", values = list("8513"))),
    list(code = "Staatsangeh\u00f6rigkeit",
         selection = list(filter = "item", values = list("-99999"))),
    list(code = "Geschlecht",          selection = list(filter = "item", values = list("-99999")))
  ),
  response = list(format = "json-stat2")
))
canton_labels <- dim_labels(d, "Kanton")
canton_codes  <- dim_codes(d, "Kanton")
vals <- unlist(d$value)

canton_df <- data.frame(canton_name = character(0),
                         canton_code = character(0),
                         iran_born = integer(0),
                         stringsAsFactors = FALSE)
for (i in seq_along(canton_labels)) {
  name <- canton_labels[i]
  if (name %in% c("Switzerland", "No indication")) next
  short <- if (!is.na(BFS_SHORT_NAME[name])) BFS_SHORT_NAME[[name]] else name
  canton_df <- rbind(canton_df, data.frame(
    canton_name = short,
    canton_code = canton_codes[i],
    iran_born   = as.integer(vals[i]),
    stringsAsFactors = FALSE
  ))
}
write_csv("ch_canton.csv", canton_df)

# =========================================================================
# 3. Time series 2010-2024 (national, total sex)
# =========================================================================
cat("Pulling trend data...\n")
d <- post_json("px-x-0103010000_499", list(
  query = list(
    list(code = "Jahr",               selection = list(filter = "all",  values = list("*"))),
    list(code = "Kanton",              selection = list(filter = "item", values = list("8100"))),
    list(code = "Bev\u00f6lkerungstyp", selection = list(filter = "item", values = list("1"))),
    list(code = "Geburtsstaat",        selection = list(filter = "item", values = list("8513"))),
    list(code = "Staatsangeh\u00f6rigkeit",
         selection = list(filter = "item", values = list("-99999"))),
    list(code = "Geschlecht",          selection = list(filter = "item", values = list("-99999")))
  ),
  response = list(format = "json-stat2")
))
year_labels <- dim_labels(d, "Jahr")
vals <- unlist(d$value)
trend_df <- data.frame(
  year  = as.integer(year_labels),
  total = as.integer(vals),
  stringsAsFactors = FALSE
)
trend_df <- trend_df[order(trend_df$year), ]
write_csv("ch_trend.csv", trend_df)

# =========================================================================
# 4. Immigration flow 2011-2024 (annual arrivals, national)
# =========================================================================
cat("Pulling immigration arrivals...\n")
d <- post_json("px-x-0103020200_103", list(
  query = list(
    list(code = "Jahr",           selection = list(filter = "all",  values = list("*"))),
    list(code = "Kanton",          selection = list(filter = "item", values = list("0"))),
    list(code = "Staatsangeh\u00f6rigkeit (Kategorie)",
         selection = list(filter = "item", values = list("0"))),
    list(code = "Geburtsstaat",    selection = list(filter = "item", values = list("8513"))),
    list(code = "Geschlecht",      selection = list(filter = "item", values = list("0"))),
    list(code = "Altersklasse",    selection = list(filter = "item", values = list("0")))
  ),
  response = list(format = "json-stat2")
))
year_labels <- dim_labels(d, "Jahr")
vals <- unlist(d$value)
arrivals_df <- data.frame(
  year  = as.integer(year_labels),
  count = as.integer(vals),
  stringsAsFactors = FALSE
)
arrivals_df <- arrivals_df[order(arrivals_df$year), ]
write_csv("ch_arrivals.csv", arrivals_df)

cat("\nDone. All CSVs written to", OUT_DIR, "\n")

# Extract Italy Iran-born data from Eurostat + ISTAT population registers.
# Run from deployment repo root:
#   Rscript R/it_export/extract_istat.R
#
# Input:  data/europe/iran_born_combined.csv (Eurostat migr_pop3ctb)
#         https://demo.istat.it/data/rcs/Dati_RCS_nascita_2025.zip (ISTAT)
# Output: data/italy/it_trend.csv
#         data/italy/it_region.csv
#         data/italy/it_headline.csv

suppressPackageStartupMessages(library(dplyr))

OUT_DIR <- "data/italy"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 1. Trend from Eurostat ---------------------------------------------------
cat("Extracting Italy trend from Eurostat...\n")
euro <- read.csv("data/europe/iran_born_combined.csv", stringsAsFactors = FALSE)
it_trend <- euro %>%
  filter(geo == "IT") %>%
  transmute(year = as.integer(year), iran_born = as.integer(value)) %>%
  arrange(year)
write.csv(it_trend, file.path(OUT_DIR, "it_trend.csv"), row.names = FALSE)
cat(sprintf("  Wrote %d rows to it_trend.csv (%d-%d)\n",
            nrow(it_trend), min(it_trend$year), max(it_trend$year)))

# --- 2. Regional breakdown from ISTAT ----------------------------------------
cat("Downloading ISTAT birth-country data...\n")
zip_url <- "https://demo.istat.it/data/rcs/Dati_RCS_nascita_2025.zip"
zip_path <- tempfile(fileext = ".zip")
download.file(zip_url, zip_path, mode = "wb", quiet = TRUE)
csv_name <- unzip(zip_path, list = TRUE)$Name[1]
unzip(zip_path, exdir = tempdir())
csv_path <- file.path(tempdir(), csv_name)

cat("Parsing ISTAT CSV...\n")
# Semicolon-delimited, read only what we need
raw <- read.csv(csv_path, sep = ";", stringsAsFactors = FALSE,
                colClasses = "character")
names(raw) <- c("anno", "codice_istat", "denominazione", "codice_stato",
                "stato", "zona", "continente", "maschi", "femmine", "totale")

# Filter to Iran (code 332)
iran <- raw %>%
  filter(codice_stato == "332") %>%
  mutate(maschi = as.integer(maschi),
         femmine = as.integer(femmine),
         totale = as.integer(totale))

# National total: codice_istat == "IT"
national <- iran %>% filter(codice_istat == "IT")
cat(sprintf("  National total: %s (%s male, %s female)\n",
            format(national$totale, big.mark = ","),
            format(national$maschi, big.mark = ","),
            format(national$femmine, big.mark = ",")))

# Regions: codice_istat is 2-digit zero-padded (01-20)
regions <- iran %>%
  filter(grepl("^[0-9]{2}$", codice_istat)) %>%
  transmute(region_code = codice_istat,
            region_name = denominazione,
            male = maschi,
            female = femmine,
            iran_born = totale) %>%
  arrange(desc(iran_born))

write.csv(regions, file.path(OUT_DIR, "it_region.csv"), row.names = FALSE)
cat(sprintf("  Wrote %d regions to it_region.csv\n", nrow(regions)))

# --- 3. Headline --------------------------------------------------------------
headline <- data.frame(
  category = c("total", "male", "female"),
  count = c(national$totale, national$maschi, national$femmine),
  year = 2025L,
  stringsAsFactors = FALSE
)
write.csv(headline, file.path(OUT_DIR, "it_headline.csv"), row.names = FALSE)
cat("  Wrote it_headline.csv\n")

# Cleanup
unlink(zip_path)
unlink(csv_path)

cat("\nDone. Summary:\n")
cat(sprintf("  Iran-born in Italy: %s\n", format(national$totale, big.mark = ",")))
cat(sprintf("  Top 5 regions: %s\n",
            paste(sprintf("%s (%s)", regions$region_name[1:5],
                          format(regions$iran_born[1:5], big.mark = ",")),
                  collapse = ", ")))

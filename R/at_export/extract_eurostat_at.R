# Extract Austria Iran-born trend from Eurostat + UN Migrant Stock.
# Run from deployment repo root:
#   Rscript R/at_export/extract_eurostat_at.R
#
# Input:  data/europe/iran_born_combined.csv, data/global/stocks_countries.csv
# Output: data/austria/at_trend.csv

cat("Extracting Austria from Eurostat + UN...\n")

combined <- read.csv("data/europe/iran_born_combined.csv",
                     stringsAsFactors = FALSE)
at <- combined[combined$geo == "AT", ]

# Add UN 1990, 1995, 2000 data points
un <- read.csv("data/global/stocks_countries.csv", stringsAsFactors = FALSE)
un_at <- un[grepl("Austria", un$destination), ]
if (nrow(un_at) > 0) {
  un_pts <- data.frame(year = c(1990L, 1995L, 2000L),
    iran_born = c(un_at$X1990, un_at$X1995, un_at$X2000))
  un_pts <- un_pts[!un_pts$year %in% at$year, ]
  un_df <- data.frame(year = un_pts$year, value = un_pts$iran_born, stringsAsFactors = FALSE)
  at_slim <- data.frame(year = as.integer(at$year), value = as.integer(at$value), stringsAsFactors = FALSE)
  at_slim <- rbind(un_df, at_slim)
}

if (exists("at_slim")) at <- at_slim
at <- at[order(at$year), ]

out <- data.frame(year = as.integer(at$year),
                  iran_born = as.integer(at$value))

# Interpolate 2010-2011 gap (Eurostat has no data for these years)
if (!2010 %in% out$year & 2009 %in% out$year & 2012 %in% out$year) {
  v2009 <- out$iran_born[out$year == 2009]
  v2012 <- out$iran_born[out$year == 2012]
  gap <- data.frame(
    year = c(2010L, 2011L),
    iran_born = as.integer(round(c(
      v2009 + (v2012 - v2009) * 1/3,
      v2009 + (v2012 - v2009) * 2/3
    )))
  )
  out <- rbind(out, gap)
  out <- out[order(out$year), ]
}

dir.create("data/austria", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "data/austria/at_trend.csv", row.names = FALSE)

cat(sprintf("  Wrote %d rows to data/austria/at_trend.csv\n", nrow(out)))
cat(sprintf("  Year range: %d-%d\n", min(out$year), max(out$year)))
cat(sprintf("  Latest value: %s\n", format(out$iran_born[nrow(out)], big.mark = ",")))
cat("  Done\n")

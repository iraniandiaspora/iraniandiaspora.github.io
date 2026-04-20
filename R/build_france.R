# Build France pages from INSEE extract.
# Run from deployment repo root:
#   Rscript R/build_france.R
#
# Input:  data/france/fr_trend.csv, fr_headline.csv
# Output: docs/pages/fr-population.html
#
# Extract first via: Rscript R/fr_export/extract_insee.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/france"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
page_template <- function(title, body_html) {
  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>', title, '</title>
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap" rel="stylesheet">
<script src="lib/plotly-3.4.0.min.js"></script>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family:"Montserrat",sans-serif; background:#fafafa; color:#333; padding:15px 40px; max-width:100%; overflow-x:hidden; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.text-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.text-card { background:white; border-radius:8px; padding:20px; text-align:center;
  font-size:15px; line-height:1.6; border:1px solid #e0e0e0; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:hidden; min-width:0; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.headline { background:white; border-radius:8px; padding:30px; text-align:center; border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em; }
a { transition: color 0.15s; }
a:hover { color: #1a4e72 !important; text-decoration: underline; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }
.wide-card { background:white; border-radius:8px; padding:24px 28px; border:1px solid #e0e0e0; margin-bottom:20px; font-size:14px; line-height:1.7; color:#333; }
.wide-card h3 { font-size:16px; font-weight:600; color:#1a4e72; margin-bottom:10px; }
.wide-card ul { padding-left:20px; margin:6px 0 10px; }
.wide-card li { margin-bottom:4px; }
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row, .text-row { grid-template-columns:1fr !important; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .headline .number { font-size:34px; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .headline .number { font-size:28px; }
  .chart-card { padding:10px; }
  .text-card { font-size:13px; padding:14px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
</head>
<body>
', body_html, '
', iframe_resize_script, '
</body>
</html>')
}

# --- Source citation strings ---
INSEE_LINK <- "<a href='https://www.insee.fr/fr/statistiques/6478089' target='_blank' style='color:#2774AE;'>INSEE</a>"
INSEE_SOURCE <- paste0("Source: ", INSEE_LINK, " &mdash; Recensement de la population, Iran-born residents 2006\u20132019")

# --- Load data ---------------------------------------------------------------
cat("Loading France extracts...\n")
trend <- read.csv(file.path(DATA_DIR, "fr_trend.csv"), stringsAsFactors = FALSE)
hl    <- read.csv(file.path(DATA_DIR, "fr_headline.csv"), stringsAsFactors = FALSE)
trend <- trend[order(trend$year), ]

fr_total  <- hl$count[hl$category == "total"]
data_yr   <- hl$year[hl$category == "total"]
fr_min_yr <- min(trend$year)

# =============================================================================
# FR-POPULATION
# =============================================================================
cat("Building fr-population...\n")

# --- Historical trend (INSEE, line + markers; gap at 2018) -------------------
p_hist <- plot_ly(trend, x = ~year, y = ~iran_born, type = "scatter",
    mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 6),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      trend$year, format(trend$iran_born, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(
      text = sprintf("<b>Iran-Born Population in France,<br>%d\u2013%d</b>",
                     fr_min_yr, data_yr),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Assemble fr-population page ---------------------------------------------
pop_body <- paste0(
  # Top row: headline with identification box (left) + trend chart (right)
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in France</div>',
  '<div class="number">', format(fr_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the Recensement de la population maintained by ',
  INSEE_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">France uses a rolling annual census. A person is counted as Iran-born based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> <span style="color:#888;">&mdash; "Dans quel pays \u00eates-vous n\u00e9(e)?"</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">French law (Loi Informatique et Libert\u00e9s, Art. 8) prohibits collecting ethnicity or ancestry statistics, so French-born children of Iran-born parents are not separately identified. INSEE\u2019s detailed country-of-birth tables are also released with a multi-year delay, so ', data_yr, ' is the most recent Iran-specific figure currently published.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card">',
  plotly_div("fr-hist", plotly_to_json(p_hist), "430px", source = INSEE_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("France: Population", pop_body),
           "docs/pages/fr-population.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nFrance: %s Iran-born (%d)\n",
  format(fr_total, big.mark = ","), data_yr))
cat(sprintf("Trend: %d rows (%d-%d)\n",
  nrow(trend), fr_min_yr, data_yr))

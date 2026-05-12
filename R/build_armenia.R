# Build Armenia page.
# Run from deployment repo root:
#   Rscript R/build_armenia.R
#
# Input:  data/armenia/am_trend.csv, am_headline.csv
# Output: docs/pages/am-population.html
#
# Extract first via: Rscript R/am_export/extract_armenia.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/armenia"

source("R/_helpers.R")

# Match the standard page_template helper from other builders.
page_template <- function(title, body_html, has_tabs = FALSE) {
  extra_head <- ""
  tab_switch_script <- ""
  paste0('<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>', title, '</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700&display=swap" rel="stylesheet">
<script src="lib/plotly-3.4.0.min.js"></script>
<style>
body { font-family:"Montserrat",system-ui,sans-serif; color:#222; background:#f6f7f9; margin:0; padding:16px 14px 28px; box-sizing:border-box; overflow-x:hidden; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; overflow:hidden; min-width:0; }
.headline { background:white; border-radius:8px; padding:24px 22px; border:1px solid #e0e0e0; text-align:center; display:flex; flex-direction:column; justify-content:center; }
.headline .label { font-size:14px; color:#555; margin-bottom:4px; }
.headline .number { font-size:46px; font-weight:700; color:#1a4e72; line-height:1.1; }
@media (max-width:900px) {
  .chart-row { grid-template-columns:1fr; }
  .headline { padding:18px; }
  .headline .number { font-size:36px; }
  .chart-card { padding:12px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
', extra_head, '
</head>
<body>
', body_html, '
', iframe_resize_script, '
</body>
</html>')
}

# --- Source citation strings ---
ARMSTAT_LINK <- "<a href='https://armstat.am/en/?nid=82&id=2623' target='_blank' style='color:#2774AE;'>Armstat</a>"
UN_LINK <- "<a href='https://www.un.org/development/desa/pd/content/international-migrant-stock' target='_blank' style='color:#2774AE;'>UN DESA</a>"

TREND_SOURCE <- paste0("Source: ", UN_LINK, " International Migrant Stock 2024 and ", ARMSTAT_LINK, " Population Censuses 2011 and 2022.")

# --- Load data ---------------------------------------------------------------
cat("Loading Armenia extracts...\n")
trend <- read.csv(file.path(DATA_DIR, "am_trend.csv"), stringsAsFactors = FALSE)
hl    <- read.csv(file.path(DATA_DIR, "am_headline.csv"), stringsAsFactors = FALSE)

am_total <- hl$count[hl$category == "iran_born"]
am_year  <- hl$year[hl$category == "iran_born"]
iran_cit <- hl$count[hl$category == "iranian_citizens"]

# --- Historical trend chart (UN snapshots + Armstat censuses) ---------------
# Use line+markers because the UN series is 5-yearly and the Armstat census
# points are single census-day snapshots; bars would imply continuous coverage.
# Both Armstat census points (2011 and 2022) align closely with the UN
# series, corroborating the long-run decline.
is_armstat <- grepl("Armstat", trend$source)

p_hist <- plot_ly() %>%
  add_trace(
    data = trend %>% filter(!is_armstat),
    x = ~year, y = ~iran_born,
    type = "scatter", mode = "lines+markers",
    name = "UN Migrant Stock",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 8),
    text = ~sprintf("<b>%d</b><br>%s Iran-born (UN estimate)",
      year, format(iran_born, big.mark = ",")),
    hoverinfo = "text"
  ) %>%
  add_trace(
    data = trend %>% filter(is_armstat),
    x = ~year, y = ~iran_born,
    type = "scatter", mode = "markers",
    name = "Armstat census",
    marker = list(color = "#c4793a", size = 12, symbol = "diamond",
                  line = list(color = "#1a4e72", width = 1.5)),
    text = ~sprintf("<b>%d</b><br>%s Iran-born (Armstat census)",
      year, format(iran_born, big.mark = ",")),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Iran-Born Population in Armenia,<br>1990–2024</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 55, b = 60, l = 60, r = 40),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18,
                  font = list(size = 12)),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Assemble am-population page ---------------------------------------------
pop_body <- paste0(
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in Armenia</div>',
  '<div class="number">', format(am_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the ', am_year, ' Population Census maintained by ',
  ARMSTAT_LINK, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Armenia counts Iran-born residents based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Place of birth</strong> <span style="color:#888;">&mdash; recorded in the 2022 census short form</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Armenia does not separately identify children of Iran-born parents.</p>',
  sprintf('<p style="margin-top:8px; font-size:11px; color:#999; line-height:1.5;">Of the %s Iran-born residents in 2022:</p>',
    format(am_total, big.mark = ",")),
  '<ul style="padding-left:18px; margin:4px 0 0; font-size:11px; color:#999; line-height:1.7;">',
  sprintf('<li>%s still held an Iranian passport</li>', format(iran_cit, big.mark = ",")),
  '<li>249 held both Iranian and Armenian citizenship</li>',
  sprintf('<li>The remaining %s had naturalized as Armenian citizens</li>',
    format(am_total - iran_cit - 249, big.mark = ",")),
  '</ul>',
  '<p style="margin-top:8px; font-size:11px; color:#999; line-height:1.5;">The UN Migrant Stock series uses interpolation between census years and gives a slightly higher 2024 estimate of 6,793.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card">',
  plotly_div("am-hist", plotly_to_json(p_hist), "430px", source = TREND_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Armenia: Population", pop_body),
           "docs/pages/am-population.html")
cat("  Done\n")

cat(sprintf("\nArmenia: %s Iran-born (Armstat %d), %s Iranian citizens; UN trend %d-%d\n",
  format(am_total, big.mark = ","), am_year,
  format(iran_cit, big.mark = ","),
  min(trend$year), max(trend$year)))

# Build Austria pages from Eurostat Iran-born data.
# Run from deployment repo root:
#   Rscript R/build_austria.R
#
# Input:  data/austria/at_trend.csv
# Output: docs/pages/at-population.html
#
# Extract first via: Rscript R/at_export/extract_eurostat_at.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/austria"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
tab_switch_script <- '
<script>
function switchTab(tabId, btn, groupId) {
  var panels = document.querySelectorAll(".tab-panel[data-group=\'" + groupId + "\']");
  panels.forEach(function(p) { p.classList.remove("active"); });
  document.getElementById(tabId).classList.add("active");
  var btns = btn.parentElement.querySelectorAll(".tab-btn");
  btns.forEach(function(b) { b.classList.remove("active"); });
  btn.classList.add("active");
  var active = document.getElementById(tabId);
  active.querySelectorAll(".js-plotly-plot").forEach(function(p) {
    if (window.Plotly) Plotly.Plots.resize(p);
  });
  reportHeight();
}
</script>'

page_template <- function(title, body_html, has_tabs = FALSE) {
  tab_css <- if (has_tabs) '
.tab-bar { display:flex; justify-content:center; gap:0; margin:12px 0 0; }
.tab-btn { padding:6px 16px; border:1px solid #ddd; background:#f0f0f0; cursor:pointer;
  font-family:"Montserrat",sans-serif; font-size:13px; color:#333; border-radius:4px; margin:0 2px; transition:background 0.15s; white-space:nowrap; }
.tab-btn.active { background:#2774AE; color:white; font-weight:600; border-color:#2774AE; }
.tab-btn:hover:not(.active) { background:#e0e0e0; }
.tab-panel { display:none; }
.tab-panel.active { display:block; }' else ''

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
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }', tab_css, '
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row, .text-row { grid-template-columns:1fr !important; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .headline .number { font-size:34px; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
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
', if (has_tabs) tab_switch_script else '', '
', iframe_resize_script, '
</body>
</html>')
}

# --- Source citation strings ---
STAT_AT_LINK <- "<a href='https://www.statistik.at/en/' target='_blank' style='color:#2774AE;'>Statistics Austria</a>"
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
TREND_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iran-born population stock, 1990\u20132025")

# --- Load data ---------------------------------------------------------------
cat("Loading Austria trend data...\n")
trend <- read.csv(file.path(DATA_DIR, "at_trend.csv"), stringsAsFactors = FALSE)
trend <- trend[order(trend$year), ]

at_latest <- trend$iran_born[nrow(trend)]
at_latest_yr <- trend$year[nrow(trend)]
at_min_yr <- min(trend$year)

# =============================================================================
# AT-POPULATION
# =============================================================================
cat("Building at-population...\n")

# --- Historical trend line chart -----------------------------------------------
p_hist <- plot_ly(trend, x = ~year, y = ~iran_born, type = "scatter",
    mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 5),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      trend$year, format(trend$iran_born, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(
      text = sprintf("<b>Iran-Born Population in Austria,<br>%d\u2013%d</b>",
                     at_min_yr, at_latest_yr),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Bundesland horizontal bar chart ------------------------------------------
bl <- read.csv(file.path(DATA_DIR, "at_bundesland.csv"), stringsAsFactors = FALSE)
bl <- bl[order(bl$iran_born), ]
bl$bundesland <- factor(bl$bundesland, levels = bl$bundesland)
bl_total <- sum(bl$iran_born)
bl$pct <- round(bl$iran_born / bl_total * 100, 1)

CENSUS_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Census 2021, Iran-born by region")

p_bl <- plot_ly(bl, y = ~bundesland, x = ~iran_born, type = "bar",
    orientation = "h",
    marker = list(color = "#2774AE",
                  line = list(color = "#1a4e72", width = 0.4)),
    text = ~sprintf("<b>%s</b><br>%s Iran-born (%s%%)",
      bundesland, format(iran_born, big.mark = ","), pct),
    hoverinfo = "text",
    textposition = "none") %>%
  layout(
    title = list(
      text = "<b>Iran-Born by Bundesland,<br>Census 2021</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ",", tickfont = list(size = 10)),
    yaxis = list(title = "", tickfont = list(size = 11),
                 ticks = "outside", ticklen = 8,
                 tickcolor = "rgba(0,0,0,0)"),
    margin = list(t = 45, b = 30, l = 140, r = 20),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE,
    bargap = 0.35
  ) %>% config(displayModeBar = FALSE)

# --- Bundesland choropleth map ------------------------------------------------
at_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "at_bundesland.geojson"),
                                 simplifyVector = FALSE)

p_bl_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = at_geojson,
    locations = bl$bundesland, z = bl$iran_born,
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>%s Iran-born (%s%%)",
      bl$bundesland, format(bl$iran_born, big.mark = ","), bl$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                      c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = 13.5, lat = 47.6), zoom = 5.4),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Sex breakdown boxes (from Bundesland census data) -------------------------
at_male   <- sum(bl$male)
at_female <- sum(bl$female)

make_gen_box <- function(val, pct_text, label, sublabel, color) {
  sprintf(
    '<div style="background:%s; border-radius:6px; padding:22px 14px; text-align:center; color:white; flex:1; min-width:0;">
      <div style="font-size:30px; font-weight:700; line-height:1.1;">%s</div>
      <div style="font-size:13px; margin-top:4px; font-weight:600;">%s</div>
      <div style="font-size:12px; opacity:0.9; margin-top:2px;">%s</div>
      <div style="font-size:11px; opacity:0.85; margin-top:3px;">%s</div>
    </div>',
    color, format(val, big.mark = ","), label, pct_text, sublabel)
}

sex_boxes <- paste0(
  '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">Iran-Born Population by Sex</div>',
  '<div style="display:flex; gap:12px; margin-top:12px;">',
  make_gen_box(at_male, paste0(round(at_male / bl_total * 100), "% of total"),
    "Male", "Iran-born men", "#1a4e72"),
  make_gen_box(at_female, paste0(round(at_female / bl_total * 100), "% of total"),
    "Female", "Iran-born women", "#5a9bd5"),
  '</div>')

# --- Assemble at-population page (Italy format) --------------------------------
pop_body <- paste0(
  # Top row: headline (left) + sex boxes card (right)
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in Austria</div>',
  '<div class="number">', format(at_latest, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  STAT_AT_LINK, ', ', at_latest_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Austria uses population registers. A person is counted as Iran-born based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> <span style="color:#888;">&mdash; recorded in the Central Register of Residents</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Austrian-born children of Iran-born parents are not counted as Iran-born in population statistics. This count reflects first-generation residents only.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  sex_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', CENSUS_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: trend line chart (left) + choropleth map (right)
  '<div class="chart-row">',
  '<div class="chart-card">',
  plotly_div("at-hist", plotly_to_json(p_hist), "430px", source = TREND_SOURCE),
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in Austria</div>',
  plotly_div("at-map", plotly_to_json(p_bl_map), "430px", source = CENSUS_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Austria: Population", pop_body, has_tabs = FALSE),
           "docs/pages/at-population.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nAustria: %s Iran-born (%d)\n",
  format(at_latest, big.mark = ","), at_latest_yr))
cat(sprintf("Trend: %d years (%d-%d)\n", nrow(trend), at_min_yr, at_latest_yr))

# Build Finland pages from Statistics Finland (StatFin) extracts.
# Run from deployment repo root:
#   Rscript R/build_finland.R
#
# Input:  data/finland/fi_trend.csv, fi_age_2025.csv, fi_headline.csv
# Output: docs/pages/fi-population.html
#
# Extract first via: Rscript R/fi_export/extract_statfin.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/finland"

source("R/_helpers.R")

iframe_resize_script <- "<script>
window.addEventListener('load', function(){
  // ResizeObserver to communicate height to parent iframe
  if (window.parent !== window) {
    var send = function() {
      var h = document.documentElement.scrollHeight;
      window.parent.postMessage({type:'iframeHeight', height:h}, '*');
    };
    new ResizeObserver(send).observe(document.body);
    send();
  }
});
</script>"

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
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row, .text-row { grid-template-columns:1fr !important; }
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
STATFIN_LINK <- "<a href='https://stat.fi/en/' target='_blank' style='color:#2774AE;'>Statistics Finland</a>"

# --- Load data ---------------------------------------------------------------
cat("Loading Finland extracts...\n")
trend  <- read.csv(file.path(DATA_DIR, "fi_trend.csv"), stringsAsFactors = FALSE)
hl     <- read.csv(file.path(DATA_DIR, "fi_headline.csv"), stringsAsFactors = FALSE)
region <- read.csv(file.path(DATA_DIR, "fi_regions.csv"), stringsAsFactors = FALSE)
trend  <- trend[order(trend$year), ]

fi_total  <- hl$count[hl$category == "total"]
fi_gen1   <- hl$count[hl$category == "gen1"]
fi_gen2   <- hl$count[hl$category == "gen2"]
data_yr   <- hl$year[hl$category == "total"]
fi_min_yr <- min(trend$year)

STATFIN_BOX_SOURCE <- paste0(
  "Source: ", STATFIN_LINK, " &mdash; Population register, ", data_yr)
TREND_SOURCE <- sprintf(
  "Source: %s &mdash; Iranian-origin population by generation, %d–%d",
  STATFIN_LINK, fi_min_yr, data_yr)
MAP_SOURCE <- paste0(
  "Source: ", STATFIN_LINK, " &mdash; Iran-born population by region")

# =============================================================================
# FI-POPULATION
# =============================================================================
cat("Building fi-population...\n")

# --- Stacked-bar trend: 1st gen + 2nd gen by year ----------------------------
p_hist <- plot_ly(trend) %>%
  add_bars(x = ~year, y = ~gen1, name = "First generation",
    marker = list(color = "#1a4e72"),
    text = ~sprintf("<b>%d</b><br>First generation: %s",
      year, format(gen1, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  add_bars(x = ~year, y = ~gen2, name = "Second generation",
    marker = list(color = "#5a9bd5"),
    text = ~sprintf("<b>%d</b><br>Second generation: %s",
      year, format(gen2, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    barmode = "stack",
    title = list(
      text = sprintf("<b>Iranian-Origin Population in Finland,<br>%d–%d</b>",
                     fi_min_yr, data_yr),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 50),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Region choropleth map (Iran-born by maakunta, 2025) ---------------------
fi_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "fi_regions.geojson"),
                                 simplifyVector = FALSE)
region$pct <- round(region$count / sum(region$count) * 100, 1)

p_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = fi_geojson,
    locations = region$geo_code, z = region$count,
    featureidkey = "properties.maakunta",
    text = sprintf("<b>%s</b><br>%s Iran-born (%.0f%% of 1st gen)",
      region$name, format(region$count, big.mark = ","), region$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                      c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = 26, lat = 65), zoom = 3.6),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Generation boxes --------------------------------------------------------
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
gen_boxes <- paste0(
  '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">Iranian-Origin Population by Generation</div>',
  '<div style="display:flex; gap:12px; margin-top:12px;">',
  make_gen_box(fi_gen1, paste0(round(fi_gen1 / fi_total * 100), "% of total"),
    "First generation", "Born in Iran", "#1a4e72"),
  make_gen_box(fi_gen2, paste0(round(fi_gen2 / fi_total * 100), "% of total"),
    "Second generation", "Born in Finland with Iran-born parent(s)", "#5a9bd5"),
  '</div>')

# --- Assemble fi-population page ---------------------------------------------
pop_body <- paste0(
  # Top row: headline + generation boxes
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Origin Population in Finland</div>',
  '<div class="number">', format(fi_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  STATFIN_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Finland uses a continuous population register. A person is classified as Iranian-origin if they meet at least one of:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Born in Iran</strong> <span style="color:#888;">&mdash; recorded in the Digital and Population Data Services Agency (DVV) register</span></li>',
  '<li><strong>Born in Finland with at least one Iran-born parent</strong> <span style="color:#888;">&mdash; from StatFin\'s &ldquo;background country&rdquo; classification</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Third-generation Finnish-born grandchildren of Iran-born grandparents are not counted as Iranian-origin in the StatFin classification.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  gen_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>',
          STATFIN_BOX_SOURCE),
  '</div>',
  '</div>',
  # Bottom row: trend (left) + region map (right) — map always bottom-right
  # per .claude/rules/new-country-framework.md.
  '<div class="chart-row">',
  '<div class="chart-card">',
  plotly_div("fi-hist", plotly_to_json(p_hist), "430px", source = TREND_SOURCE),
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title">Geographic Distribution in Finland</div>',
  plotly_div("fi-map", plotly_to_json(p_map), "430px", source = MAP_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Finland: Population", pop_body),
           "docs/pages/fi-population.html")
cat("  Done\n")

cat(sprintf("\nFinland: %s Iranian-origin (%s 1st gen + %s 2nd gen), %d; %d-year series\n",
  format(fi_total, big.mark = ","),
  format(fi_gen1, big.mark = ","),
  format(fi_gen2, big.mark = ","),
  data_yr, nrow(trend)))

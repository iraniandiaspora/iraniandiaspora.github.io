# Build Italy pages from ISTAT + Eurostat extracts.
# Run from deployment repo root:
#   Rscript R/build_italy.R
#
# Input:  data/italy/*.csv
# Output: docs/pages/it-population.html
#
# Extract first via: Rscript R/it_export/extract_istat.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/italy"

# --- Helpers (canonical pattern, from build_nl.R) ----------------------------
strip_internal_classes <- function(x) {
  if (is.list(x)) {
    if (inherits(x, "zcolor")) class(x) <- "list"
    return(lapply(x, strip_internal_classes))
  }
  if (inherits(x, "zcolor")) class(x) <- NULL
  x
}

plotly_to_json <- function(p) {
  b <- plotly_build(p)
  b$x$data   <- strip_internal_classes(b$x$data)
  b$x$layout <- strip_internal_classes(b$x$layout)
  if (is.null(b$x$layout$font)) b$x$layout$font <- list()
  b$x$layout$font$family <- "Montserrat, sans-serif"
  b$x$layout$hoverlabel <- list(
    bgcolor = "white", bordercolor = "#ccc",
    font = list(family = "Montserrat, sans-serif", size = 13, color = "#333"))
  list(data   = toJSON(b$x$data, auto_unbox = TRUE),
       layout = toJSON(b$x$layout, auto_unbox = TRUE),
       config = toJSON(b$x$config, auto_unbox = TRUE))
}

plotly_div <- function(id, json, height = "500px", source = NULL) {
  init_js <- sprintf(
    'var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox"});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  chart <- sprintf(
    '<div id="%s" style="width:100%%;height:%s;touch-action:pan-y;"></div>\n<script>(function(){%s})();</script>',
    id, height, init_js)
  if (!is.null(source)) {
    chart <- paste0(chart, sprintf(
      '\n<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0; padding-right:2px;">%s</p>',
      source))
  }
  chart
}

iframe_resize_script <- '
<script>
function reportHeight() {
  if (window.parent !== window) {
    window.parent.postMessage({ type: "iframeHeight", height: document.body.scrollHeight + 20 }, "*");
  }
}
function resizeAllPlots() {
  document.querySelectorAll(".js-plotly-plot").forEach(function(p) {
    if (window.Plotly) Plotly.Plots.resize(p);
  });
  reportHeight();
}
window.addEventListener("load", function(){ setTimeout(resizeAllPlots, 300); });
window.addEventListener("resize", function(){ setTimeout(resizeAllPlots, 150); });
if (window.ResizeObserver) {
  var ro = new ResizeObserver(function(entries) {
    entries.forEach(function(e) {
      var plot = e.target.querySelector(".js-plotly-plot") || (e.target.classList.contains("js-plotly-plot") ? e.target : null);
      if (plot && window.Plotly) Plotly.Plots.resize(plot);
    });
    reportHeight();
  });
  window.addEventListener("load", function() {
    setTimeout(function() {
      document.querySelectorAll(".js-plotly-plot").forEach(function(p) {
        ro.observe(p.parentElement || p);
      });
    }, 500);
  });
}
new MutationObserver(reportHeight).observe(document.body, { childList: true, subtree: true });
</script>'

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
</style>
</head>
<body>
', body_html, '
', iframe_resize_script, '
</body>
</html>')
}

# --- Source citation strings ---
ISTAT_LINK <- "<a href='https://demo.istat.it/' target='_blank' style='color:#2774AE;'>ISTAT</a>"
ISTAT_SOURCE <- paste0("Source: ", ISTAT_LINK, " &mdash; Resident population by country of birth, 2025")
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
HIST_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iran-born population stock, 2002\u20132025")

# --- Load data ---------------------------------------------------------------
cat("Loading Italy extracts...\n")
hl     <- read.csv(file.path(DATA_DIR, "it_headline.csv"), stringsAsFactors = FALSE)
trend  <- read.csv(file.path(DATA_DIR, "it_trend.csv"), stringsAsFactors = FALSE)
region <- read.csv(file.path(DATA_DIR, "it_region.csv"), stringsAsFactors = FALSE,
                   colClasses = c(region_code = "character"))
# Ensure zero-padded 2-digit codes to match GeoJSON
region$region_code <- sprintf("%02d", as.integer(region$region_code))

geojson_path <- file.path(DATA_DIR, "it_regions.geojson")
has_geojson  <- file.exists(geojson_path)
if (!has_geojson) cat("  WARNING: it_regions.geojson not found, map will be skipped\n")

it_total  <- hl$count[hl$category == "total"]
it_male   <- hl$count[hl$category == "male"]
it_female <- hl$count[hl$category == "female"]
data_yr   <- hl$year[1]

# =============================================================================
# IT-POPULATION
# =============================================================================
cat("Building it-population...\n")

# --- Historical trend (Eurostat, line + markers) --------------------------------
p_hist <- plot_ly(trend, x = ~year, y = ~iran_born, type = "scatter",
    mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 5),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      trend$year, format(trend$iran_born, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iran-Born Population in Italy, 2002\u20132025</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Regional horizontal bar (20 regions, descending) ------------------------
region <- region %>% arrange(iran_born)
region$pct <- round(region$iran_born / it_total * 100, 1)

p_region <- plot_ly(region, y = ~reorder(region_name, iran_born), x = ~iran_born,
    type = "bar", orientation = "h",
    marker = list(color = "#2774AE"),
    text = sprintf("<b>%s</b><br>%s Iran-born (%.1f%%)",
      region$region_name, format(region$iran_born, big.mark = ","), region$pct),
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iran-Born by Region in Italy, 2025</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 10)),
    margin = list(t = 40, b = 30, l = 160),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Regional choropleth map -------------------------------------------------
if (has_geojson) {
  it_geojson <- fromJSON(geojson_path, simplifyVector = FALSE)

  p_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = it_geojson,
      locations = region$region_code,
      z = region$iran_born,
      featureidkey = "properties.reg_istat_code",
      text = sprintf("<b>%s</b><br>%s Iran-born (%.1f%%)",
        region$region_name,
        format(region$iran_born, big.mark = ","),
        region$pct),
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                        c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron",
        center = list(lon = 12.5, lat = 42.5), zoom = 4.5),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  map_html <- plotly_div("it-map", plotly_to_json(p_map), "500px",
    source = ISTAT_SOURCE)
} else {
  map_html <- '<div class="section-title">Map unavailable (GeoJSON missing)</div>'
}

# --- Sex breakdown boxes ------------------------------------------------------
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
  '<div style="display:flex; gap:12px; margin-top:12px;">',
  make_gen_box(it_male, paste0(round(it_male / it_total * 100), "% of total"),
    "Male", "Iran-born men", "#1a4e72"),
  make_gen_box(it_female, paste0(round(it_female / it_total * 100), "% of total"),
    "Female", "Iran-born women", "#5a9bd5"),
  '</div>')

# --- Top region share (for console summary) -----------------------------------
top_region <- region$region_name[region$iran_born == max(region$iran_born)]
top_region_pct <- round(max(region$iran_born) / it_total * 100)

# --- Assemble it-population page ----------------------------------------------
pop_body <- paste0(
  # Top row: headline (left) + sex boxes (right)
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in Italy</div>',
  '<div class="number">', format(it_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  ISTAT_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Italy uses population registers. A person is counted as Iran-born based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> <span style="color:#888;">&mdash; recorded in municipal civil registers</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Italian-born children of Iran-born parents are not counted.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  sex_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', ISTAT_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: trend (left) + map (right)
  '<div class="chart-row">',
  '<div class="chart-card">',
  plotly_div("it-hist", plotly_to_json(p_hist), "430px", source = HIST_SOURCE),
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in Italy</div>',
  map_html,
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Italy: Population", pop_body),
           "docs/pages/it-population.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nItaly: %s Iran-born (%s male, %s female)\n",
  format(it_total, big.mark = ","),
  format(it_male, big.mark = ","),
  format(it_female, big.mark = ",")))
cat(sprintf("Regions: %d, top: %s (%d%%)\n",
  nrow(region), top_region, top_region_pct))
cat(sprintf("Trend: %d-%d (%s to %s)\n",
  min(trend$year), max(trend$year),
  format(min(trend$iran_born), big.mark = ","),
  format(max(trend$iran_born), big.mark = ",")))

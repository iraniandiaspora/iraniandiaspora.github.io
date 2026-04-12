# Build Australia pages from pre-exported CSVs
# Run from deployment repo root:
#   Rscript R/build_australia.R
#
# Produces: docs/pages/au-population.html, docs/pages/au-immigration.html

library(plotly)
library(dplyr)
library(jsonlite)

DATA_DIR <- "data/australia"

# --- Helpers (shared with build_canada.R / build_global.R) ---
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
  b$x$data <- strip_internal_classes(b$x$data)
  b$x$layout <- strip_internal_classes(b$x$layout)
  if (is.null(b$x$layout$font)) b$x$layout$font <- list()
  b$x$layout$font$family <- "Montserrat, sans-serif"
  b$x$layout$hoverlabel <- list(
    bgcolor = "white", bordercolor = "#ccc",
    font = list(family = "Montserrat, sans-serif", size = 13, color = "#333"))
  list(data = toJSON(b$x$data, auto_unbox = TRUE),
       layout = toJSON(b$x$layout, auto_unbox = TRUE),
       config = toJSON(b$x$config, auto_unbox = TRUE))
}

plotly_div <- function(id, json, height = "500px", source = NULL) {
  init_js <- sprintf('var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox"});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  chart <- sprintf('<div id="%s" style="width:100%%;height:%s;touch-action:pan-y;"></div>\n<script>(function(){%s})();</script>',
    id, height, init_js)
  if (!is.null(source)) {
    chart <- paste0(chart, sprintf('\n<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0; padding-right:2px;">%s</p>', source))
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

page_template <- function(title, body_html, has_tabs = FALSE, extra_head = "") {
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
.text-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.text-card { background:white; border-radius:8px; padding:20px; text-align:center;
  font-size:15px; line-height:1.6; border:1px solid #e0e0e0; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:visible; min-width:0; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.headline { background:white; border-radius:8px; padding:30px; text-align:center; border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:36px; font-weight:700; color:#1a4e72; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
.stat-row { display:grid; grid-template-columns:repeat(3, 1fr); gap:16px; margin-bottom:20px; }
.stat-card { background:white; border-radius:8px; padding:18px 12px; text-align:center; border:1px solid #e0e0e0; }
.stat-card .stat-num { font-size:24px; font-weight:700; color:#1a4e72; }
.stat-card .stat-label { font-size:12px; color:#666; margin-top:4px; }
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .stat-row { grid-template-columns:1fr 1fr !important; }
  .headline .number { font-size:28px; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
}
@media (max-width:600px) {
  .measure-num { font-size:22px !important; }
  .measure-label { font-size:11px !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .stat-row { grid-template-columns:1fr !important; }
  .headline .number { font-size:24px; }
  .chart-card { padding:10px; }
}', tab_css, '
</style>
', extra_head, '
</head>
<body>
', body_html, '
', if (has_tabs) tab_switch_script else '', '
', iframe_resize_script, '
</body>
</html>')
}

ABS_SOURCE <- "Source: <a href='https://www.abs.gov.au/census/find-census-data' target='_blank' style='color:#2774AE;'>Australian Bureau of Statistics</a> \u2014 Census of Population and Housing, 2021<br>Iran-born population enumerated via country of birth question."

blues <- c("#1a4e72", "#2774AE", "#5a9bd5", "#8bbdde", "#d4e6f1")

# =====================================================
# AUSTRALIA POPULATION
# =====================================================
cat("Building au-population...\n")

age_sex <- read.csv(file.path(DATA_DIR, "age_sex.csv"), stringsAsFactors = FALSE)
state_pop <- read.csv(file.path(DATA_DIR, "state_population.csv"), stringsAsFactors = FALSE)
lga_pop <- read.csv(file.path(DATA_DIR, "lga_population.csv"), stringsAsFactors = FALSE)
ancestry <- read.csv(file.path(DATA_DIR, "ancestry_by_state.csv"), stringsAsFactors = FALSE)
persian <- read.csv(file.path(DATA_DIR, "persian_speakers_by_state.csv"), stringsAsFactors = FALSE)

total_birthplace <- sum(state_pop$count)
total_ancestry <- sum(ancestry$ancestry_count)
total_persian <- sum(persian$persian_speakers)

# --- Three colored boxes for census measures ---
make_measure_box <- function(number, label, color) {
  sprintf('<div style="background:%s; border-radius:8px; padding:24px 12px; text-align:center; color:white; flex:1; min-width:0;">
  <div class="measure-num" style="font-size:32px; font-weight:700;">%s</div>
  <div class="measure-label" style="font-size:13px; opacity:0.9; margin-top:8px; line-height:1.4;">%s</div>
</div>', color, format(number, big.mark = ","), label)
}

measures_html <- paste0(
  '<div style="display:flex; flex-direction:column; gap:12px; height:100%; justify-content:center; width:100%;">',
  '<div style="font-size:15px; font-weight:600; text-align:center; color:#333;">Three Census Measures</div>',
  '<div style="display:flex; gap:12px; width:100%;">',
  make_measure_box(total_ancestry, "Iranian<br>ancestry", "#1a4e72"),
  make_measure_box(total_persian, "Persian speakers<br>(excl. Dari)", "#5a9bd5"),
  make_measure_box(total_birthplace, "Born in<br>Iran", "#2774AE"),
  '</div>',
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0;">%s</p>', ABS_SOURCE),
  '</div>'
)

# --- State choropleth map ---
# NOTE: this builder depends on the `ozmaps` package in addition to
# plotly/dplyr/jsonlite/readxl. Install with:
#   install.packages(c("sf", "ozmaps"))
# This is the only R builder that needs ozmaps.
library(sf)
library(ozmaps)
sf_use_s2(FALSE)

state_sf <- ozmap_states %>% st_transform(4326)
# Join population data
state_pop_map <- state_pop %>% filter(state != "Other Territories")
state_total <- sum(state_pop$count)
state_pop_map$pct <- round(state_pop_map$count / state_total * 100, 1)

state_sf <- state_sf %>%
  left_join(state_pop_map, by = c("NAME" = "state"))
state_sf$count[is.na(state_sf$count)] <- 0
state_sf$pct[is.na(state_sf$pct)] <- 0

# Write state GeoJSON for plotly
state_geojson_path <- file.path(DATA_DIR, "au_states.geojson")
st_write(state_sf %>% select(NAME, count, geometry),
  state_geojson_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
state_geojson <- jsonlite::fromJSON(state_geojson_path, simplifyVector = FALSE)
state_data <- state_sf %>% st_drop_geometry() %>% filter(NAME != "Other Territories")

p_state_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = state_geojson,
    locations = state_data$NAME, z = state_data$count,
    featureidkey = "properties.NAME",
    text = sprintf("<b>%s</b><br>%s Iran-born<br>%.1f%% of total",
      state_data$NAME, format(state_data$count, big.mark = ","), state_data$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.01, "#c6dbef"), c(0.1, "#6baed6"),
      c(0.5, "#2171b5"), c(1, "#08306b")),
    showscale = FALSE,
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lon = 134, lat = -28),
      zoom = 3
    ),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Sydney LGA choropleth (zoom map) ---
lga_sf <- abs_lga %>% st_transform(4326)
# Strip type suffixes from LGA names: "Brisbane (C)" -> "Brisbane"
lga_sf$clean_name <- trimws(gsub("\\s*\\([^)]+\\)\\s*$", "", lga_sf$NAME))

# Join population data
lga_sf <- lga_sf %>%
  left_join(lga_pop %>% select(lga_name, count), by = c("clean_name" = "lga_name"))
lga_sf$count[is.na(lga_sf$count)] <- 0

# Greater Sydney LGAs (wider bounding box for metro area)
sydney_lgas <- lga_sf %>%
  filter(count > 0) %>%
  st_filter(st_as_sfc(st_bbox(c(xmin = 150.3, ymin = -34.3, xmax = 151.6, ymax = -33.3),
    crs = 4326)))
sydney_lgas <- sydney_lgas %>% filter(count >= 10) %>% arrange(desc(count))

syd_geojson_path <- file.path(DATA_DIR, "au_sydney_lgas.geojson")
st_write(sydney_lgas %>% select(clean_name, count, geometry),
  syd_geojson_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
syd_geojson <- jsonlite::fromJSON(syd_geojson_path, simplifyVector = FALSE)
syd_data <- sydney_lgas %>% st_drop_geometry()
syd_data$pct <- round(syd_data$count / total_birthplace * 100, 1)

p_sydney_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = syd_geojson,
    locations = syd_data$clean_name, z = syd_data$count,
    featureidkey = "properties.clean_name",
    text = sprintf("<b>%s</b><br>%s Iran-born<br>%.1f%% of total",
      syd_data$clean_name, format(syd_data$count, big.mark = ","), syd_data$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#c6dbef"), c(0.05, "#9ecae1"), c(0.15, "#6baed6"),
      c(0.4, "#2171b5"), c(1, "#08306b")),
    showscale = FALSE,
    marker = list(line = list(width = 1, color = "#999"), opacity = 0.85)
  ) %>% layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lon = 151.0, lat = -33.85),
      zoom = 7
    ),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Melbourne LGA choropleth (zoom map) ---
melb_lgas <- lga_sf %>%
  filter(count > 0) %>%
  st_filter(st_as_sfc(st_bbox(c(xmin = 144.3, ymin = -38.3, xmax = 146.0, ymax = -37.3),
    crs = 4326)))
melb_lgas <- melb_lgas %>% filter(count >= 10) %>% arrange(desc(count))

melb_geojson_path <- file.path(DATA_DIR, "au_melbourne_lgas.geojson")
st_write(melb_lgas %>% select(clean_name, count, geometry),
  melb_geojson_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
melb_geojson <- jsonlite::fromJSON(melb_geojson_path, simplifyVector = FALSE)
melb_data <- melb_lgas %>% st_drop_geometry()
melb_data$pct <- round(melb_data$count / total_birthplace * 100, 1)

p_melbourne_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = melb_geojson,
    locations = melb_data$clean_name, z = melb_data$count,
    featureidkey = "properties.clean_name",
    text = sprintf("<b>%s</b><br>%s Iran-born<br>%.1f%% of total",
      melb_data$clean_name, format(melb_data$count, big.mark = ","), melb_data$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#c6dbef"), c(0.05, "#9ecae1"), c(0.15, "#6baed6"),
      c(0.4, "#2171b5"), c(1, "#08306b")),
    showscale = FALSE,
    marker = list(line = list(width = 1, color = "#999"), opacity = 0.85)
  ) %>% layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lon = 145.0, lat = -37.85),
      zoom = 7
    ),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Assemble population page ---
pop_body <- paste0(
  # Top row: headline + measures chart
  '<div class="chart-row" style="grid-template-columns:45% 55%;">',
  '<div class="headline">',
  '<div class="label">Iranian-Origin Population in Australia</div>',
  '<div class="number">81,111</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
  '<a href="https://www.abs.gov.au/census" style="color:#2774AE;" target="_blank">',
  'Australian Bureau of Statistics</a>, Census of Population and Housing, 2021</div>',
  '<div style="margin:14px auto 0; max-width:460px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">The census provides three separate measures of the Iranian-origin population:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> <span style="color:#888;">&mdash; &ldquo;In which country was the person born?&rdquo;</span></li>',
  '<li><strong>Ancestry</strong> <span style="color:#888;">&mdash; &ldquo;What is the person&rsquo;s ancestry?&rdquo; (up to two responses)</span></li>',
  '<li><strong>Language</strong> <span style="color:#888;">&mdash; &ldquo;Does the person speak a language other than English at home?&rdquo;</span></li>',
  '</ul>',
  '<p style="margin-top:10px;">The headline uses the ancestry count (81,111), the broadest single measure. ',
  'These three counts overlap substantially and cannot yet be combined into a single unduplicated estimate. ',
  'A compound figure will be added once cross-tabulated microdata become available.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; align-items:center;">', measures_html, '</div>',
  '</div>',

  # Bottom row: map with tabs
  '<div class="chart-card">',
  '<div class="section-title">Geographic Distribution of Iran-Born Australians</div>',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'au-state-tab\',this,\'au-geo\')">By State</button>',
  '<button class="tab-btn" onclick="switchTab(\'au-syd-tab\',this,\'au-geo\')">Greater Sydney</button>',
  '<button class="tab-btn" onclick="switchTab(\'au-melb-tab\',this,\'au-geo\')">Greater Melbourne</button>',
  '</div>',
  '<div id="au-state-tab" class="tab-panel active" data-group="au-geo">',
  plotly_div("au-state-map", plotly_to_json(p_state_map), "420px", source = ABS_SOURCE),
  '<script>if(window.innerWidth<900){setTimeout(function(){var el=document.getElementById("au-state-map");if(el&&window.Plotly)Plotly.relayout(el,{"mapbox.zoom":2.2,"mapbox.center.lat":-26});},500);}</script>',
  '</div>',
  '<div id="au-syd-tab" class="tab-panel" data-group="au-geo">',
  plotly_div("au-syd-map", plotly_to_json(p_sydney_map), "420px",
    source = paste0(ABS_SOURCE, "<br>Local Government Areas in Greater Sydney with 10+ Iran-born residents.")),
  '</div>',
  '<div id="au-melb-tab" class="tab-panel" data-group="au-geo">',
  plotly_div("au-melb-map", plotly_to_json(p_melbourne_map), "420px",
    source = paste0(ABS_SOURCE, "<br>Local Government Areas in Greater Melbourne with 10+ Iran-born residents.")),
  '<script>if(window.innerWidth<900){setTimeout(function(){var el=document.getElementById("au-melb-map");if(el&&window.Plotly)Plotly.relayout(el,{"mapbox.zoom":6.5});},500);}</script>',
  '</div>',
  '</div>'
)

writeLines(page_template("Australia: Population", pop_body, has_tabs = TRUE), "docs/pages/au-population.html")
cat("  Done\n")


# =====================================================
# AUSTRALIA IMMIGRATION
# =====================================================
cat("Building au-immigration...\n")

arrival <- read.csv(file.path(DATA_DIR, "year_of_arrival.csv"), stringsAsFactors = FALSE)
citizenship <- read.csv(file.path(DATA_DIR, "citizenship.csv"), stringsAsFactors = FALSE)

# --- Year of arrival chart (Canada-style: wide bars for periods, narrow for annual) ---
# Convert period data to numeric x-axis with bar widths
arr <- arrival %>% mutate(
  year = case_when(
    period == "Before 1951" ~ 1945,
    period == "1951 - 1960" ~ 1955,
    period == "1961 - 1970" ~ 1965,
    period == "1971 - 1980" ~ 1975,
    period == "1981 - 1990" ~ 1985,
    period == "1991 - 2000" ~ 1995,
    period == "2001 - 2010" ~ 2005,
    period == "2011 - 2015" ~ 2013,
    grepl("^Arrived", period) ~ as.numeric(gsub("Arrived ", "", period)),
    TRUE ~ NA_real_
  ),
  is_period = !grepl("^Arrived", period),
  # Period bars: show per-year average; annual bars: show actual count
  period_years = case_when(
    period == "Before 1951" ~ 10, period == "2011 - 2015" ~ 5,
    is_period ~ 10, TRUE ~ 1
  ),
  annual_avg = round(count / period_years),
  bar_width = case_when(
    period == "2011 - 2015" ~ 4.5,
    is_period ~ 9.5,
    TRUE ~ 0.8
  ),
  bar_color = ifelse(is_period, "#5a9bd5", "#2774AE")
) %>% filter(!is.na(year))

arr <- arr %>% arrange(year)
# Bar heights stay at annual_avg so period bars don't swamp post-2015
# single-year bars visually, but cumulative share is computed from the
# actual arrival counts (not the averaged series) so the hover value is
# meaningful: "cumulative share of all Iran-born arrivals so far."
arr$cum_true <- cumsum(arr$count)
total_true <- sum(arr$count)
arr$cum_pct <- round(arr$cum_true / total_true * 100, 1)

arr$hover <- ifelse(arr$is_period,
  sprintf("<b>%s</b><br>Total arrivals: %s (%s/year avg)<br>Cumulative: %.1f%%",
    arr$period, trimws(format(arr$count, big.mark = ",")),
    trimws(format(arr$annual_avg, big.mark = ",")), arr$cum_pct),
  sprintf("<b>%d</b><br>Arrivals: %s<br>Cumulative: %.1f%%",
    arr$year, trimws(format(arr$count, big.mark = ",")), arr$cum_pct))

p_arrival <- plot_ly() %>%
  add_bars(data = arr, x = ~year, y = ~annual_avg,
    width = ~bar_width, marker = list(color = ~bar_color),
    text = ~hover, hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iranian Migration to Australia:<br>Arrivals by Period</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 11), dtick = 10,
      range = c(1938, 2023)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 65, b = 50),
    showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white",
    annotations = list(
      list(text = "Pre-2016 values show period arrivals averaged per year. 2016\u20132021 show annual counts.",
        x = 0.5, y = -0.12, xref = "paper", yref = "paper", showarrow = FALSE,
        font = list(size = 9, color = "#888"), xanchor = "center"))
  ) %>% config(displayModeBar = FALSE)

# --- Citizenship chart (moved from population page) ---
cit_aus <- citizenship$count[citizenship$category == "Australian citizen"]
cit_not <- citizenship$count[citizenship$category == "Not Australian citizen"]
cit_ns <- citizenship$count[citizenship$category == "Not stated"]
cit_total <- cit_aus + cit_not
cit_pct <- round(cit_aus / cit_total * 100, 1)

cit_data <- data.frame(
  status = c("Australian\ncitizen", "Not an Australian\ncitizen"),
  count = c(cit_aus, cit_not),
  pct = c(cit_pct, round(100 - cit_pct, 1)),
  stringsAsFactors = FALSE
)

p_cit <- plot_ly(data = cit_data, x = ~status, y = ~count, type = "bar",
    marker = list(color = c("#2774AE", "#d4816b")),
    text = ~sprintf("<b>%s</b><br>%s (%.1f%%)",
      gsub("\n", " ", status), format(count, big.mark = ","), pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Citizenship Status<br>of Iran-Born Australians</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = ""),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 55, b = 60),
    showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Key period stats ---
pre_rev <- sum(arrival$count[arrival$period %in% c("Before 1951", "1951 - 1960",
  "1961 - 1970", "1971 - 1980")])
post_rev_80s <- arrival$count[arrival$period == "1981 - 1990"]
peak_period <- arrival$count[arrival$period == "2011 - 2015"]

# --- Assemble immigration page ---
immig_body <- paste0(
  '<div class="page-content">',
  '<div class="text-card pt1">Iranian migration to Australia was minimal before 1980, ',
  'with only ', format(pre_rev, big.mark = ","), ' Iran-born residents arriving before that year. ',
  'The 1980s saw a sharp increase to ', format(post_rev_80s, big.mark = ","),
  ' arrivals, and numbers continued rising through each subsequent period.</div>',
  '<div class="text-card pt2">The peak period was 2011\u20132015, when ',
  format(peak_period, big.mark = ","), ' Iran-born individuals arrived\u2014more than a third of ',
  'the current Iran-born population. Annual arrivals ranged between 2,800 and 3,500 from 2016 to 2019, ',
  'then fell to 1,283 in 2020 and 727 in 2021.</div>',
  '<div class="chart-card pc1">', plotly_div("au-arrival", plotly_to_json(p_arrival), "450px", source = ABS_SOURCE), '</div>',
  '<div class="chart-card pc2">', plotly_div("au-cit", plotly_to_json(p_cit), "450px", source = ABS_SOURCE), '</div>',
  '</div>'
)

writeLines(page_template("Australia: Immigration", immig_body), "docs/pages/au-immigration.html")
cat("  Done\n")

cat("All Australia pages built.\n")

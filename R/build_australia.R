# Build Australia pages from pre-exported CSVs
# Run from deployment repo root:
#   Rscript R/build_australia.R
#
# Produces: docs/pages/au-population.html, docs/pages/au-immigration.html,
#           docs/pages/au-education.html, docs/pages/au-workinc.html

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

plotly_div <- function(id, json, height = "500px", source = NULL, legend_html = NULL) {
  init_js <- sprintf('var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox",showTips:true});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  chart <- sprintf('<div id="%s" style="width:100%%;height:%s;touch-action:manipulation;"></div>\n<script>(function(){%s})();</script>',
    id, height, init_js)
  if (!is.null(legend_html)) chart <- paste0(chart, '\n', legend_html)
  if (!is.null(source)) {
    chart <- paste0(chart, sprintf('\n<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0; padding-right:2px;">%s</p>', source))
  }
  chart
}

make_html_legend <- function(colors, labels = names(colors), break_after = NULL) {
  items <- mapply(function(col, lab) {
    sprintf('<span style="display:inline-flex;align-items:center;gap:4px;margin:0 8px;"><span style="display:inline-block;width:14px;height:14px;background:%s;border-radius:2px;flex-shrink:0;"></span><span style="font-size:12px;color:#333;">%s</span></span>', col, lab)
  }, colors, labels, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if (!is.null(break_after) && break_after < length(items)) {
    row1 <- paste(items[1:break_after], collapse = "")
    row2 <- paste(items[(break_after + 1):length(items)], collapse = "")
    inner <- paste0('<div style="display:flex;flex-wrap:wrap;justify-content:center;gap:2px;">', row1, '</div>',
                    '<div style="display:flex;flex-wrap:wrap;justify-content:center;gap:2px;">', row2, '</div>')
  } else {
    inner <- paste(items, collapse = "")
  }
  sprintf('<div style="text-align:center;margin:6px 0 2px;">%s</div>', inner)
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
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em; }
a { transition: color 0.15s; }
a:hover { color: #1a4e72 !important; text-decoration: underline; }
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
  .headline .number { font-size:34px; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
}
@media (max-width:600px) {
  .measure-num { font-size:22px !important; }
  .measure-label { font-size:11px !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .stat-row { grid-template-columns:1fr !important; }
  .headline .number { font-size:28px; }
  .chart-card { padding:10px; }
  .tab-btn { font-size:11px; padding:4px 8px; }
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
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
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
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
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
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
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

# --- Waterfall chart (compound definition) ---
wf <- read.csv(file.path(DATA_DIR, "au_compound.csv"), stringsAsFactors = FALSE)
wf$ymin <- wf$cumulative - wf$count
wf$ymax <- wf$cumulative
wf$short_label <- c("Birth +\nAncestry +\nLanguage", "Birth +\nAncestry", "Birth +\nLanguage",
                     "Ancestry +\nLanguage", "Birth\nonly", "Ancestry\nonly", "Language\nonly",
                     "Children of\nIran-born\nparents")

# Same color scheme as US waterfall: blue gradient for core, then distinct
wf_colors <- c("#1a4e72", "#2774AE", "#5a9bd5", "#4a8c6f", "#c4793a", "#d4a943", "#7b5ea7", "#e07b54")

p_waterfall <- plot_ly() %>%
  add_bars(data = wf, x = ~short_label, y = ~count, base = ~ymin,
    marker = list(color = wf_colors),
    text = sprintf("<b>%s</b><br>%s (%.1f%%)<br>Cumulative: %s",
      wf$component, format(wf$count, big.mark = ","), wf$pct,
      format(wf$cumulative, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Australians: How We Count</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 9), tickangle = 0,
      categoryorder = "array", categoryarray = wf$short_label),
    yaxis = list(title = "", tickformat = ","),
    showlegend = FALSE,
    margin = list(t = 50, b = 110, l = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

compound_total <- wf$cumulative[nrow(wf)]

# --- Assemble population page ---
pop_body <- paste0(
  # Top row: headline + waterfall
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Australian Population</div>',
  '<div class="number">', format(compound_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
  '<a href="https://www.abs.gov.au/census" style="color:#2774AE;" target="_blank">',
  'Australian Bureau of Statistics</a>, Census of Population and Housing, 2021</div>',
  '<div style="margin:14px auto 0; max-width:460px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">A person is counted if they meet <em>at least one</em> of four criteria:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> <span style="color:#888;">&mdash; born in Iran</span></li>',
  '<li><strong>Ancestry</strong> <span style="color:#888;">&mdash; reports Iranian ancestry</span></li>',
  '<li><strong>Language at home</strong> <span style="color:#888;">&mdash; speaks Persian (excluding Dari)</span></li>',
  '<li><strong>Parental birthplace</strong> <span style="color:#888;">&mdash; Australian-born with at least one Iran-born parent</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">The compound total counts each person once, even if they meet more than one criterion.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card">',
  plotly_div("au-waterfall", plotly_to_json(p_waterfall), "430px",
    source = "Source: <a href='https://www.abs.gov.au/census' target='_blank' style='color:#2774AE;'>ABS</a> \u2014 Census of Population and Housing, 2021. Cell counts are randomly adjusted by ABS to prevent identification of individuals; totals may not sum exactly."),
  '</div>',
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
# Merge pre-1961 periods into "Before 1971" since counts are small
arrival <- arrival %>%
  mutate(period = ifelse(period %in% c("Before 1951", "1951 - 1960", "1961 - 1970"),
    "Before 1971", period)) %>%
  group_by(period) %>%
  summarize(count = sum(count), .groups = "drop")

# Convert period data to numeric x-axis with bar widths
arr <- arrival %>% mutate(
  year = case_when(
    period == "Before 1971" ~ 1965,
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
    period == "Before 1971" ~ 20, period == "2011 - 2015" ~ 5,
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
  add_trace(data = arr, x = ~year, y = ~cum_pct, type = "scatter",
    mode = "lines", yaxis = "y2",
    line = list(color = "lightblue", width = 2),
    hoverinfo = "skip", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iranian Migration to Australia:<br>Arrivals by Period</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 11), dtick = 10,
      range = c(1958, 2023)),
    yaxis = list(title = "", tickformat = ","),
    yaxis2 = list(title = "", overlaying = "y", side = "right",
      ticksuffix = "%", range = c(0, 105), showgrid = FALSE,
      tickfont = list(size = 10, color = "#888")),
    margin = list(t = 65, b = 50, r = 40),
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
    marker = list(color = c("#2774AE", "#e07b54")),
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

peak_period_pct <- round(peak_period / total_birthplace * 100)

# --- Assemble immigration page ---
immig_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iran-born Australians arrived during 2011&ndash;2015.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Arrivals have continued at about 2,500&ndash;3,500 per year since 2016.</div>
  </div>', peak_period_pct),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iran-born Australians hold Australian citizenship.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Eligible residents can apply for citizenship after four years in Australia.</div>
  </div>', cit_pct),
  '<div class="chart-card pc1">', plotly_div("au-arrival", plotly_to_json(p_arrival), "430px", source = ABS_SOURCE), '</div>',
  '<div class="chart-card pc2">', plotly_div("au-cit", plotly_to_json(p_cit), "450px", source = ABS_SOURCE), '</div>',
  '</div>'
)

writeLines(page_template("Australia: Immigration", immig_body), "docs/pages/au-immigration.html")
cat("  Done\n")


# =====================================================
# AUSTRALIA EDUCATION & RELIGION
# =====================================================
cat("Building au-education...\n")

edu <- read.csv(file.path(DATA_DIR, "au_education.csv"), stringsAsFactors = FALSE)
rel <- read.csv(file.path(DATA_DIR, "au_religion.csv"), stringsAsFactors = FALSE)
second_gen <- read.csv(file.path(DATA_DIR, "au_second_gen.csv"), stringsAsFactors = FALSE)

# --- Education horizontal bar ---
# Exclude supplementary/not stated/not applicable
edu_chart <- edu %>%
  filter(!education_level %in% c("Supplementary Codes", "Not stated", "Not applicable")) %>%
  arrange(desc(count))

edu_total <- sum(edu_chart$count)
edu_chart$pct <- round(edu_chart$count / edu_total * 100, 1)

# Shorten long labels for y-axis
edu_chart$label <- edu_chart$education_level
edu_chart$label <- gsub("Secondary Education - ", "Secondary: ", edu_chart$label)
edu_chart$label <- gsub(" Level$", "", edu_chart$label)
edu_chart$label <- gsub("Graduate Diploma and Graduate Certificate", "Grad. Diploma & Certificate", edu_chart$label)
edu_chart$label <- gsub("Advanced Diploma and Diploma", "Diploma & Adv. Diploma", edu_chart$label)
edu_chart$label <- factor(edu_chart$label, levels = rev(edu_chart$label))

bachelors_plus <- sum(edu_chart$count[edu_chart$education_level %in%
  c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
    "Bachelor Degree Level")])
bachelors_pct <- round(bachelors_plus / edu_total * 100, 1)
postgrad_count <- edu_chart$count[edu_chart$education_level == "Postgraduate Degree Level"][1]
postgrad_pct   <- round(postgrad_count / edu_total * 100)

# Ordinal blue gradient: darkest for highest qualification
edu_blues <- colorRampPalette(c("#08306b", "#c6dbef"))(nrow(edu_chart))
p_edu <- plot_ly(edu_chart, y = ~label, x = ~count, type = "bar",
    orientation = "h", marker = list(color = edu_blues),
    text = sprintf("<b>%s</b><br>%s (%.1f%%)",
      edu_chart$education_level,
      format(edu_chart$count, big.mark = ","), edu_chart$pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(
      text = "<b>Highest Educational Attainment<br>of Iran-Born Australians, 2021</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 11)),
    margin = list(l = 200, r = 20, t = 55, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Religion by generation (100% stacked horizontal bar) ---
rel_gen <- read.csv(file.path(DATA_DIR, "au_religion_by_gen.csv"), stringsAsFactors = FALSE)

# Keep top 4 categories (Islam, Secular, Christianity, Other Religions)
rel_cats <- c("Secular/No religion", "Islam", "Other Religions", "Christianity")
rel_gen <- rel_gen %>% filter(religion %in% rel_cats)
rel_gen$gen <- factor(rel_gen$gen, levels = c("1st Generation", "2nd Generation"))

rel_colors <- c(
  "Secular/No religion" = "#d4a943",
  "Islam"               = "#1a4e72",
  "Christianity"        = "#4a8c6f",
  "Other Religions"     = "#7b5ea7"
)

p_rel <- plot_ly()
for (cat in rel_cats) {
  sub <- rel_gen %>% filter(religion == cat)
  p_rel <- p_rel %>% add_bars(data = sub, y = ~gen, x = ~pct, name = cat,
    orientation = "h", marker = list(color = rel_colors[cat]),
    hovertext = sprintf("<b>%s</b><br>%s<br>%.1f%%", cat, sub$gen, sub$pct),
    hoverinfo = "text", textposition = "none",
    legendgroup = cat, showlegend = FALSE)
}
p_rel <- p_rel %>% layout(
  barmode = "stack",
  title = list(text = "<b>Religion of Iranian-Australians<br>by Generation, 2021</b>",
    font = list(size = 15, family = "Montserrat")),
  xaxis = list(title = "", ticksuffix = "%", range = c(0, 105)),
  yaxis = list(title = "", categoryorder = "array",
    categoryarray = rev(levels(rel_gen$gen)), ticklabelstandoff = 6),
  margin = list(t = 55, b = 40, l = 120), showlegend = FALSE,
  plot_bgcolor = "white", paper_bgcolor = "white"
) %>% config(displayModeBar = FALSE)

rel_leg <- make_html_legend(rel_colors)

no_relig_pct <- 46.4  # all Iranian ancestry
islam_pct <- 25.3

# --- Assemble education page ---
second_gen_count <- second_gen$count[1]

edu_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iran-born Australian adults (15+) hold a bachelor&rsquo;s degree or higher.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%d%% hold a postgraduate degree &mdash; about half of all bachelor&rsquo;s-or-higher holders.</div>
  </div>', bachelors_pct, postgrad_pct),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iranian-ancestry Australians report no religious affiliation.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Islam %s%%, Christianity 12%%.</div>
  </div>', no_relig_pct, islam_pct),
  '<div class="chart-card pc1">', plotly_div("au-edu", plotly_to_json(p_edu), "430px", source = ABS_SOURCE), '</div>',
  '<div class="chart-card pc2">', plotly_div("au-rel", plotly_to_json(p_rel), "430px",
    source = "Source: <a href='https://www.abs.gov.au/census' target='_blank' style='color:#2774AE;'>ABS</a> \u2014 Census 2021. Iranian ancestry population, 1st gen = Iran-born, 2nd gen = Australian-born.",
    legend_html = rel_leg), '</div>',
  '</div>'
)

writeLines(page_template("Australia: Education & Religion", edu_body), "docs/pages/au-education.html")
cat("  Done\n")


# =====================================================
# AUSTRALIA WORK & INCOME
# =====================================================
cat("Building au-workinc...\n")

lf <- read.csv(file.path(DATA_DIR, "au_labourforce.csv"), stringsAsFactors = FALSE)
occ <- read.csv(file.path(DATA_DIR, "au_occupation.csv"), stringsAsFactors = FALSE)
ind <- read.csv(file.path(DATA_DIR, "au_industry.csv"), stringsAsFactors = FALSE)
inc <- read.csv(file.path(DATA_DIR, "au_income_weekly.csv"), stringsAsFactors = FALSE)

# --- Labour force summary stats ---
employed_ft <- lf$count[lf$status == "Employed, worked full-time"]
employed_pt <- lf$count[lf$status == "Employed, worked part-time"]
employed_away <- lf$count[lf$status == "Employed, away from work"]
total_employed <- employed_ft + employed_pt + employed_away
unemp_ft <- lf$count[lf$status == "Unemployed, looking for full-time work"]
unemp_pt <- lf$count[lf$status == "Unemployed, looking for part-time work"]
total_unemployed <- unemp_ft + unemp_pt
nilf <- lf$count[lf$status == "Not in the labour force"]
pop_15plus <- total_birthplace - lf$count[lf$status == "Not applicable"]
labour_force <- total_employed + total_unemployed
participation_rate <- round(labour_force / pop_15plus * 100, 1)
unemployment_rate <- round(total_unemployed / labour_force * 100, 1)

# --- Occupation horizontal bar ---
occ_chart <- occ %>%
  filter(!occupation %in% c("Inadequately described", "Not stated", "Not applicable")) %>%
  arrange(desc(count))

occ_total <- sum(occ_chart$count)
occ_chart$pct <- round(occ_chart$count / occ_total * 100, 1)
# Shorten long labels for y-axis (mobile-friendly)
occ_chart$label <- occ_chart$occupation
occ_chart$label <- gsub(" Workers$", "", occ_chart$label)
occ_chart$label <- gsub("Community and Personal Service", "Community Services", occ_chart$label)
occ_chart$label <- gsub("Clerical and Administrative", "Clerical & Admin.", occ_chart$label)
occ_chart$label <- gsub("Technicians and Trades", "Technicians & Trades", occ_chart$label)
occ_chart$label <- gsub("Machinery Operators and Drivers", "Machinery Operators", occ_chart$label)
occ_chart$label <- factor(occ_chart$label, levels = rev(occ_chart$label))

# Categorical: occupations are distinct categories
occ_colors <- c("#1a4e72", "#2774AE", "#4a8c6f", "#c4793a",
                "#d4a943", "#7b5ea7", "#2ca089", "#e07b54")
p_occ <- plot_ly(occ_chart, y = ~label, x = ~count, type = "bar",
    orientation = "h", marker = list(color = occ_colors[seq_len(nrow(occ_chart))]),
    text = sprintf("<b>%s</b><br>%s (%.1f%%)",
      occ_chart$occupation,
      format(occ_chart$count, big.mark = ","), occ_chart$pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(
      text = "<b>Occupation of Iran-Born Workers<br>in Australia, 2021</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 11)),
    margin = list(l = 220, r = 20, t = 55, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Industry horizontal bar (top 15 only, excl. residual categories) ---
ind_chart <- ind %>%
  filter(!industry %in% c("Inadequately described", "Not stated", "Not applicable")) %>%
  arrange(desc(count)) %>%
  head(15)

ind_total <- sum(ind$count[!ind$industry %in% c("Inadequately described", "Not stated", "Not applicable")])
ind_chart$pct <- round(ind_chart$count / ind_total * 100, 1)
# Shorten long labels
ind_chart$label <- gsub(", Forestry and Fishing", "", ind_chart$industry)
ind_chart$label <- gsub(", Gas, Water and Waste Services", "/Gas/Water", ind_chart$label)
ind_chart$label <- gsub(", Postal and Warehousing", "/Postal", ind_chart$label)
ind_chart$label <- gsub("Information Media and Telecommunications", "Info & Telecom", ind_chart$label)
ind_chart$label <- gsub("Financial and Insurance Services", "Finance & Insurance", ind_chart$label)
ind_chart$label <- gsub(", Hiring and Real Estate Services", "/Real Estate", ind_chart$label)
ind_chart$label <- gsub("Professional, Scientific and Technical Services", "Professional & Technical", ind_chart$label)
ind_chart$label <- gsub("Administrative and Support Services", "Admin. & Support", ind_chart$label)
ind_chart$label <- gsub("Public Administration and Safety", "Public Admin. & Safety", ind_chart$label)
ind_chart$label <- gsub("Education and Training", "Education & Training", ind_chart$label)
ind_chart$label <- gsub("Health Care and Social Assistance", "Health & Social Asst.", ind_chart$label)
ind_chart$label <- gsub("Arts and Recreation Services", "Arts & Recreation", ind_chart$label)
ind_chart$label <- gsub("Accommodation and Food Services", "Accommodation & Food", ind_chart$label)
ind_chart$label <- factor(ind_chart$label, levels = rev(ind_chart$label))

# Categorical: industries are distinct categories
ind_colors <- rep(c("#1a4e72", "#2774AE", "#4a8c6f", "#c4793a", "#d4a943",
                    "#7b5ea7", "#2ca089", "#e07b54"), length.out = nrow(ind_chart))
p_ind <- plot_ly(ind_chart, y = ~label, x = ~count, type = "bar",
    orientation = "h", marker = list(color = ind_colors),
    text = sprintf("<b>%s</b><br>%s (%.1f%%)",
      ind_chart$industry,
      format(ind_chart$count, big.mark = ","), ind_chart$pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(
      text = "<b>Industry of Employment<br>of Iran-Born Workers in Australia, 2021</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 10)),
    margin = list(l = 185, r = 20, t = 55, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Income weekly bar chart ---
inc_chart <- inc %>%
  filter(!income_band_weekly %in% c("Not stated", "Not applicable"))

# Order factor by the implicit ordering in the CSV (already sorted low to high)
inc_chart$label <- gsub("\\$", "A$", inc_chart$income_band_weekly)
inc_chart$label <- gsub("Nil income", "A$0", inc_chart$label)
inc_chart$label <- gsub("Negative income", "Negative", inc_chart$label)
inc_chart$label <- factor(inc_chart$label, levels = inc_chart$label)

inc_stated <- sum(inc_chart$count)
inc_chart$pct <- round(inc_chart$count / inc_stated * 100, 1)

# Color: higher income = darker
n_bars <- nrow(inc_chart)
inc_chart$color <- colorRampPalette(c("#c6dbef", "#08306b"))(n_bars)

p_inc <- plot_ly(inc_chart, x = ~label, y = ~count, type = "bar",
    marker = list(color = ~color),
    text = sprintf("<b>%s weekly</b><br>%s (%.1f%%)",
      inc_chart$income_band_weekly,
      format(inc_chart$count, big.mark = ","), inc_chart$pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(
      text = "<b>Weekly Personal Income<br>of Iran-Born Australians, 2021</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickangle = -45, tickfont = list(size = 9)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 55, b = 90),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Personal income decile chart (Iran-born vs national, ages 25-54) ---
# Line + shaded area, matching US/CA pattern but teal to mark personal income
dec <- read.csv(file.path(DATA_DIR, "au_income_deciles.csv"), stringsAsFactors = FALSE)
decile_labels <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
dec$label <- decile_labels
dec$label <- factor(dec$label, levels = decile_labels)

p_dec <- plot_ly(data = dec, x = ~label, y = ~pct, type = "scatter", mode = "markers+lines",
    marker = list(color = "#00897b", size = 8),
    line = list(color = "#00897b", width = 1),
    text = sprintf("<b>Decile:</b> %s<br><b>Count:</b> %s<br><b>Share:</b> %.1f%%",
      dec$label, format(dec$count, big.mark = ","), dec$pct),
    hoverinfo = "text", textposition = "none") %>%
  # Shaded area
  add_trace(x = ~label, y = ~pct, type = "scatter", mode = "lines",
    fill = "tozeroy", fillcolor = "rgba(178,223,219,0.3)",
    marker = list(size = 0, opacity = 0),
    hoverinfo = "skip", showlegend = FALSE) %>%
  # 10% reference line
  add_trace(x = decile_labels, y = rep(10, 10), type = "scatter", mode = "lines",
    line = list(color = "#cc0000", width = 1.5, dash = "dot"),
    marker = list(size = 0, opacity = 0),
    hoverinfo = "skip", showlegend = FALSE) %>%
  layout(
    title = list(
      text = "<b>Position in Australian<br>Personal Income Distribution:<br>Iran-Born (Ages 25\u201354)</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "Income Decile (Lowest to Highest)", titlefont = list(size = 11),
      categoryorder = "array", categoryarray = decile_labels),
    yaxis = list(title = "", ticksuffix = "%", range = c(0, max(dec$pct) + 3)),
    showlegend = FALSE,
    margin = list(t = 75, b = 70),
    plot_bgcolor = "white", paper_bgcolor = "white",
    annotations = list(
      list(text = "10% =<br>national<br>baseline", x = decile_labels[5], y = 13,
        showarrow = FALSE, font = list(size = 8, color = "#cc0000"), xanchor = "center"))
  ) %>% config(displayModeBar = FALSE)

AU_INC_SOURCE <- "Source: <a href='https://www.abs.gov.au/census/find-census-data' target='_blank' style='color:#2774AE;'>ABS</a> \u2014 Census 2021<br>Ages 25\u201354 (prime working years).<br>Each decile holds 10% of all Australians, ranked by pre-tax personal income."

# Decile share for text card
dec_d1_pct <- dec$pct[1]
dec_d10_pct <- dec$pct[10]

# --- Labour force status bar ---
lf_chart <- lf %>%
  filter(!status %in% c("Not stated", "Not applicable"))

lf_chart$label <- c("Full-time", "Part-time", "Away from work",
  "Unemp. (FT)", "Unemp. (PT)", "Not in\nlabour force")
lf_chart$label <- factor(lf_chart$label, levels = lf_chart$label)
lf_chart$color <- c("#1a4e72", "#2774AE", "#5a9bd5", "#e07b54", "#d4a943", "#b0b0b0")
lf_chart$pct <- round(lf_chart$count / pop_15plus * 100, 1)

p_lf <- plot_ly(lf_chart, x = ~label, y = ~count, type = "bar",
    marker = list(color = ~color),
    text = sprintf("<b>%s</b><br>%s (%.1f%% of pop. 15+)",
      lf_chart$status,
      format(lf_chart$count, big.mark = ","), lf_chart$pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(
      text = "<b>Labour Force Status<br>of Iran-Born Australians, 2021</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 10)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 55, b = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Top occupations for text card ---
top_occ <- occ_chart$occupation[1]
top_occ_pct <- occ_chart$pct[1]
top_ind <- ind_chart$industry[1]
top_ind_pct <- ind_chart$pct[1]

# --- Median income approximation ---
# Find the band containing the median (50th percentile)
inc_ordered <- inc_chart
inc_ordered$cumsum <- cumsum(inc_ordered$count)
median_idx <- which(inc_ordered$cumsum >= inc_stated / 2)[1]
median_band <- inc_ordered$income_band_weekly[median_idx]

# Occupation + income distribution shares for the factoid cards
occ_full       <- read.csv(file.path(DATA_DIR, "au_occupation.csv"), stringsAsFactors = FALSE)
occ_work       <- occ_full[!occ_full$occupation %in%
  c("Not stated", "Not applicable", "Inadequately described"), ]
prof_pct       <- round(occ_work$count[occ_work$occupation == "Professionals"] /
                        sum(occ_work$count) * 100)
ind_full       <- read.csv(file.path(DATA_DIR, "au_industry.csv"), stringsAsFactors = FALSE)
ind_work       <- ind_full[!ind_full$industry %in%
  c("Not stated", "Not applicable", "Inadequately described"), ]
ind_work       <- ind_work[order(-ind_work$count), ]
top_ind        <- ind_work$industry[1]
top_ind_pct    <- round(ind_work$count[1] / sum(ind_work$count) * 100)

inc_dec <- read.csv(file.path(DATA_DIR, "au_income_deciles.csv"), stringsAsFactors = FALSE)
bottom20_pct <- round(sum(inc_dec$pct[inc_dec$decile <= 2]))
top20_pct    <- round(sum(inc_dec$pct[inc_dec$decile >= 9]))

# --- Assemble work/income page ---
workinc_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of employed Iran-born Australians work as professionals &mdash; the largest occupation group.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%d%% are employed in the largest industry, %s. Overall labour-force participation is %s%%.</div>
  </div>', prof_pct, top_ind_pct, top_ind, participation_rate),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iran-born Australians fall in the bottom two national income deciles; %d%% are in the top two.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Median weekly personal income band: %s.</div>
  </div>', bottom20_pct, top20_pct, median_band),

  # Chart cell 1: tabbed (Occupation | Industry | Labour Force Status)
  '<div class="chart-card pc1">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'au-tab-occ\',this,\'work-tabs\')">Occupation</button>',
  '<button class="tab-btn" onclick="switchTab(\'au-tab-ind\',this,\'work-tabs\')">Industry</button>',
  '<button class="tab-btn" onclick="switchTab(\'au-tab-lf\',this,\'work-tabs\')">Labour Force</button>',
  '</div>',
  '<div id="au-tab-occ" class="tab-panel active" data-group="work-tabs">',
  plotly_div("au-occ", plotly_to_json(p_occ), "430px", source = ABS_SOURCE),
  '</div>',
  '<div id="au-tab-ind" class="tab-panel" data-group="work-tabs">',
  plotly_div("au-ind", plotly_to_json(p_ind), "430px", source = ABS_SOURCE),
  '</div>',
  '<div id="au-tab-lf" class="tab-panel" data-group="work-tabs">',
  plotly_div("au-lf", plotly_to_json(p_lf), "430px", source = ABS_SOURCE),
  '</div>',
  '</div>',

  # Chart cell 2: tabbed (Income Deciles | Weekly Income)
  '<div class="chart-card pc2">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'au-tab-dec\',this,\'inc-tabs\')">Income Deciles</button>',
  '<button class="tab-btn" onclick="switchTab(\'au-tab-inc\',this,\'inc-tabs\')">Weekly Income</button>',
  '</div>',
  '<div id="au-tab-dec" class="tab-panel active" data-group="inc-tabs">',
  plotly_div("au-dec", plotly_to_json(p_dec), "430px", source = AU_INC_SOURCE),
  '</div>',
  '<div id="au-tab-inc" class="tab-panel" data-group="inc-tabs">',
  plotly_div("au-inc", plotly_to_json(p_inc), "430px", source = ABS_SOURCE),
  '</div>',
  '</div>',
  '</div>'
)

writeLines(page_template("Australia: Work & Income", workinc_body, has_tabs = TRUE),
           "docs/pages/au-workinc.html")
cat("  Done\n")

cat("All Australia pages built.\n")

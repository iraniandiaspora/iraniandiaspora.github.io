# Build Switzerland page from BFS (Swiss Federal Statistical Office) extracts.
# Run from deployment repo root:
#   Rscript R/build_switzerland.R
#
# Input:  data/switzerland/*.csv, data/switzerland/ch_cantons.geojson
# Output: docs/pages/ch-population.html
#
# Extract first via: python3 R/ch_export/extract_bfs.py

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/switzerland"

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

plotly_div <- function(id, json, height = "500px", source = NULL, legend_html = NULL) {
  init_js <- sprintf(
    'var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox"});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  chart <- sprintf(
    '<div id="%s" style="width:100%%;height:%s;touch-action:pan-y;"></div>\n<script>(function(){%s})();</script>',
    id, height, init_js)
  if (!is.null(legend_html)) {
    chart <- paste0(chart, '\n', legend_html)
  }
  if (!is.null(source)) {
    chart <- paste0(chart, sprintf(
      '\n<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0; padding-right:2px;">%s</p>',
      source))
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
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; }
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
BFS_LINK <- "<a href='https://www.bfs.admin.ch/bfs/en/home.html' target='_blank' style='color:#2774AE;'>BFS</a>"
BFS_SOURCE <- paste0("Source: ", BFS_LINK, " &mdash; Population Register, 2024")
BFS_IMM_SOURCE <- paste0("Source: ", BFS_LINK, " &mdash; Immigration of permanent resident population, 2011\u20132024")

# --- Load data ---------------------------------------------------------------
cat("Loading Switzerland BFS extracts...\n")
hl       <- read.csv(file.path(DATA_DIR, "ch_headline.csv"), stringsAsFactors = FALSE)
trend    <- read.csv(file.path(DATA_DIR, "ch_trend.csv"), stringsAsFactors = FALSE)
canton   <- read.csv(file.path(DATA_DIR, "ch_canton.csv"), stringsAsFactors = FALSE)
arrivals <- read.csv(file.path(DATA_DIR, "ch_arrivals.csv"), stringsAsFactors = FALSE)

ch_total   <- hl$count[hl$category == "total"]
ch_male    <- hl$count[hl$category == "male"]
ch_female  <- hl$count[hl$category == "female"]
ch_swiss   <- hl$count[hl$category == "swiss_citizen"]
ch_foreign <- hl$count[hl$category == "foreign"]
data_yr    <- hl$year[1]


# =============================================================================
# CH-POPULATION
# =============================================================================
cat("Building ch-population...\n")

# --- Population trend 2010-2024 (line + markers) ------------------------------
p_trend <- plot_ly(trend, x = ~year, y = ~total, type = "scatter",
    mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 5),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      trend$year, format(trend$total, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iran-Born Population in Switzerland,<br>2010\u20132024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Annual arrivals 2011-2024 (bars + cumulative % line) ---------------------
arrivals <- arrivals[order(arrivals$year), ]
total_arr <- sum(arrivals$count)
arrivals$cumulative <- cumsum(arrivals$count)
arrivals$cum_pct <- round(arrivals$cumulative / total_arr * 100, 1)
max_bar <- max(arrivals$count)

p_arrivals <- plot_ly() %>%
  add_bars(data = arrivals, x = ~year, y = ~count,
    marker = list(color = "#2774AE",
      line = list(color = "#1a4e72", width = 0.3)),
    text = sprintf("<b>%d</b><br>%s arrivals<br>Cumulative: %.0f%%",
      arrivals$year, format(arrivals$count, big.mark = ","),
      arrivals$cum_pct),
    hoverinfo = "text", textposition = "none", showlegend = FALSE, name = "Arrivals") %>%
  add_trace(data = arrivals, x = ~year,
    y = ~cum_pct / 100 * max_bar,
    type = "scatter", mode = "lines",
    yaxis = "y2",
    line = list(color = "lightblue", width = 2),
    text = sprintf("<b>%d:</b> %.0f%% cumulative",
      arrivals$year, arrivals$cum_pct),
    hoverinfo = "text", showlegend = FALSE, name = "Cumulative",
    inherit = FALSE) %>%
  layout(
    title = list(
      text = "<b>Iran-Born Annual Immigration<br>to Switzerland, 2011\u20132024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2),
    yaxis = list(title = "", tickformat = ","),
    yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
      range = c(0, max_bar * 1.05),
      tickvals = seq(0, max_bar, length.out = 5),
      ticktext = c("0%", "25%", "50%", "75%", "100%"),
      tickfont = list(size = 10)),
    showlegend = FALSE,
    margin = list(t = 40, b = 30, r = 40),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Canton choropleth map ---------------------------------------------------
ch_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "ch_cantons.geojson"),
                                 simplifyVector = FALSE)
canton$pct <- round(canton$iran_born / ch_total * 100, 1)

p_canton_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = ch_geojson,
    locations = canton$canton_name, z = canton$iran_born,
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>%s Iran-born (%.1f%%)",
      canton$canton_name,
      format(canton$iran_born, big.mark = ","), canton$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                      c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.6, thickness = 12),
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = 8.2, lat = 46.8), zoom = 6.3),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Canton horizontal bar chart (descending) --------------------------------
canton_sorted <- canton[order(canton$iran_born), ]
canton_sorted$canton_name <- factor(canton_sorted$canton_name,
  levels = canton_sorted$canton_name)

p_canton_bar <- plot_ly(canton_sorted, y = ~canton_name, x = ~iran_born,
    type = "bar", orientation = "h",
    marker = list(color = "#2774AE"),
    text = sprintf("<b>%s</b><br>%s Iran-born (%.1f%%)",
      canton_sorted$canton_name,
      format(canton_sorted$iran_born, big.mark = ","),
      round(canton_sorted$iran_born / ch_total * 100, 1)),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iran-Born Population<br>by Canton, 2024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 9)),
    margin = list(l = 160, r = 20, t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Citizenship summary text ------------------------------------------------
swiss_pct <- round(ch_swiss / ch_total * 100)

# --- Assemble ch-population page ---------------------------------------------
zurich_pct <- round(canton$iran_born[canton$canton_name == "Zürich"] / ch_total * 100)

pop_body <- paste0(
  # Top row: headline + canton choropleth
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in Switzerland</div>',
  '<div class="number">', format(ch_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  BFS_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Switzerland uses population registers. A person is counted as Iran-born based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> &mdash; recorded in the population register</li>',
  '</ul>',
  sprintf('<p style="margin-top:10px; font-size:12px; color:#555; line-height:1.5;">Of the %s Iran-born residents, %s (%d%%) hold Swiss citizenship, %s hold Iranian citizenship, and %s hold other nationalities.</p>',
    format(ch_total, big.mark = ","),
    format(ch_swiss, big.mark = ","), swiss_pct,
    format(hl$count[hl$category == "iranian_citizen"], big.mark = ","),
    format(ch_total - ch_swiss - hl$count[hl$category == "iranian_citizen"], big.mark = ",")),
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">The register tracks country of birth but not parental birthplace. Swiss-born children of Iran-born parents are not counted.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card">',
  plotly_div("ch-trend", plotly_to_json(p_trend), "430px", source = BFS_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: arrivals (left) + map (right)
  '<div class="chart-row">',
  '<div class="chart-card">',
  plotly_div("ch-arrivals", plotly_to_json(p_arrivals), "430px", source = BFS_IMM_SOURCE),
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in Switzerland</div>',
  plotly_div("ch-map", plotly_to_json(p_canton_map), "430px", source = BFS_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Switzerland: Population", pop_body, has_tabs = TRUE),
           "docs/pages/ch-population.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nSwitzerland: %s Iran-born (M: %s, F: %s)\n",
  format(ch_total, big.mark = ","),
  format(ch_male, big.mark = ","),
  format(ch_female, big.mark = ",")))
cat(sprintf("Swiss citizens: %s (%d%%), Foreign: %s\n",
  format(ch_swiss, big.mark = ","), swiss_pct,
  format(ch_foreign, big.mark = ",")))
cat(sprintf("Cantons: %d, Zurich share: %d%%\n", nrow(canton), zurich_pct))
cat(sprintf("Trend: %s (%d) -> %s (%d)\n",
  format(trend$total[1], big.mark = ","), trend$year[1],
  format(trend$total[nrow(trend)], big.mark = ","), trend$year[nrow(trend)]))
cat(sprintf("Total arrivals 2011-2024: %s\n", format(total_arr, big.mark = ",")))

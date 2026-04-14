# Build Norway pages from SSB (Statistics Norway) extracts.
# Run from deployment repo root:
#   Rscript R/build_norway.R
#
# Input:  data/norway/*.csv
# Output: docs/pages/no-population.html
#
# Extract first via: Rscript R/no_export/extract_ssb.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/norway"

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
SSB_LINK <- "<a href='https://www.ssb.no/en/' target='_blank' style='color:#2774AE;'>Statistics Norway (SSB)</a>"
SSB_SOURCE <- paste0("Source: ", SSB_LINK, " &mdash; Population Register, 2026")
SSB_EMP_SOURCE <- paste0("Source: ", SSB_LINK, " &mdash; Employed immigrants, 4th quarter, 2001\u20132025")

# --- Load data ---------------------------------------------------------------
cat("Loading Norway SSB extracts...\n")
hl       <- read.csv(file.path(DATA_DIR, "no_headline.csv"), stringsAsFactors = FALSE)
trend    <- read.csv(file.path(DATA_DIR, "no_trend.csv"), stringsAsFactors = FALSE)
hist_sex <- read.csv(file.path(DATA_DIR, "no_hist_sex.csv"), stringsAsFactors = FALSE)
county   <- read.csv(file.path(DATA_DIR, "no_county.csv"), stringsAsFactors = FALSE)
employ   <- read.csv(file.path(DATA_DIR, "no_employment.csv"), stringsAsFactors = FALSE)

no_total <- hl$count[hl$category == "total"]
no_gen1  <- hl$count[hl$category == "gen1"]
no_gen2  <- hl$count[hl$category == "gen2"]
data_yr  <- hl$year[1]

# Clean county names: strip Sami variants after " - "
county$county_name <- sub(" - .*", "", county$county_name)


# =============================================================================
# NO-POPULATION
# =============================================================================
cat("Building no-population...\n")

# --- Historical sex time series 1970-2026 (line chart) -----------------------
p_hist <- plot_ly() %>%
  add_trace(data = hist_sex, x = ~year, y = ~male, type = "scatter",
    mode = "lines+markers", name = "Male",
    line = list(color = "#2171b5", width = 2),
    marker = list(color = "#2171b5", size = 4),
    text = sprintf("<b>%d</b><br>Male: %s<br>Female: %s<br>Total: %s",
      hist_sex$year, format(hist_sex$male, big.mark = ","),
      format(hist_sex$female, big.mark = ","),
      format(hist_sex$total, big.mark = ",")),
    hoverinfo = "text") %>%
  add_trace(data = hist_sex, x = ~year, y = ~female, type = "scatter",
    mode = "lines+markers", name = "Female",
    line = list(color = "#c4793a", width = 2),
    marker = list(color = "#c4793a", size = 4),
    text = sprintf("<b>%d</b><br>Female: %s",
      hist_sex$year, format(hist_sex$female, big.mark = ",")),
    hoverinfo = "text") %>%
  layout(
    title = list(text = "<b>Iran-Born Population in Norway,<br>1970\u20132026</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ","),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

no_pop_leg <- make_html_legend(c("Male" = "#2171b5", "Female" = "#c4793a"))

# --- Employment rate trend (2001-2025) ----------------------------------------
p_employ <- plot_ly() %>%
  add_trace(data = employ, x = ~year, y = ~pct_total, type = "scatter",
    mode = "lines+markers", name = "Total",
    line = list(color = "#1a4e72", width = 3),
    marker = list(color = "#1a4e72", size = 7),
    text = sprintf("<b>%d</b><br>Employment rate: %.1f%%<br>%s employed (20\u201366)",
      employ$year, employ$pct_total,
      format(employ$employed_total, big.mark = ",")),
    hoverinfo = "text") %>%
  add_trace(data = employ, x = ~year, y = ~pct_male, type = "scatter",
    mode = "lines", name = "Male",
    line = list(color = "#2171b5", width = 1.5, dash = "dash"),
    text = sprintf("<b>%d</b><br>Male: %.1f%%", employ$year, employ$pct_male),
    hoverinfo = "text") %>%
  add_trace(data = employ, x = ~year, y = ~pct_female, type = "scatter",
    mode = "lines", name = "Female",
    line = list(color = "#c4793a", width = 1.5, dash = "dash"),
    text = sprintf("<b>%d</b><br>Female: %.1f%%", employ$year, employ$pct_female),
    hoverinfo = "text") %>%
  layout(
    title = list(text = "<b>Employment Rate of Iranian Immigrants<br>(ages 20\u201366), 2001\u20132025</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2),
    yaxis = list(title = "", ticksuffix = "%", range = c(40, 75)),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

no_emp_leg <- make_html_legend(c("Total" = "#1a4e72", "Male (dashed)" = "#2171b5",
                                 "Female (dashed)" = "#c4793a"))

# --- County choropleth map ---------------------------------------------------
no_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "no_counties.geojson"),
                                 simplifyVector = FALSE)
county$join_name <- gsub(" - .*$", "", county$county_name)
county$pct <- round(county$count / no_total * 100, 1)

p_county <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = no_geojson,
    locations = county$join_name, z = county$count,
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>%s Iranian-origin (%.1f%%)",
      county$county_name,
      format(county$count, big.mark = ","), county$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                      c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = FALSE,
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = 15, lat = 64), zoom = 3.2),
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
  '<div style="display:flex; gap:12px; margin-top:12px;">',
  make_gen_box(no_gen1, paste0(round(no_gen1 / no_total * 100), "% of total"),
    "Immigrants", "Born in Iran", "#1a4e72"),
  make_gen_box(no_gen2, paste0(round(no_gen2 / no_total * 100), "% of total"),
    "Norwegian-born", "Born in Norway to immigrant parent(s)", "#5a9bd5"),
  '</div>')

# --- Latest employment stats ---
latest_emp <- employ[nrow(employ), ]

# --- Assemble no-population page ---------------------------------------------
oslo_pct <- round(county$count[county$county_name == "Oslo"] / no_total * 100)

pop_body <- paste0(
  # Top row: headline + generation grid
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Origin Population in Norway</div>',
  '<div class="number">', format(no_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  SSB_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Norway uses population registers. A person is classified as Iranian-origin if they are:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Immigrant</strong>: born in Iran with two foreign-born parents</li>',
  '<li><strong>Norwegian-born to immigrant parents</strong>: born in Norway with two Iran-born parents</li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Children with one Norwegian-born parent are not counted as immigrants or Norwegian-born to immigrant parents in SSB statistics.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  gen_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', SSB_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: tabbed (history | employment) + county bar
  '<div class="chart-row">',
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'no-tab-hist\',this,\'pop-tabs\')">Population Since 1970</button>',
  '<button class="tab-btn" onclick="switchTab(\'no-tab-emp\',this,\'pop-tabs\')">Employment Rate</button>',
  '</div>',
  '<div id="no-tab-hist" class="tab-panel active" data-group="pop-tabs">',
  plotly_div("no-hist", plotly_to_json(p_hist), "400px", source = SSB_SOURCE,
    legend_html = no_pop_leg),
  '</div>',
  '<div id="no-tab-emp" class="tab-panel" data-group="pop-tabs">',
  plotly_div("no-employ", plotly_to_json(p_employ), "400px", source = SSB_EMP_SOURCE,
    legend_html = no_emp_leg),
  '</div>',
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in Norway</div>',
  plotly_div("no-county", plotly_to_json(p_county), "500px", source = SSB_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Norway: Population", pop_body, has_tabs = TRUE),
           "docs/pages/no-population.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nNorway: %s Iranian-origin (%s immigrants + %s Norwegian-born)\n",
  format(no_total, big.mark = ","),
  format(no_gen1, big.mark = ","),
  format(no_gen2, big.mark = ",")))
cat(sprintf("Counties: %d, Oslo share: %d%%\n", nrow(county), oslo_pct))
cat(sprintf("Employment rate (20-66): %.1f%% (%d)\n",
  latest_emp$pct_total, latest_emp$year))

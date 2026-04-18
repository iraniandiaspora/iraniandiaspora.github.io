# Build Finland pages from Eurostat extract.
# Run from deployment repo root:
#   Rscript R/build_finland.R
#
# Input:  data/finland/fi_trend.csv, fi_headline.csv
# Output: docs/pages/fi-population.html
#
# Extract first via: Rscript R/fi_export/extract_eurostat_fi.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/finland"

# --- Helpers (canonical pattern, from build_italy.R) -------------------------
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
    'var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox",showTips:true});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  chart <- sprintf(
    '<div id="%s" style="width:100%%;height:%s;touch-action:manipulation;"></div>\n<script>(function(){%s})();</script>',
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
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"

# --- Load data ---------------------------------------------------------------
cat("Loading Finland extracts...\n")
trend <- read.csv(file.path(DATA_DIR, "fi_trend.csv"), stringsAsFactors = FALSE)
hl    <- read.csv(file.path(DATA_DIR, "fi_headline.csv"), stringsAsFactors = FALSE)
trend <- trend[order(trend$year), ]

fi_total  <- hl$count[hl$category == "total"]
data_yr   <- hl$year[hl$category == "total"]
fi_min_yr <- min(trend$year)

TREND_SOURCE <- sprintf("Source: %s &mdash; Iran-born population stock, %d\u2013%d",
                        EURO_LINK, fi_min_yr, data_yr)

# =============================================================================
# FI-POPULATION
# =============================================================================
cat("Building fi-population...\n")

# --- Historical trend (Eurostat, annual 2001-2025) ---------------------------
p_hist <- plot_ly(trend, x = ~year, y = ~iran_born, type = "bar",
    marker = list(color = "#2774AE",
                  line = list(color = "#1a4e72", width = 0.4)),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      trend$year, format(trend$iran_born, big.mark = ",")),
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  layout(
    title = list(
      text = sprintf("<b>Iran-Born Population in Finland,<br>%d\u2013%d</b>",
                     fi_min_yr, data_yr),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 4),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Assemble fi-population page ---------------------------------------------
pop_body <- paste0(
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in Finland</div>',
  '<div class="number">', format(fi_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the population register maintained by ',
  STATFIN_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Finland uses a continuous population register. A person is counted as Iran-born based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> <span style="color:#888;">&mdash; recorded in the Digital and Population Data Services Agency (DVV) register</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Finnish-born children of Iran-born parents are not counted here. This figure reflects first-generation residents only.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card">',
  plotly_div("fi-hist", plotly_to_json(p_hist), "430px", source = TREND_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Finland: Population", pop_body),
           "docs/pages/fi-population.html")
cat("  Done\n")

cat(sprintf("\nFinland: %s Iran-born (%d); %d years (%d-%d)\n",
  format(fi_total, big.mark = ","), data_yr,
  nrow(trend), fi_min_yr, data_yr))

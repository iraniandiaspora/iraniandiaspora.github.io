# UK population page — ONS Census 2021 (E+W) + Scotland Census 2022 + NI 2021.
#
# Consumes:
#   data/uk/iran_uk_summary_2021.csv  -- 4 UK nations + totals
#   data/uk/uk_la_clean.csv           -- 331 lower-tier local authorities (E+W)
#
# Produces:
#   docs/pages/uk-population.html
#
# Run from the deployment repo root:
#   Rscript R/build_uk.R
#
# (Regenerate the clean LA files first via:
#    Rscript R/uk_export/clean_nomis_data.R)

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

# ---------- PLOT HELPERS (matches the other builders' pattern) ------------
unclass_deep <- function(x) {
  if (is.list(x)) {
    class(x) <- NULL
    x[] <- lapply(x, unclass_deep)
  } else if (is.numeric(x) || is.integer(x) || is.logical(x) || is.character(x)) {
    class(x) <- NULL
  }
  x
}
plotly_to_json <- function(plot_obj) {
  b <- plotly_build(plot_obj)$x
  b$data   <- unclass_deep(b$data)
  b$layout <- unclass_deep(b$layout)
  list(
    data   = toJSON(b$data, auto_unbox = TRUE, null = "null", na = "null"),
    layout = toJSON(b$layout, auto_unbox = TRUE, null = "null", na = "null"),
    config = toJSON(list(responsive = TRUE, displayModeBar = FALSE), auto_unbox = TRUE)
  )
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
.chart-row { display:grid; grid-template-columns:1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:hidden; min-width:0; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.headline { background:white; border-radius:8px; padding:30px; text-align:center; border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row { grid-template-columns:1fr !important; }
  .headline .number { font-size:34px; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .headline .number { font-size:28px; }
  .chart-card { padding:10px; }
}
</style>
</head>
<body>
', body_html, '
', iframe_resize_script, '
</body>
</html>')
}

# ---------- DATA ----------------------------------------------------------
cat("Building UK population page...\n")

summary_df <- read.csv("data/uk/iran_uk_summary_2021.csv", stringsAsFactors = FALSE)
region_df  <- read.csv("data/uk/uk_region_iran.csv", stringsAsFactors = FALSE)

uk_total  <- summary_df$iran_born[summary_df$country == "UK total"]
nireland  <- summary_df$iran_born[summary_df$country == "Northern Ireland"]

# ---------- CHART: 11-region UK choropleth --------------------------------
uk_geojson <- jsonlite::fromJSON("data/uk/uk_regions.geojson",
  simplifyVector = FALSE)

p_uk_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = uk_geojson,
    locations = region_df$region_code,
    z = region_df$iran_born,
    featureidkey = "properties.AREACD",
    text = sprintf("<b>%s</b><br>%s Iran-born residents<br>%.1f%% of UK total",
      region_df$region_name,
      format(region_df$iran_born, big.mark = ","),
      region_df$iran_born / uk_total * 100),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"), c(0.08, "#6baed6"),
      c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = FALSE,
    marker = list(line = list(color = "white", width = 1), opacity = 0.92)
  ) %>% layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lon = -2.5, lat = 54.7),
      zoom = 4.3
    ),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# ---------- PAGE ASSEMBLY -------------------------------------------------
ONS_LINK    <- "<a href='https://www.nomisweb.co.uk/sources/census_2021' target='_blank' style='color:#2774AE;'>ONS Census 2021</a>"
SCOT_LINK   <- "<a href='https://www.scotlandscensus.gov.uk/' target='_blank' style='color:#2774AE;'>Scotland&rsquo;s Census 2022</a>"
NISRA_LINK  <- "<a href='https://www.nisra.gov.uk/statistics/census/2021-census' target='_blank' style='color:#2774AE;'>NISRA Census 2021</a>"

pop_body <- paste0(
  # Single top row: headline (40%) + UK choropleth map (60%)
  '<div class="chart-row" style="grid-template-columns:40% 60%;">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in the United Kingdom</div>',
  '<div class="number">', format(uk_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the most recent ', ONS_LINK,
  ', ', SCOT_LINK, ', and ', NISRA_LINK, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Each nation&rsquo;s census asks the same <strong>place of birth</strong> question:</p>',
  '<p style="padding-left:12px; margin:0; font-style:italic; color:#555;">&ldquo;What is your country of birth?&rdquo;</p>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">The UK census does not ask about ancestry or parental origin, so British-born children of Iran-born parents are not counted here. The map shows nine English regions plus Scotland and Wales; Northern Ireland (461 Iran-born) is not shown on the regional map. Sum of LAD-aggregated regions differs from the published England + Wales total by &plusmn;6 due to ONS disclosure rounding at the local authority level.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution of Iran-Born Residents</div>',
  plotly_div("uk-region-map", plotly_to_json(p_uk_map), "500px",
    source = paste0("Source: ", ONS_LINK, " (England and Wales); ", SCOT_LINK,
      ". Aggregated from 331 local authorities into nine English regions plus Scotland and Wales.")),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("United Kingdom: Population", pop_body), "docs/pages/uk-population.html")
cat("  Done\n")
cat(sprintf("\nUK population: %s Iran-born (map shows 11 regions; NI %d not shown)\n",
  format(uk_total, big.mark = ","), nireland))

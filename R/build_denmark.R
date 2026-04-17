# Build Denmark pages from DST (Statistics Denmark) extracts.
# Run from deployment repo root:
#   Rscript R/build_denmark.R
#
# Input:  data/denmark/*.csv, data/europe/iran_born_combined.csv
# Output: docs/pages/dk-population.html, dk-workinc.html
#
# Extract first via: Rscript R/dk_export/extract_dst.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/denmark"

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
DST_LINK <- "<a href='https://www.dst.dk/en/' target='_blank' style='color:#2774AE;'>Statistics Denmark (DST)</a>"
DST_SOURCE <- paste0("Source: ", DST_LINK, " &mdash; Population Register, 2026")
DST_EMP_SOURCE <- paste0("Source: ", DST_LINK, " &mdash; Full-time employees (RAS), Q4 2024")
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"

# --- Load data ---------------------------------------------------------------
cat("Loading Denmark DST extracts...\n")
hl      <- read.csv(file.path(DATA_DIR, "dk_headline.csv"), stringsAsFactors = FALSE)
trend   <- read.csv(file.path(DATA_DIR, "dk_trend.csv"), stringsAsFactors = FALSE)
region  <- read.csv(file.path(DATA_DIR, "dk_region.csv"), stringsAsFactors = FALSE)
industry <- read.csv(file.path(DATA_DIR, "dk_industry.csv"), stringsAsFactors = FALSE)

dk_total <- hl$count[hl$category == "total"]
dk_gen1  <- hl$count[hl$category == "gen1"]
dk_gen2  <- hl$count[hl$category == "gen2"]
data_yr  <- hl$year[1]

# Clean region names: strip "Region " prefix
region$region_name <- sub("^Region ", "", region$region_name)

# Translate industry names from Danish to English
industry_en <- c(
  "Landbrug, skovbrug og fiskeri" = "Agriculture & Fishing",
  "R\u00e5stofindvinding" = "Mining & Quarrying",
  "Industri" = "Manufacturing",
  "Energiforsyning" = "Energy Supply",
  "Vandforsyning og renovation" = "Water & Waste",
  "Bygge og anl\u00e6g" = "Construction",
  "Handel" = "Trade & Retail",
  "Transport" = "Transport",
  "Hoteller og restauranter" = "Hotels & Restaurants",
  "Information og kommunikation" = "IT & Telecom",
  "Finansiering og forsikring" = "Finance & Insurance",
  "Ejendomshandel og udlejning" = "Real Estate",
  "Videnservice" = "Professional Services",
  "Rejsebureauer, reng\u00f8ring og anden operationel service" = "Admin & Support",
  "Offentlig administration, forsvar og politi" = "Public Admin & Defence",
  "Undervisning" = "Education",
  "Sundhed og socialv\u00e6sen" = "Health & Social Work",
  "Kultur og fritid" = "Arts & Recreation",
  "Andre serviceydelser mv." = "Other Services"
)
industry$sector_en <- ifelse(industry$sector_name %in% names(industry_en),
                             industry_en[industry$sector_name],
                             industry$sector_name)


# =============================================================================
# DK-POPULATION
# =============================================================================
cat("Building dk-population...\n")

# --- Historical trend 1980-2026 (stacked area: gen1 + gen2) ---
p_hist <- plot_ly() %>%
  add_trace(data = trend, x = ~year, y = ~gen1, type = "scatter",
    mode = "lines", stackgroup = "one", fillcolor = "rgba(26,78,114,0.7)",
    line = list(color = "#1a4e72", width = 1),
    name = "Immigrants",
    text = sprintf("<b>%d</b><br>Immigrants: %s<br>Descendants: %s<br>Total: %s",
      trend$year, format(trend$gen1, big.mark = ","),
      format(trend$gen2, big.mark = ","),
      format(trend$total, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  add_trace(data = trend, x = ~year, y = ~gen2, type = "scatter",
    mode = "lines", stackgroup = "one", fillcolor = "rgba(90,155,213,0.7)",
    line = list(color = "#5a9bd5", width = 1),
    name = "Descendants",
    text = sprintf("<b>%d</b><br>Descendants: %s",
      trend$year, format(trend$gen2, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Origin Population in Denmark,<br>1980\u20132026</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ","),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

dk_trend_leg <- make_html_legend(c("Immigrants" = "#1a4e72", "Descendants" = "#5a9bd5"))

# --- Region choropleth map ---------------------------------------------------
dk_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "dk_regions.geojson"),
                                 simplifyVector = FALSE)
region$join_name <- gsub("^Region ", "", region$region_name)
region$pct <- round(region$count / dk_total * 100, 1)

p_region <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = dk_geojson,
    locations = region$join_name, z = region$count,
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>%s Iranian-origin (%.1f%%)",
      region$region_name,
      format(region$count, big.mark = ","), region$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                      c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = 10.5, lat = 56), zoom = 5.5),
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
  make_gen_box(dk_gen1, paste0(round(dk_gen1 / dk_total * 100), "% of total"),
    "Immigrants", "Born in Iran", "#1a4e72"),
  make_gen_box(dk_gen2, paste0(round(dk_gen2 / dk_total * 100), "% of total"),
    "Descendants", "Born in Denmark to Iranian parent(s)", "#5a9bd5"),
  '</div>')

# --- Assemble dk-population page ---------------------------------------------
hovedstaden_pct <- round(region$count[region$region_name == "Hovedstaden"] / dk_total * 100)

pop_body <- paste0(
  # Top row: headline + generation grid
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Origin Population in Denmark</div>',
  '<div class="number">', format(dk_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  DST_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Denmark uses population registers. A person is classified as Iranian-origin if they are:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Immigrant</strong>: born in Iran, neither parent a Danish citizen born in Denmark</li>',
  '<li><strong>Descendant</strong>: born in Denmark, neither parent a Danish citizen born in Denmark</li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">The descendant category requires that neither parent is a Danish citizen born in Denmark. Third-generation (both parents Danish-born citizens) are not counted as Iranian-origin.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  gen_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', DST_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: historical trend + region bar
  '<div class="chart-row">',
  '<div class="chart-card">',
  plotly_div("dk-hist", plotly_to_json(p_hist), "400px", source = DST_SOURCE,
    legend_html = dk_trend_leg),
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in Denmark</div>',
  plotly_div("dk-region", plotly_to_json(p_region), "500px", source = DST_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Denmark: Population", pop_body),
           "docs/pages/dk-population.html")
cat("  Done\n")


# =============================================================================
# DK-WORKINC (industry employment)
# =============================================================================
cat("Building dk-workinc...\n")

# --- Industry horizontal bar chart -------------------------------------------
industry <- industry[order(industry$count), ]  # ascending for horizontal bar
industry$sector_en <- factor(industry$sector_en, levels = industry$sector_en)
total_employed <- industry$total_employed[1]

# Categorical colors for distinct industry sectors
dk_ind_colors <- rep(c("#1a4e72", "#2774AE", "#4a8c6f", "#c4793a", "#d4a943",
                       "#7b5ea7", "#2ca089", "#e07b54"), length.out = nrow(industry))
p_industry <- plot_ly(industry, y = ~sector_en, x = ~count, type = "bar",
    orientation = "h", marker = list(color = dk_ind_colors),
    text = sprintf("<b>%s</b><br>%s employees (%.1f%%)",
      industry$sector_en,
      format(industry$count, big.mark = ","),
      industry$pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Origin Full-Time Employees<br>by Industry in Denmark, 2024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 10)),
    margin = list(l = 140, r = 20, t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# Top 3 sectors
top3 <- industry[order(-industry$count), ][1:3, ]

workinc_body <- paste0(
  '<div class="chart-row">',
  sprintf('<div class="text-card pt1" style="text-align:center; display:flex; flex-direction:column; justify-content:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%.0f%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iranian-origin full-time equivalent employees in Denmark work in %s.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:380px; margin-left:auto; margin-right:auto;">Next largest sectors: %s (%.0f%%) and %s (%.0f%%).</div>
    <div style="font-size:11px; color:#999; margin-top:14px; line-height:1.5; max-width:380px; margin-left:auto; margin-right:auto;">Based on Register-based Labour Force Statistics (RAS). Self-employed are not included.</div>
  </div>',
    top3$pct[1], top3$sector_en[1],
    top3$sector_en[2], top3$pct[2],
    top3$sector_en[3], top3$pct[3]),
  '<div class="chart-card">',
  plotly_div("dk-industry", plotly_to_json(p_industry), "520px", source = DST_EMP_SOURCE),
  '</div>',
  '</div>'
)

writeLines(page_template("Denmark: Work & Income", workinc_body),
           "docs/pages/dk-workinc.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nDenmark: %s Iranian-origin (%s immigrants + %s descendants)\n",
  format(dk_total, big.mark = ","),
  format(dk_gen1, big.mark = ","),
  format(dk_gen2, big.mark = ",")))
cat(sprintf("Regions: %d, Hovedstaden share: %d%%\n",
  nrow(region), hovedstaden_pct))
cat(sprintf("Employed: %s across %d sectors\n",
  format(total_employed, big.mark = ","), nrow(industry)))

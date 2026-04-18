# Build Türkiye pages from Eurostat extracts.
# Run from deployment repo root:
#   Rscript R/build_turkey.R
#
# Inputs:  data/turkey/{tr_trend.csv, tr_citizens.csv, tr_age_2025.csv, tr_headline.csv}
# Outputs: docs/pages/tr-population.html, docs/pages/tr-immigration.html
#
# Extract first via: Rscript R/tr_export/extract_eurostat_tr.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/turkey"

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
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
TUIK_LINK <- "<a href='https://data.tuik.gov.tr/' target='_blank' style='color:#2774AE;'>T\u00dc\u0130K</a>"

HEADLINE_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iran-born residents of T\u00fcrkiye, 2025")
TREND_SOURCE <- paste0("Source: ", EURO_LINK,
                       " &mdash; Iran-born population stock, 2019\u20132025 (2021\u20132022 not published by ",
                       TUIK_LINK, ")")
AGE_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iran-born by age and sex, 2025")
CIT_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iranian citizens resident in T\u00fcrkiye, 2014\u20132025")
COMPARE_SOURCE <- paste0("Source: ", EURO_LINK,
                         " &mdash; Iran-born and Iranian citizens, T\u00fcrkiye 2014\u20132025")

# --- Load data ---------------------------------------------------------------
cat("Loading T\u00fcrkiye extracts...\n")
trend   <- read.csv(file.path(DATA_DIR, "tr_trend.csv"), stringsAsFactors = FALSE)
cit     <- read.csv(file.path(DATA_DIR, "tr_citizens.csv"), stringsAsFactors = FALSE)
age_df  <- read.csv(file.path(DATA_DIR, "tr_age_2025.csv"), stringsAsFactors = FALSE)
hl      <- read.csv(file.path(DATA_DIR, "tr_headline.csv"), stringsAsFactors = FALSE)
trend   <- trend[order(trend$year), ]
cit     <- cit[order(cit$year), ]

tr_total  <- hl$count[hl$category == "total"]
tr_male   <- hl$count[hl$category == "male"]
tr_female <- hl$count[hl$category == "female"]
data_yr   <- hl$year[hl$category == "total"]
tr_min_yr <- min(trend$year)

# =============================================================================
# TR-POPULATION (Figures 1 & 2)
# =============================================================================
cat("Building tr-population...\n")

# --- Figure 1: Annual Iran-born stock 2019-2025 (bar) -------------------------
p_hist <- plot_ly(trend, x = ~year, y = ~total, type = "bar",
    marker = list(color = "#2774AE",
                  line = list(color = "#1a4e72", width = 0.4)),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      trend$year, format(trend$total, big.mark = ",")),
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  layout(
    title = list(
      text = sprintf("<b>Iran-Born Population in T\u00fcrkiye,<br>%d\u2013%d</b>",
                     tr_min_yr, data_yr),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickmode = "array", tickvals = trend$year),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 40),
    plot_bgcolor = "white", paper_bgcolor = "white",
    annotations = list(list(
      x = 2021.5, y = 0, xref = "x", yref = "paper",
      text = "reporting gap<br>2021\u20132022", showarrow = FALSE,
      font = list(size = 10, color = "#888")
    ))
  ) %>% config(displayModeBar = FALSE)

# --- Figure 2: Age distribution 2025, M vs F (horizontal bars) ----------------
# Age axis: oldest (85+) at top, youngest (Under 5) at bottom — standard
# population-chart convention. Plotly renders the first factor level at the
# bottom of the y-axis, so levels go youngest → oldest here.
age_df$age_label <- factor(age_df$age_label, levels = age_df$age_label)

# Trace order: female added first (so within each pair the female bar sits
# BELOW the male bar), male second (on top). The `traceorder = "reversed"`
# legend flag keeps "Male" listed first in the legend.
p_age <- plot_ly() %>%
  add_trace(
    x = age_df$female, y = age_df$age_label,
    type = "bar", orientation = "h",
    name = "Female",
    marker = list(color = "#c4793a"),
    text = sprintf("<b>%s</b><br>Female: %s",
      as.character(age_df$age_label), format(age_df$female, big.mark = ",")),
    hoverinfo = "text", textposition = "none"
  ) %>%
  add_trace(
    x = age_df$male, y = age_df$age_label,
    type = "bar", orientation = "h",
    name = "Male",
    marker = list(color = "#1a4e72"),
    text = sprintf("<b>%s</b><br>Male: %s",
      as.character(age_df$age_label), format(age_df$male, big.mark = ",")),
    hoverinfo = "text", textposition = "none"
  ) %>%
  layout(
    title = list(
      text = "<b>Iran-Born in T\u00fcrkiye by Age and Sex,<br>2025</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 10)),
    margin = list(t = 50, b = 30, l = 60, r = 20),
    barmode = "group", bargap = 0.2,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08,
                  traceorder = "reversed"),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Sex breakdown boxes (from headline counts) -------------------------------
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
  make_gen_box(tr_male, paste0(round(tr_male / tr_total * 100), "% of total"),
    "Male", "Iran-born men", "#1a4e72"),
  make_gen_box(tr_female, paste0(round(tr_female / tr_total * 100), "% of total"),
    "Female", "Iran-born women", "#c4793a"),
  '</div>')

# --- Assemble tr-population page (Italy format; no map available) ------------
pop_body <- paste0(
  # Top row: headline (left) + sex boxes card (right)
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in T\u00fcrkiye</div>',
  '<div class="number">', format(tr_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the address-based population register maintained by ',
  TUIK_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">T\u00fcrkiye uses an address-based population register. A person is counted as Iran-born based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> <span style="color:#888;">&mdash; recorded in the address-based register (ADNKS)</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">T\u00fcrkiye-born children of Iran-born parents are not counted as Iran-born in these statistics. The figure reflects first-generation residents only.</p>',
  '<p style="margin-top:8px; font-size:11px; color:#999; line-height:1.5;">The UN International Migrant Stock reports about 200,000 Iran-born residents of T\u00fcrkiye in 2024, higher than the figure above because UN numbers add UNHCR-registered refugees and asylum seekers to the national register and are extrapolated from 2020.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  sex_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>',
          HEADLINE_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: Iran-born trend (left) + age chart (right)
  '<div class="chart-row">',
  '<div class="chart-card">',
  plotly_div("tr-hist", plotly_to_json(p_hist), "540px", source = TREND_SOURCE),
  '</div>',
  '<div class="chart-card">',
  plotly_div("tr-age", plotly_to_json(p_age), "540px", source = AGE_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("T\u00fcrkiye: Population", pop_body),
           "docs/pages/tr-population.html")
cat("  Done (tr-population)\n")

# =============================================================================
# TR-IMMIGRATION (Figures 3 & 4)
# =============================================================================
cat("Building tr-immigration...\n")

# --- Figure 3: Iranian citizens 2014-2025 (bar) -------------------------------
p_cit <- plot_ly(cit, x = ~year, y = ~iranian_citizens, type = "bar",
    marker = list(color = "#2774AE",
                  line = list(color = "#1a4e72", width = 0.4)),
    text = sprintf("<b>%d</b><br>%s Iranian citizens resident in T\u00fcrkiye",
      cit$year, format(cit$iranian_citizens, big.mark = ",")),
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  layout(
    title = list(
      text = "<b>Iranian Citizens Resident in T\u00fcrkiye,<br>2014\u20132025</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickmode = "array", tickvals = cit$year),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 40),
    plot_bgcolor = "white", paper_bgcolor = "white",
    annotations = list(list(
      x = 2021.5, y = 0.5, xref = "x", yref = "paper",
      text = "reporting<br>gap<br>2021\u20132022", showarrow = FALSE,
      font = list(size = 11, color = "#888"),
      align = "center"
    ))
  ) %>% config(displayModeBar = FALSE)

# --- Figure 4: Iran-born vs Iranian citizens overlay, 2014-2025 ---------------
# Citizens series is longer (2014-2025); Iran-born is 2019-2025 with gap.
# Show both as lines with markers, same x-axis; Iran-born dashed where missing.
compare_years <- sort(unique(c(cit$year, trend$year)))
compare <- data.frame(year = compare_years) %>%
  left_join(cit %>% rename(citizens = iranian_citizens), by = "year") %>%
  left_join(trend %>% select(year, iran_born = total), by = "year")

p_compare <- plot_ly() %>%
  add_trace(
    data = compare, x = ~year, y = ~citizens,
    type = "scatter", mode = "lines+markers",
    name = "Iranian citizens",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 7),
    text = ~sprintf("<b>%d</b><br>%s Iranian citizens",
      year, format(citizens, big.mark = ",")),
    hoverinfo = "text"
  ) %>%
  add_trace(
    data = compare, x = ~year, y = ~iran_born,
    type = "scatter", mode = "lines+markers",
    name = "Iran-born",
    line = list(color = "#c4793a", width = 2.5, dash = "dash"),
    marker = list(color = "#c4793a", size = 7, symbol = "square"),
    text = ~sprintf("<b>%d</b><br>%s Iran-born",
      year, format(iran_born, big.mark = ",")),
    hoverinfo = "text",
    connectgaps = FALSE
  ) %>%
  layout(
    title = list(
      text = "<b>Iran-Born vs Iranian Citizens in T\u00fcrkiye,<br>2014\u20132025</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickmode = "array",
                 tickvals = compare_years),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 50, l = 60, r = 15),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Text cards for page 2 ----------------------------------------------------
cit_peak <- cit$iranian_citizens[which.max(cit$iranian_citizens)]
cit_peak_yr <- cit$year[which.max(cit$iranian_citizens)]
cit_2014 <- cit$iranian_citizens[cit$year == 2014]
cit_growth <- round(cit_peak / cit_2014, 1)

# 2025 overlap: born in Iran vs holding Iranian passport
born_2025 <- trend$total[trend$year == 2025]
cit_2025 <- cit$iranian_citizens[cit$year == 2025]
# Gap (Iran-born minus Iranian citizens) roughly reflects naturalization
naturalized_approx <- born_2025 - cit_2025

imm_body <- paste0(
  '<div class="page-content">',
  # pt1: factoid above pc1 (Iranian citizens surge chart)
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s\u00d7</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">Iranian citizens resident in T\u00fcrkiye grew %s-fold between 2014 (%s) and 2023 (%s).</div>
  </div>',
    cit_growth, cit_growth,
    format(cit_2014, big.mark = ","),
    format(cit_peak, big.mark = ",")),
  # pt2: factoid above pc2 (Iran-born vs citizens overlay)
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">gap in 2025 between Iran-born residents (%s) and Iranian-passport holders (%s).</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">The gap captures former Iranian nationals who have naturalized as Turkish citizens, plus Iran-born residents of other nationalities. The two series use different denominators, so their difference approximates &mdash; but does not exactly measure &mdash; naturalization.</div>
  </div>',
    format(naturalized_approx, big.mark = ","),
    format(born_2025, big.mark = ","),
    format(cit_2025, big.mark = ",")),
  '<div class="chart-card pc1">',
  plotly_div("tr-citizens", plotly_to_json(p_cit), "430px", source = CIT_SOURCE),
  '</div>',
  '<div class="chart-card pc2">',
  plotly_div("tr-compare", plotly_to_json(p_compare), "430px", source = COMPARE_SOURCE),
  '</div>',
  '</div>'
)

writeLines(page_template("T\u00fcrkiye: Immigration", imm_body),
           "docs/pages/tr-immigration.html")
cat("  Done (tr-immigration)\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nT\u00fcrkiye: %s Iran-born (%d)\n",
            format(tr_total, big.mark = ","), data_yr))
cat(sprintf("Citizens: %s \u2192 %s (%d\u2013%d)\n",
            format(cit_2014, big.mark = ","),
            format(cit$iranian_citizens[nrow(cit)], big.mark = ","),
            min(cit$year), max(cit$year)))

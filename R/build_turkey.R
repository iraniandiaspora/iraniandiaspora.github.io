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

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
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
  if (typeof reportHeight === "function") reportHeight();
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
', MAPBOX_ATTRIB_HIDE_CSS, '
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
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
TUIK_LINK <- "<a href='https://data.tuik.gov.tr/' target='_blank' style='color:#2774AE;'>T\u00dc\u0130K</a>"
GOC_LINK  <- "<a href='https://www.goc.gov.tr/ikamet-izinleri' target='_blank' style='color:#2774AE;'>Presidency of Migration Management (G\u00f6\u00e7 \u0130daresi)</a>"

HEADLINE_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iran-born residents of T\u00fcrkiye, 2025")
TREND_SOURCE <- paste0("Source: ", EURO_LINK, " and ", TUIK_LINK,
                       " &mdash; Iran-born residents of T\u00fcrkiye, 2015\u20132025")
AGE_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iran-born by age and sex, 2025")
CIT_SOURCE <- paste0("Source: ", EURO_LINK, " and ", TUIK_LINK,
                     " &mdash; Iranian citizens in T\u00fcrkiye, 2014\u20132025")
COMPARE_SOURCE <- paste0("Source: ", EURO_LINK, " and ", TUIK_LINK,
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
    plot_bgcolor = "white", paper_bgcolor = "white"
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
    yaxis = list(title = "", tickfont = list(size = 10),
                 ticks = "outside", ticklen = 8,
                 tickcolor = "rgba(0,0,0,0)"),
    margin = list(t = 50, b = 30, l = 70, r = 20),
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
  '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">Iran-Born Population by Sex</div>',
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
    text = sprintf("<b>%d</b><br>%s Iranian citizens",
      cit$year, format(cit$iranian_citizens, big.mark = ",")),
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  layout(
    title = list(
      text = "<b>Iranian Citizens Resident in T\u00fcrkiye,<br>2014\u20132025</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickmode = "array", tickvals = cit$year),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 40),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Figure 4: Iran-born vs Iranian citizens overlay, 2014-2025 ---------------
# Citizens series is longer (2014-2025); Iran-born is 2019-2025.
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
# --- Residence permits by type (Göç İdaresi snapshot, May 2026) --------------
permit <- read.csv(file.path(DATA_DIR, "tr_permits.csv"), stringsAsFactors = FALSE)
permit_total <- permit$count[permit$permit_type == "total"]
permit_breakdown <- permit %>%
  filter(permit_type != "total") %>%
  mutate(
    label = factor(c("Short-term", "Student", "Family", "Other / long-term")[
      match(permit_type, c("short_term", "student", "family", "other_long_term"))],
      levels = c("Other / long-term", "Family", "Student", "Short-term")),
    pct = round(count / permit_total * 100)
  ) %>% arrange(label)
permit_short_pct   <- permit_breakdown$pct[permit_breakdown$label == "Short-term"]
permit_student_pct <- permit_breakdown$pct[permit_breakdown$label == "Student"]

p_permits <- plot_ly(permit_breakdown, y = ~label, x = ~count, type = "bar",
    orientation = "h",
    marker = list(color = c("#b0b0b0", "#8a5a3a", "#d4a943", "#2774AE")),
    text = ~sprintf("<b>%s</b><br>%s permits (%d%% of total)",
      as.character(label), format(count, big.mark = ","), pct),
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  layout(
    title = list(
      text = "<b>Iranian Residence Permits in Türkiye,<br>by Type (May 2026)</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 12),
                 ticks = "outside", ticklen = 8,
                 tickcolor = "rgba(0,0,0,0)"),
    margin = list(t = 55, b = 40, l = 140),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

PERMIT_SOURCE <- paste0(
  "Source: ", GOC_LINK,
  " &mdash; Residence permits by type and nationality, 7 May 2026"
)

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
  # pt1: factoid above pc1 (citizens trend). Iranian-citizen growth 2014\u2192peak.
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s\u00d7</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">Iranian citizens resident in T\u00fcrkiye grew %s-fold between 2014 (%s) and the %d peak (%s).</div>
  </div>',
    cit_growth, cit_growth,
    format(cit_2014, big.mark = ","),
    cit_peak_yr,
    format(cit_peak, big.mark = ",")),
  # pt2: factoid above pc2 (permits by type).
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">Iranian residence permits in T\u00fcrkiye in May 2026 \u2014 the fourth-largest foreign group after Turkmen, Azerbaijani, and Syrian nationals.</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>About %d%% are short-term permits</li>
      <li>About %d%% are student permits</li>
      <li>Iranian students are the second-largest foreign-student group in T\u00fcrkiye</li>
    </ul>
  </div>',
    format(permit_total, big.mark = ","),
    permit_short_pct, permit_student_pct),
  # pc1: citizens trend (untabbed now that Iran-born trend lives on the pop page)
  '<div class="chart-card pc1">',
  plotly_div("tr-citizens", plotly_to_json(p_cit), "430px", source = CIT_SOURCE),
  '</div>',
  # pc2: permits by type (replaces the Iran-born vs citizens overlay, which
  # was the same data as the citizens chart; Iran-born moved to tr-population)
  '<div class="chart-card pc2">',
  plotly_div("tr-permits", plotly_to_json(p_permits), "430px", source = PERMIT_SOURCE),
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

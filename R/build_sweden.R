# Build Sweden pages from SCB (Statistics Sweden) extracts.
# Run from deployment repo root:
#   Rscript R/build_sweden.R
#
# Input:  data/sweden/*.csv, data/europe/iran_born_combined.csv
# Output: docs/pages/se-population.html
#
# Extract first via: Rscript R/se_export/extract_scb.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/sweden"

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
SCB_LINK <- "<a href='https://www.scb.se/en/' target='_blank' style='color:#2774AE;'>Statistics Sweden (SCB)</a>"
SCB_SOURCE <- paste0("Source: ", SCB_LINK, " &mdash; Population Register, 2025")
SCB_POP_SOURCE <- paste0("Source: ", SCB_LINK, " &mdash; Population by country of birth, 2024")
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
HIST_SOURCE <- paste0("Source: ", EURO_LINK, ", Iran-born population stock")

# --- Load data ---------------------------------------------------------------
cat("Loading Sweden SCB extracts...\n")
hl      <- read.csv(file.path(DATA_DIR, "se_headline.csv"), stringsAsFactors = FALSE)
trend   <- read.csv(file.path(DATA_DIR, "se_trend.csv"), stringsAsFactors = FALSE)
county  <- read.csv(file.path(DATA_DIR, "se_county.csv"), stringsAsFactors = FALSE)
yrssince <- read.csv(file.path(DATA_DIR, "se_yrssince.csv"), stringsAsFactors = FALSE)

se_total <- hl$count[hl$category == "total"]
se_gen1  <- hl$count[hl$category == "gen1"]
se_gen2  <- hl$count[hl$category == "gen2"]
data_yr  <- hl$year[1]

# Clean county names: strip " county" suffix
county$county_name <- sub(" county$", "", county$county_name)


# =============================================================================
# SE-POPULATION
# =============================================================================
cat("Building se-population...\n")

# --- Historical trend (Eurostat + SCB merged to fill gaps) -------------------
# Eurostat has gaps (2000-2002, 2022); SCB has 2000-2024 continuous.
# Use Eurostat as base, fill gaps from SCB.
eurostat_all <- read.csv("data/europe/iran_born_combined.csv",
                         stringsAsFactors = FALSE)
euro_se <- eurostat_all %>%
  filter(geo == "SE") %>%
  mutate(year = as.integer(year), iran_born = as.integer(value)) %>%
  select(year, iran_born) %>%
  arrange(year)

scb_hist <- read.csv(file.path(DATA_DIR, "se_hist_iran_born.csv"),
                     stringsAsFactors = FALSE)
# Fill missing Eurostat years from SCB
missing_years <- setdiff(scb_hist$year, euro_se$year)
if (length(missing_years) > 0) {
  fill <- scb_hist %>%
    filter(year %in% missing_years) %>%
    select(year, iran_born)
  euro_se <- bind_rows(euro_se, fill) %>% arrange(year)
}
# Extend back with UN Migrant Stock 1990, 1995
un_global <- read.csv("data/global/stocks_countries.csv", stringsAsFactors = FALSE)
un_se <- un_global[grepl("Sweden", un_global$destination), ]
if (nrow(un_se) > 0) {
  un_pts <- data.frame(year = c(1990L, 1995L),
    iran_born = c(un_se$X1990, un_se$X1995))
  un_pts <- un_pts[!un_pts$year %in% euro_se$year, ]
  euro_se <- bind_rows(un_pts, euro_se) %>% arrange(year)
}

p_hist <- plot_ly(euro_se, x = ~year, y = ~iran_born, type = "scatter",
    mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 5),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      euro_se$year, format(euro_se$iran_born, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iran-Born Population in Sweden,<br>1990\u20132025</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Years since immigration chart -------------------------------------------
yrs <- yrssince[!is.na(yrssince$count), ]
# Create display labels
yrs$label <- yrs$duration
yrs$label <- sub(" years$", "y", yrs$label)
yrs$label <- sub(" year$", "y", yrs$label)
yrs$label <- sub("50 \\+ y", "50+y", yrs$label)

total_yrs <- sum(yrs$count)
yrs$pct <- round(yrs$count / total_yrs * 100, 1)

p_yrssince <- plot_ly(yrs, x = ~seq_len(nrow(yrs)) - 1L, y = ~count, type = "bar",
    marker = list(color = "#2774AE"),
    text = sprintf("<b>%s</b><br>%s (%s%%)",
      yrs$duration, format(yrs$count, big.mark = ","), yrs$pct),
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iran-Born by Years Since Immigration,<br>Sweden 2024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickangle = -45, tickfont = list(size = 9),
      tickvals = seq(0, nrow(yrs) - 1),
      ticktext = yrs$label),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 40, b = 70),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- County choropleth map ---------------------------------------------------
se_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "se_counties.geojson"),
                                 simplifyVector = FALSE)
county$join_name <- gsub(" county$", "", county$county_name)
county$pct <- round(county$count / se_gen1 * 100, 1)

p_county <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = se_geojson,
    locations = county$join_name, z = county$count,
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>%s Iran-born (%.1f%%)",
      county$county_name,
      format(county$count, big.mark = ","), county$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                      c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = 17, lat = 62), zoom = 3.3),
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
  make_gen_box(se_gen1, paste0(round(se_gen1 / se_total * 100), "% of total"),
    "First generation", "Born in Iran", "#1a4e72"),
  make_gen_box(se_gen2, paste0(round(se_gen2 / se_total * 100), "% of total"),
    "Second generation", "Born in Sweden, parent(s) born in Iran", "#5a9bd5"),
  '</div>')

# --- Stockholm metro share ---
stockholm <- county$count[county$county_name == "Stockholm"]
sthlm_pct <- round(stockholm / se_gen1 * 100)

# --- Assemble se-population page ---------------------------------------------
pop_body <- paste0(
  # Top row: headline + generation grid
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Origin Population in Sweden</div>',
  '<div class="number">', format(se_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  SCB_LINK, ', ', data_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Sweden uses population registers. A person is classified as Iranian-origin if they meet at least one of:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Born in Iran</strong> <span style="color:#888;">&mdash; first generation</span></li>',
  '<li><strong>Born in Sweden</strong> with a mother or father born in Iran <span style="color:#888;">&mdash; second generation</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">The second generation includes children with one Swedish-born and one Iran-born parent. Third-generation residents are not counted. Classification is from birth records, not self-reported ethnicity.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  gen_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', SCB_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: tabbed (historical trend | pyramid | years since) + county bar
  '<div class="chart-row">',
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'se-tab-hist\',this,\'pop-tabs\')">Population Over Time</button>',
  '<button class="tab-btn" onclick="switchTab(\'se-tab-yrs\',this,\'pop-tabs\')">Years Since Immigration</button>',
  '</div>',
  '<div id="se-tab-hist" class="tab-panel active" data-group="pop-tabs">',
  plotly_div("se-hist", plotly_to_json(p_hist), "430px", source = HIST_SOURCE),
  '</div>',
  '<div id="se-tab-yrs" class="tab-panel" data-group="pop-tabs">',
  plotly_div("se-yrssince", plotly_to_json(p_yrssince), "430px", source = SCB_POP_SOURCE),
  '</div>',
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in Sweden</div>',
  plotly_div("se-county", plotly_to_json(p_county), "500px", source = SCB_POP_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Sweden: Population", pop_body, has_tabs = TRUE),
           "docs/pages/se-population.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nSweden: %s Iranian-origin (%s 1st gen + %s 2nd gen)\n",
  format(se_total, big.mark = ","),
  format(se_gen1, big.mark = ","),
  format(se_gen2, big.mark = ",")))
cat(sprintf("Counties: %d, Stockholm share: %d%%\n", nrow(county), sthlm_pct))

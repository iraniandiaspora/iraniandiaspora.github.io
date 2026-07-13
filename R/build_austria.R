# Build Austria pages from Eurostat Iran-born data.
# Run from deployment repo root:
#   Rscript R/build_austria.R
#
# Input:  data/austria/at_trend.csv
# Output: docs/pages/at-population.html
#
# Extract first via: Rscript R/at_export/extract_eurostat_at.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/austria"

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

# --- Source citation strings ---
STAT_AT_LINK <- "<a href='https://www.statistik.at/en/' target='_blank' style='color:#2774AE;'>Statistics Austria</a>"
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
TREND_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iran-born population stock, 2000\u20132025; 1990 and 1995 from UN DESA")

# --- Load data ---------------------------------------------------------------
cat("Loading Austria trend data...\n")
trend <- read.csv(file.path(DATA_DIR, "at_trend.csv"), stringsAsFactors = FALSE)
trend <- trend[order(trend$year), ]

at_latest <- trend$iran_born[nrow(trend)]
at_latest_yr <- trend$year[nrow(trend)]
at_min_yr <- min(trend$year)

# =============================================================================
# AT-POPULATION
# =============================================================================
cat("Building at-population...\n")

# --- Historical trend line chart -----------------------------------------------
p_hist <- plot_ly(trend, x = ~year, y = ~iran_born, type = "scatter",
    mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 5),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      trend$year, format(trend$iran_born, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(
      text = sprintf("<b>Iran-Born Population in Austria,<br>%d\u2013%d</b>",
                     at_min_yr, at_latest_yr),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 50, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Bundesland horizontal bar chart ------------------------------------------
bl <- read.csv(file.path(DATA_DIR, "at_bundesland.csv"), stringsAsFactors = FALSE)
bl <- bl[order(bl$iran_born), ]
bl$bundesland <- factor(bl$bundesland, levels = bl$bundesland)
bl_total <- sum(bl$iran_born)
bl$pct <- round(bl$iran_born / bl_total * 100, 1)

CENSUS_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Census 2021, Iran-born by region")

# --- Bundesland choropleth map ------------------------------------------------
at_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "at_bundesland.geojson"),
                                 simplifyVector = FALSE)

p_bl_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = at_geojson,
    locations = bl$bundesland, z = bl$iran_born,
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>%s Iran-born<br>%.1f%% of total",
      bl$bundesland, format(bl$iran_born, big.mark = ","), bl$pct),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                      c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = 13.5, lat = 47.6), zoom = 5.4),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Sex breakdown boxes (from Bundesland census data) -------------------------
at_male   <- sum(bl$male)
at_female <- sum(bl$female)

sex_boxes <- paste0(
  '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">Iran-Born Population by Sex</div>',
  '<div style="display:flex; gap:12px; margin-top:12px;">',
  make_gen_box(at_male, paste0(round(at_male / bl_total * 100), "% of total"),
    "Male", "Iran-born men", "#1a4e72"),
  make_gen_box(at_female, paste0(round(at_female / bl_total * 100), "% of total"),
    "Female", "Iran-born women", "#5a9bd5"),
  '</div>')

# --- Assemble at-population page (Italy format) --------------------------------
pop_body <- paste0(
  # Top row: headline (left) + sex boxes card (right)
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in Austria</div>',
  '<div class="number">', format(at_latest, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  STAT_AT_LINK, ', ', at_latest_yr, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Austria uses population registers. A person is counted as Iran-born based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
  '<li><strong>Country of birth</strong> <span style="color:#6b6b6b;">&mdash; recorded in the Central Register of Residents</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Austrian-born children of Iran-born parents are not counted as Iran-born in population statistics. This count reflects first-generation residents only.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  sex_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', CENSUS_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: trend line chart (left) + choropleth map (right)
  '<div class="chart-row">',
  '<div class="chart-card">',
  plotly_div("at-hist", plotly_to_json(p_hist), "430px", source = TREND_SOURCE),
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in Austria</div>',
  plotly_div("at-map", plotly_to_json(p_bl_map), "430px", source = CENSUS_SOURCE),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Austria: Population", pop_body, has_tabs = FALSE),
           "docs/pages/at-population.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nAustria: %s Iran-born (%d)\n",
  format(at_latest, big.mark = ","), at_latest_yr))
cat(sprintf("Trend: %d years (%d-%d)\n", nrow(trend), at_min_yr, at_latest_yr))

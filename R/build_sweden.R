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

# --- Source citation strings ---
SCB_LINK <- "<a href='https://www.scb.se/en/' target='_blank' style='color:#2774AE;'>Statistics Sweden (SCB)</a>"
SCB_SOURCE <- paste0("Source: ", SCB_LINK, " &mdash; Population Register, 2025")
SCB_POP_SOURCE <- paste0("Source: ", SCB_LINK, " &mdash; Population by country of birth, 2024")
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
HIST_SOURCE <- paste0("Source: ", EURO_LINK, " &mdash; Iran-born population stock, 2000–2024")

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

# --- Year of arrival chart (US model: calendar x-axis + cumulative line) ------
# SCB reports "years since immigration" in single-year bins (0-9), 5-year bins
# (10-49), and an open 50+ bin. We reframe to year of arrival to match the
# US/NL/AU pattern. The 5-year and open bins are normalized to a per-year
# average height with proportional bar width (the Australia method) so a
# multi-year cohort cannot visually swamp a single year; the cumulative-%
# line is computed from the true (un-averaged) counts. Data are 2024 vintage.
yrs <- yrssince[!is.na(yrssince$count), ]

parse_arrival <- function(dur) {
  if (grepl("^50", dur)) {                 # "50 + years" -> open tail
    list(span = 5L, low = 1970L, high = 1974L, label = "Before 1975")
  } else if (grepl("-", dur)) {            # "10-14 years" -> 5-year bin
    b <- as.integer(strsplit(sub(" years", "", dur), "-")[[1]])
    list(span = b[2] - b[1] + 1L, low = 2024L - b[2], high = 2024L - b[1],
         label = sprintf("%d–%d", 2024L - b[2], 2024L - b[1]))
  } else {                                 # "N year(s)" -> single year
    n <- as.integer(sub(" .*", "", dur))
    list(span = 1L, low = 2024L - n, high = 2024L - n,
         label = as.character(2024L - n))
  }
}
pa <- lapply(yrs$duration, parse_arrival)
yrs$span      <- vapply(pa, `[[`, integer(1), "span")
yrs$arr_low   <- vapply(pa, `[[`, integer(1), "low")
yrs$arr_high  <- vapply(pa, `[[`, integer(1), "high")
yrs$arr_lab   <- vapply(pa, `[[`, character(1), "label")
yrs$center    <- (yrs$arr_low + yrs$arr_high) / 2
yrs$is_period <- yrs$span > 1L
yrs$annual_avg <- round(yrs$count / yrs$span)
yrs$bar_width  <- ifelse(yrs$is_period, yrs$span * 0.9, 0.8)
yrs$bar_color  <- ifelse(yrs$is_period, "#5a9bd5", "#2774AE")

# Cumulative share from TRUE counts, oldest arrival cohort first
yrs <- yrs[order(yrs$center), ]
yrs$cum_true <- cumsum(yrs$count)
yrs$cum_pct  <- round(yrs$cum_true / sum(yrs$count) * 100, 1)

yrs$hover <- ifelse(yrs$is_period,
  sprintf("<b>%s</b><br>%s arrivals (%s/year avg)<br>Cumulative: %.1f%%",
    yrs$arr_lab, format(yrs$count, big.mark = ","),
    format(yrs$annual_avg, big.mark = ","), yrs$cum_pct),
  sprintf("<b>%s</b><br>%s arrivals<br>Cumulative: %.1f%%",
    yrs$arr_lab, format(yrs$count, big.mark = ","), yrs$cum_pct))

p_yrssince <- plot_ly() %>%
  add_bars(data = yrs, x = ~center, y = ~annual_avg, width = ~bar_width,
    marker = list(color = ~bar_color),
    text = ~hover, hoverinfo = "text", textposition = "none",
    showlegend = FALSE) %>%
  add_trace(data = yrs, x = ~center, y = ~cum_pct, type = "scatter",
    mode = "lines", yaxis = "y2",
    line = list(color = "lightblue", width = 2),
    hoverinfo = "skip", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iran-Born Residents in Sweden<br>by Year of Arrival</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 10, range = c(1967, 2026),
      tickfont = list(size = 11)),
    yaxis = list(title = "", tickformat = ","),
    yaxis2 = list(title = "", overlaying = "y", side = "right",
      ticksuffix = "%", range = c(0, 105), showgrid = FALSE,
      tickfont = list(size = 10, color = "#6b6b6b")),
    margin = list(t = 40, b = 55, r = 45),
    plot_bgcolor = "white", paper_bgcolor = "white",
    annotations = list(
      list(text = "Arrivals before 2015 are grouped into 5-year periods, shown as a per-year average.",
        x = 0.5, y = -0.15, xref = "paper", yref = "paper", showarrow = FALSE,
        font = list(size = 9, color = "#6b6b6b"), xanchor = "center"))
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
gen_boxes <- paste0(
  '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">Iranian-Origin Population by Generation</div>',
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
  '<li><strong>Born in Iran</strong> <span style="color:#6b6b6b;">&mdash; first generation</span></li>',
  '<li><strong>Born in Sweden</strong> with a mother or father born in Iran <span style="color:#6b6b6b;">&mdash; second generation</span></li>',
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
  '<button class="tab-btn" onclick="switchTab(\'se-tab-yrs\',this,\'pop-tabs\')">Year of Arrival</button>',
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

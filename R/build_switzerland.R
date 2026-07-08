# Build Switzerland page from BFS (Swiss Federal Statistical Office) extracts.
# Run from deployment repo root:
#   Rscript R/build_switzerland.R
#
# Input:  data/switzerland/*.csv, data/switzerland/ch_cantons.geojson
# Output: docs/pages/ch-population.html
#
# Extract first via: Rscript R/ch_export/extract_bfs.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/switzerland"

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

# --- Annual arrivals 2011-2024 (bars + stock-share % line) ---------------------
# The cumulative line is computed against the *stock* of Iran-born residents
# (ch_trend), not against arrivals alone. Iran-born residents existed in
# Switzerland well before 2011, so a cumsum(arrivals) starting at 0 would
# misleadingly imply no Iranians were present in 2010. Instead: at year y,
# line value = stock[y] / stock[latest] * 100 = "share of current stock
# already present by end of year y". Anchor the line at 2010 to show the
# pre-chart baseline (~47%).
arrivals <- arrivals[order(arrivals$year), ]
trend <- trend[order(trend$year), ]
total_arr <- sum(arrivals$count)
stock_final <- trend$total[trend$year == max(trend$year)]
# Line is anchored at arrivals years (2011-2024). At year Y, value =
# stock[Y] / stock[latest] * 100 = "share of current stock already present
# by end of year Y". The first point (2011) naturally lands above zero
# because a substantial Iran-born community existed before the chart
# starts; the line has no 2010 point so it aligns with the first bar.
stock_line <- merge(data.frame(year = arrivals$year), trend, by = "year", all.x = TRUE)
stock_line <- stock_line[order(stock_line$year), ]
stock_line$cum_pct <- round(stock_line$total / stock_final * 100, 1)
max_bar <- max(arrivals$count)

p_arrivals <- plot_ly() %>%
  add_bars(data = arrivals, x = ~year, y = ~count,
    marker = list(color = "#2774AE",
      line = list(color = "#1a4e72", width = 0.3)),
    text = sprintf("<b>%d</b><br>%s arrivals",
      arrivals$year, format(arrivals$count, big.mark = ",")),
    hoverinfo = "text", textposition = "none", showlegend = FALSE, name = "Arrivals") %>%
  add_trace(data = stock_line, x = ~year,
    y = ~cum_pct / 100 * max_bar,
    type = "scatter", mode = "lines",
    yaxis = "y2",
    line = list(color = "lightblue", width = 2),
    text = sprintf("<b>By end of %d:</b> %.0f%% of 2024 Iran-born stock already present",
      stock_line$year, stock_line$cum_pct),
    hoverinfo = "text", showlegend = FALSE, name = "Share of current stock",
    inherit = FALSE) %>%
  layout(
    title = list(
      text = "<b>Iran-Born Annual Immigration<br>to Switzerland, 2011\u20132024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2, range = c(2010.5, 2024.5)),
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
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = 8.2, lat = 46.8), zoom = 5.8),
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
    yaxis = list(title = "", tickfont = list(size = 9),
                 ticks = "outside", ticklen = 8,
                 tickcolor = "rgba(0,0,0,0)"),
    margin = list(l = 170, r = 20, t = 40, b = 30),
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

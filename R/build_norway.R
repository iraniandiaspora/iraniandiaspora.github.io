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
SSB_LINK <- "<a href='https://www.ssb.no/en/' target='_blank' style='color:#2774AE;'>Statistics Norway (SSB)</a>"
SSB_SOURCE <- paste0("Source: ", SSB_LINK, " &mdash; Population Register, 2026")
SSB_EMP_SOURCE <- paste0("Source: ", SSB_LINK, " &mdash; Employed immigrants, fourth quarter, 2001\u20132025")

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

# --- Historical total Iran-born 1970-2026 (single line) ----------------------
# Single total line: the value of this series is its length (1970-2026), which
# shows when the Iran-born population took off. The sex split was dropped (no
# other country breaks population by sex, and it added no information); the
# generation breakdown lives in its own tab (gen data only goes back to 2010).
no_hist_max <- max(hist_sex$year)
p_hist <- plot_ly(hist_sex, x = ~year, y = ~total, type = "scatter",
    mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 5),
    text = ~sprintf("<b>%d</b><br>%s Iran-born", year, format(total, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = sprintf("<b>Iran-Born Population in Norway,<br>1970\u2013%d</b>", no_hist_max),
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 10),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Generation split (stacked area) -----------------------------------------
# Norway's register splits 1st gen (Iran-born) from 2nd gen (Norwegian-born to
# Iran-born parents) only from 2010 onward. Stacked area shows each generation's
# relative size; the total-line tab carries the long pre-2010 history. Both
# traces share one hover so the mouseover is consistent (the old by-sex chart
# showed different fields on each band).
gen_trend <- trend[trend$year <= data_yr, ]
no_gen_min <- min(gen_trend$year); no_gen_max <- max(gen_trend$year)
no_gen_hover <- sprintf(
  "<b>%d</b><br>Immigrants: %s<br>Norwegian-born: %s<br>Total: %s",
  gen_trend$year, format(gen_trend$gen1, big.mark = ","),
  format(gen_trend$gen2, big.mark = ","), format(gen_trend$total, big.mark = ","))
p_gen <- plot_ly(gen_trend) %>%
  add_trace(x = ~year, y = ~gen1, type = "scatter", mode = "lines",
    stackgroup = "one", name = "Immigrants",
    fillcolor = "rgba(26,78,114,0.75)", line = list(color = "#1a4e72", width = 1),
    text = no_gen_hover, hoverinfo = "text") %>%
  add_trace(x = ~year, y = ~gen2, type = "scatter", mode = "lines",
    stackgroup = "one", name = "Norwegian-born",
    fillcolor = "rgba(90,155,213,0.75)", line = list(color = "#5a9bd5", width = 1),
    text = no_gen_hover, hoverinfo = "text") %>%
  layout(
    title = list(text = sprintf("<b>Iranian-Origin Population in Norway<br>by Generation, %d\u2013%d</b>", no_gen_min, no_gen_max),
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

no_gen_leg <- make_html_legend(c("Immigrants" = "#1a4e72",
                                 "Norwegian-born" = "#5a9bd5"))

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
    title = list(text = "<b>Employment Rate of Iranian Immigrants<br>(Ages 20\u201366), 2001\u20132025</b>",
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
    text = sprintf("<b>%s</b><br>%s Iranian-origin<br>%.1f%% of total",
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
      center = list(lon = 15, lat = 64), zoom = 3.2),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Generation boxes --------------------------------------------------------
gen_boxes <- paste0(
  '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">Iranian-Origin Population by Generation</div>',
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
  '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
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
  '<button class="tab-btn active" onclick="switchTab(\'no-tab-hist\',this,\'pop-tabs\')">Population Over Time</button>',
  '<button class="tab-btn" onclick="switchTab(\'no-tab-gen\',this,\'pop-tabs\')">By Generation</button>',
  '<button class="tab-btn" onclick="switchTab(\'no-tab-emp\',this,\'pop-tabs\')">Employment Rate</button>',
  '</div>',
  '<div id="no-tab-hist" class="tab-panel active" data-group="pop-tabs">',
  plotly_div("no-hist", plotly_to_json(p_hist), "400px", source = SSB_SOURCE),
  '</div>',
  '<div id="no-tab-gen" class="tab-panel" data-group="pop-tabs">',
  plotly_div("no-gen", plotly_to_json(p_gen), "400px", source = SSB_SOURCE,
    legend_html = no_gen_leg),
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

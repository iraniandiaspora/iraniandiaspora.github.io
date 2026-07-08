# Build Israel page from CBS (Central Bureau of Statistics) extracts.
# Run from deployment repo root:
#   Rscript R/build_israel.R
#
# Input:  data/israel/*.csv
# Output: docs/pages/il-population.html
#
# Extract first via: Rscript R/il_export/extract_cbs_israel.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/israel"

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
CBS_LINK <- "<a href='https://www.cbs.gov.il/en' target='_blank' style='color:#2774AE;'>Israel Central Bureau of Statistics (CBS)</a>"
CBS_SOURCE <- paste0("Source: ", CBS_LINK, " &mdash; Statistical Abstract of Israel, 2024")

# --- Load data ---------------------------------------------------------------
cat("Loading Israel CBS extracts...\n")
hl       <- read.csv(file.path(DATA_DIR, "il_headline.csv"), stringsAsFactors = FALSE)
age      <- read.csv(file.path(DATA_DIR, "il_age.csv"), stringsAsFactors = FALSE)
age_det  <- read.csv(file.path(DATA_DIR, "il_age_detail.csv"), stringsAsFactors = FALSE)
comp     <- read.csv(file.path(DATA_DIR, "il_comparison.csv"), stringsAsFactors = FALSE)

il_total <- hl$count[hl$category == "total"]
il_gen1  <- hl$count[hl$category == "gen1"]
il_gen2  <- hl$count[hl$category == "gen2"]

# =============================================================================
# IL-POPULATION
# =============================================================================
cat("Building il-population...\n")

# --- Age distribution (grouped horizontal bars) --------------------------------
# Order age groups from youngest at bottom to oldest at top
age_order <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55+")
age$age_group <- factor(age$age_group, levels = age_order)
age <- age[order(age$age_group), ]

# Add Israeli-born first so Iran-born (dark blue) sits on top of each group
p_age <- plot_ly() %>%
  add_trace(y = age$age_group, x = age$gen2, type = "bar", orientation = "h",
    name = "Israeli-born (2nd gen)",
    marker = list(color = "#5a9bd5"),
    text = sprintf("<b>%s</b><br>Israeli-born: %s", age$age_group,
      format(age$gen2, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  add_trace(y = age$age_group, x = age$gen1, type = "bar", orientation = "h",
    name = "Iran-born (1st gen)",
    marker = list(color = "#1a4e72"),
    text = sprintf("<b>%s</b><br>Iran-born: %s", age$age_group,
      format(age$gen1, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Age Distribution of Iranian-Origin<br>Population in Israel</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", autorange = TRUE, categoryorder = "array",
      categoryarray = age_order,
      ticks = "outside", ticklen = 8,
      tickcolor = "rgba(0,0,0,0)"),
    barmode = "group",
    showlegend = FALSE,
    margin = list(t = 50, b = 30, l = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

age_legend <- make_html_legend(
  c("Iran-born (1st gen)" = "#1a4e72", "Israeli-born (2nd gen)" = "#5a9bd5"))

# --- Iran-born detail chart (full 8 age bins) ----------------------------------
gen1_det <- age_det[age_det$generation == "Iran-born", ]
gen1_order <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
gen1_det$age_group <- factor(gen1_det$age_group, levels = gen1_order)
gen1_det <- gen1_det[order(gen1_det$age_group), ]
gen1_det$pct <- round(gen1_det$count / il_gen1 * 100, 1)

# Blue gradient from light (youngest) to dark (oldest)
gen1_colors <- c("#d4e6f1", "#c6dbef", "#8bbdde", "#5a9bd5", "#2774AE",
                 "#1a5c8a", "#1a4e72", "#08306b")

p_gen1_detail <- plot_ly() %>%
  add_trace(y = gen1_det$age_group, x = gen1_det$count, type = "bar", orientation = "h",
    marker = list(color = gen1_colors),
    text = sprintf("<b>%s</b><br>%s (%.1f%%)",
      gen1_det$age_group, format(gen1_det$count, big.mark = ","), gen1_det$pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iran-Born Population in Israel<br>by Age</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", categoryorder = "array", categoryarray = gen1_order,
                 ticks = "outside", ticklen = 8,
                 tickcolor = "rgba(0,0,0,0)"),
    showlegend = FALSE,
    margin = list(t = 50, b = 30, l = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Iran-born population over time (UN Migrant Stock 1990–2024) --------------
# Bottom-right slot (replaces the Asian-origin comparison, which moves to a
# second tab on the same card). UN line only — register figure is implicit
# in the headline.
un_csv <- read.csv("data/global/stocks_countries.csv", stringsAsFactors = FALSE,
                   check.names = FALSE)
un_row <- un_csv[un_csv$destination == "Israel" &
                 grepl("^Iran", un_csv$origin), ]
un_years <- as.integer(sub("^X", "", names(un_row)[grepl("^X", names(un_row))]))
un_vals  <- as.integer(unlist(un_row[, grepl("^X", names(un_row))]))
un_trend <- data.frame(year = un_years, iran_born = un_vals)

p_un_trend <- plot_ly(un_trend, x = ~year, y = ~iran_born,
    type = "scatter", mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 8),
    text = ~sprintf("<b>%d</b><br>%s Iran-born",
      year, format(iran_born, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(
      text = "<b>Iran-Born Population in Israel,<br>1990–2024</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 55, b = 40, l = 60, r = 15),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

UN_SOURCE <- paste0(
  "Source: <a href='https://www.un.org/development/desa/pd/content/international-migrant-stock' target='_blank' style='color:#2774AE;'>UN DESA</a> — International Migrant Stock 2024"
)

# --- Comparison with other Asian-origin groups --------------------------------
# Sort by total descending (already sorted). Categorical colors per country.
comp_colors <- c("Iraq" = "#7b5ea7", "Iran" = "#1a4e72", "Yemen" = "#d4a943",
                 "T\u00fcrkiye" = "#2ca089", "India and Pakistan" = "#e07b54",
                 "Syria and Lebanon" = "#8bbdde")
comp$color <- comp_colors[comp$country]
comp$country <- factor(comp$country, levels = rev(comp$country))

p_comp <- plot_ly() %>%
  add_trace(y = comp$country, x = comp$total, type = "bar", orientation = "h",
    marker = list(color = comp$color),
    text = sprintf("<b>%s</b><br>Total: %s<br>First generation: %s<br>Second generation: %s",
      comp$country, format(comp$total, big.mark = ","),
      format(comp$gen1, big.mark = ","), format(comp$gen2, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Asian-Origin Groups in Israel</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "",
                 ticks = "outside", ticklen = 8,
                 tickcolor = "rgba(0,0,0,0)"),
    showlegend = FALSE,
    margin = list(t = 40, b = 30, l = 130),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Generation boxes --------------------------------------------------------
gen_boxes <- paste0(
  '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">Iranian-Origin Population by Generation</div>',
  '<div style="display:flex; gap:12px; margin-top:12px;">',
  make_gen_box(il_gen1, paste0(round(il_gen1 / il_total * 100), "% of total"),
    "Iran-born", "Born in Iran", "#1a4e72"),
  make_gen_box(il_gen2, paste0(round(il_gen2 / il_total * 100), "% of total"),
    "Israeli-born", "Born in Israel, father born in Iran", "#5a9bd5"),
  '</div>')

# --- Age stat for text card ---
pct_55plus <- round(sum(age$gen1[age$age_group == "55+"]) / il_gen1 * 100)

# --- Assemble il-population page ----------------------------------------------
pop_body <- paste0(
  # Top row: headline + generation boxes
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Origin Population in Israel</div>',
  '<div class="number">', format(il_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  CBS_LINK, ', 2024</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">Israel defines &ldquo;country of origin&rdquo; using a paternal-line, two-generation rule:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Born abroad</strong>: own country of birth is Iran</li>',
  '<li><strong>Born in Israel</strong>: father&rsquo;s country of birth is Iran</li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Third-generation descendants (Israeli-born, father Israeli-born) and maternal-only lines are not counted.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  gen_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', CBS_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: tabbed age charts + comparison chart
  '<div class="chart-row">',
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'il-tab-both\',this,\'age-tabs\')">Iran-Born + Israeli-Born</button>',
  '<button class="tab-btn" onclick="switchTab(\'il-tab-detail\',this,\'age-tabs\')">Iran-Born Only</button>',
  '</div>',
  '<div id="il-tab-both" class="tab-panel active" data-group="age-tabs">',
  plotly_div("il-age", plotly_to_json(p_age), "430px", source = CBS_SOURCE,
    legend_html = age_legend),
  '</div>',
  '<div id="il-tab-detail" class="tab-panel" data-group="age-tabs">',
  plotly_div("il-gen1-detail", plotly_to_json(p_gen1_detail), "430px", source = CBS_SOURCE),
  '</div>',
  '</div>',
  # Bottom-right: tabbed chart — population over time (default) +
  # Asian-origin comparison (second tab). Per framework rules we don't
  # add new rows; the comparative-context chart becomes a second tab.
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'il-tab-trend\',this,\'il-right-tabs\')">Population Over Time</button>',
  '<button class="tab-btn" onclick="switchTab(\'il-tab-comp\',this,\'il-right-tabs\')">Comparative Context</button>',
  '</div>',
  '<div id="il-tab-trend" class="tab-panel active" data-group="il-right-tabs">',
  plotly_div("il-un-trend", plotly_to_json(p_un_trend), "430px", source = UN_SOURCE),
  '</div>',
  '<div id="il-tab-comp" class="tab-panel" data-group="il-right-tabs">',
  plotly_div("il-comp", plotly_to_json(p_comp), "430px", source = CBS_SOURCE),
  '</div>',
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Israel: Population", pop_body, has_tabs = TRUE),
           "docs/pages/il-population.html")
cat("  Done\n")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nIsrael: %s Iranian-origin (%s Iran-born + %s Israeli-born)\n",
  format(il_total, big.mark = ","),
  format(il_gen1, big.mark = ","),
  format(il_gen2, big.mark = ",")))
cat(sprintf("Iran-born aged 55+: %d%%\n", pct_55plus))
cat(sprintf("Iran is #%d largest Asian-origin group (after Iraq)\n",
  which(comp$country[order(-comp$total)] == "Iran")))

# Build all 6 Canada pages from pre-computed CSV outputs
# Run from the iraniandiaspora.github.io/ directory:
#   Rscript R/build_canada.R

library(plotly)
library(dplyr)
library(jsonlite)

DATA_DIR <- "data/canada"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
make_html_legend <- function(colors, labels = names(colors), break_after = NULL) {
  items <- mapply(function(col, lab) {
    # Escape & as &amp; in data-lg and JS string so it matches plotly legendgroup after HTML decoding
    html_lab <- gsub("&", "&amp;", lab)
    sprintf('<span data-lg="%s" style="display:inline-flex; align-items:center; gap:4px; cursor:pointer; transition:opacity 0.2s;" onmouseenter="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOn)el.__hlOn(this.getAttribute(\'data-lg\'));" onmouseleave="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOff)el.__hlOff();"><span style="width:12px; height:12px; background:%s; border-radius:2px; display:inline-block;"></span> %s</span>',
      lab, col, html_lab)
  }, colors, labels, SIMPLIFY = TRUE)
  sprintf('<div style="display:flex; justify-content:center; flex-wrap:wrap; gap:6px 14px; font-size:12px; color:#444; margin:6px 0 2px; line-height:2;">%s</div>',
    paste(items, collapse = ""))
}

PUMF_LINK <- "<a href='https://dc1.chass.utoronto.ca/census/index.html' target='_blank' style='color:#2774AE;'>Census PUMF</a>"
PUMF_SOURCE <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "Based on a 2.7% public-use sample of census records.<br>Estimates for small sub-groups may have limited reliability.")
PUMF_SRC_IMMIG <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "Iran-born respondents only. Pre-1995 immigration periods are grouped into multi-year bands.<br>Smaller estimates should be interpreted with caution.")
PUMF_SRC_EDUC_1G <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "First generation ages 30+. Smaller estimates should be interpreted with caution.")
PUMF_SRC_FOS_1G <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "First generation ages 25+. Smaller estimates should be interpreted with caution.")
PUMF_SRC_EDUC_2G <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "Second-generation data not shown:<br>too few respondents for reliable estimates.")
PUMF_SRC_WORK_1G <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "First generation ages 25+, by age group and gender. Smaller estimates should be interpreted with caution.")
PUMF_SRC_WORK_2G <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "Second generation ages 25+, all ages combined due to small sample size. Smaller estimates should be interpreted with caution.")
PUMF_SRC_INCOME <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "Ages 25\u201354 (prime working years). Each decile holds 10% of all Canadian households, ranked by pre-tax household income.<br>Smaller estimates should be interpreted with caution.")
PUMF_SRC_INCOME_AGE <- paste0("Source: ", PUMF_LINK, " \u2014 2021 Canadian Census<br>",
  "First generation, ages 25\u201374. Pre-tax personal income. Smaller estimates should be interpreted with caution.")

# Tab-switching JS (injected once per page that uses tabs)
tab_switch_script <- '
<script>
function switchTab(tabId, btn, groupId) {
  var panels = document.querySelectorAll(".tab-panel[data-group=\'" + groupId + "\']");
  panels.forEach(function(p) { p.classList.remove("active"); });
  document.getElementById(tabId).classList.add("active");
  var btns = btn.parentElement.querySelectorAll(".tab-btn");
  btns.forEach(function(b) { b.classList.remove("active"); });
  btn.classList.add("active");
  // Resize any plotly charts in newly-visible tab and re-bindhover events
  var active = document.getElementById(tabId);
  active.querySelectorAll(".js-plotly-plot").forEach(function(p) {
    if (window.Plotly) {
      Plotly.Plots.resize(p);
      if (p.__hlOn) {
        p.removeAllListeners("plotly_hover");
        p.removeAllListeners("plotly_unhover");
        p.on("plotly_hover", function(d) { p.__hlOn(d.points[0].data.legendgroup); });
        p.on("plotly_unhover", p.__hlOff);
      }
    }
  });
  reportHeight();
}
</script>'

page_template <- function(title, body_html, has_tabs = FALSE, extra_head = "") {
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
.text-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.text-row-4 { display:grid; grid-template-columns:repeat(4,1fr); gap:16px; margin-bottom:20px; }
.text-card { background:white; border-radius:8px; padding:20px; text-align:center;
  font-size:15px; line-height:1.6; border:1px solid #e0e0e0; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:hidden; min-width:0; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.headline { background:white; border-radius:8px; padding:30px; text-align:center; border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em; }
a { transition: color 0.15s; }
a:hover { color: #1a4e72 !important; text-decoration: underline; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
.source { font-size:12px; color:#666; text-align:right; padding:4px 0; margin-top:10px; }
.source a { color:#2774AE; }
.footnote { font-size:12px; color:#888; text-align:center; margin:8px 0; font-style:italic; }
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }
.page-content-4 { display:grid; grid-template-columns:repeat(4,1fr); gap:16px; margin-bottom:20px; }
.page-content-4 .chart-card { margin-bottom:0; }
.p4-t1 { grid-area:1/1; } .p4-t2 { grid-area:1/2; } .p4-t3 { grid-area:1/3; } .p4-t4 { grid-area:1/4; }
.p4-c1 { grid-area:2/1/2/3; } .p4-c2 { grid-area:2/3/2/5; }', tab_css, '
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .text-row-4 { grid-template-columns:1fr 1fr; }
  .text-row, .text-row-4 { order:1; } /* push text below charts on mobile */
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .page-content-4 { grid-template-columns:1fr; }
  .p4-t1,.p4-t2,.p4-t3,.p4-t4,.p4-c1,.p4-c2 { grid-area:auto; }
  .p4-c1 { order:1; } .p4-t1 { order:2; } .p4-t2 { order:3; }
  .p4-c2 { order:4; } .p4-t3 { order:5; } .p4-t4 { order:6; }
  .headline .number { font-size:34px; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
}
@media (max-width:600px) {
  .text-row-4 { grid-template-columns:1fr !important; }
}
@media (max-width:600px) {
  .id-table { border-spacing:4px !important; }
  .id-cell { padding:8px 4px !important; min-height:60px !important; }
  .id-num { font-size:16px !important; }
  .id-label-col { width:55px !important; font-size:10px !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .headline .number { font-size:28px; }
  .chart-card { padding:10px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
', extra_head, '
</head>
<body>
', body_html, '
', if (has_tabs) tab_switch_script else '', '
', iframe_resize_script, '
</body>
</html>')
}

# Colors
blues <- c("#1a4e72", "#2774AE", "#5a9bd5", "#8bbdde", "#d4e6f1")


# =====================================================
# CANADA POPULATION
# =====================================================
cat("Building ca-population...\n")

pop <- read.csv(file.path(DATA_DIR, "population/iranian_population_final_breakdown_complete.csv"))
pop_included <- pop %>% filter(Included_in_Total == "Yes")

# --- Waterfall chart (compound definition, same style as US/AU) ---
wf <- pop_included
wf$cumulative <- cumsum(wf$Population)
wf$ymin <- wf$cumulative - wf$Population
wf$short_label <- c("Born + Iranian\n+ Persian",
                     "Born + Iranian\n(no Persian)",
                     "Born + Persian\n(no Iranian)",
                     "Born only\n(neither)",
                     "2nd gen +\nIranian + Persian",
                     "2nd gen +\nIranian only",
                     "2nd gen +\nPersian only")

# Same color scheme as US/AU waterfall
wf_colors <- c("#1a4e72", "#2774AE", "#5a9bd5", "#4a8c6f", "#c4793a", "#d4a943", "#7b5ea7")

p_waterfall <- plot_ly() %>%
  add_bars(data = wf, x = ~short_label, y = ~Population, base = ~ymin,
    marker = list(color = wf_colors),
    text = sprintf("<b>%s</b><br>%s (%.1f%%)<br>Cumulative: %s",
      wf$Category, format(wf$Population, big.mark = ","),
      wf$Percentage_of_Total, format(wf$cumulative, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Canadians: How We Count</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 9), tickangle = 0,
      categoryorder = "array", categoryarray = wf$short_label),
    yaxis = list(title = "", tickformat = ","),
    showlegend = FALSE,
    margin = list(t = 50, b = 110, l = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# Region bar chart — built from iranians_by_province.csv so it shares the
# denominator used by the province map below. Previously this pulled from
# immigration/settlement_by_province.csv which sums to first-generation
# settlement only (~169,521) and conflicted with the province-map total
# (~240,189). Both charts now describe the same universe.
prov_for_regions <- read.csv(file.path(DATA_DIR, "population/iranians_by_province.csv"))

# Map provinces to regions (same grouping as before)
prov_totals <- prov_for_regions %>%
  mutate(region = case_when(
    province %in% c("Ontario", "Quebec") ~ "Central",
    province %in% c("British Columbia", "Alberta", "Manitoba", "Saskatchewan") ~ "Western",
    province %in% c("New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                    "Prince Edward Island") ~ "Atlantic",
    province %in% c("Yukon", "Northwest Territories", "Nunavut") ~ "Northern",
    TRUE ~ "Other"
  ))

region_totals <- prov_totals %>%
  group_by(region) %>%
  summarize(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pop))

region_grand <- sum(region_totals$pop, na.rm = TRUE)
region_totals$pct <- round(region_totals$pop / region_grand * 100, 1)

# Add Northern with 0 if not present
region_order <- c("Central", "Western", "Atlantic", "Northern")
if (!"Northern" %in% region_totals$region) {
  region_totals <- bind_rows(region_totals, data.frame(region = "Northern", pop = 0, pct = 0))
}
region_totals <- region_totals %>% filter(region %in% region_order)
region_totals$region <- factor(region_totals$region, levels = region_order)

p_region <- plot_ly(data = region_totals, x = ~region, y = ~pct, type = "bar",
    marker = list(color = c("#7b5ea7", "#d4a943", "#2ca089", "#e07b54")),
    text = ~sprintf("<b>%s Canada</b><br>%s (%.1f%%)", region,
      format(round(pop), big.mark = ","), pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Canadians by<br>Region of Residence</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = ""),
    yaxis = list(title = "", ticksuffix = "%"),
    margin = list(t = 55, b = 50), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

# Provincial choropleth map
prov_data <- read.csv(file.path(DATA_DIR, "population/iranians_by_province.csv"))
# Map province names to ISO 3166-2:CA codes for plotly
prov_iso <- c("Ontario" = "CA-ON", "British Columbia" = "CA-BC", "Quebec" = "CA-QC",
  "Alberta" = "CA-AB", "Manitoba" = "CA-MB", "Saskatchewan" = "CA-SK",
  "Nova Scotia" = "CA-NS", "New Brunswick" = "CA-NB",
  "Newfoundland and Labrador" = "CA-NL", "Prince Edward Island" = "CA-PE",
  "Yukon" = "CA-YT", "Northwest Territories" = "CA-NT", "Nunavut" = "CA-NU")
prov_data$iso <- prov_iso[prov_data$province]
prov_data <- prov_data %>% filter(!is.na(iso))
prov_total <- sum(prov_data$pop, na.rm = TRUE)

prov_geojson_url <- "https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/canada.geojson"
p_prov_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = prov_geojson_url,
    locations = prov_data$province, z = prov_data$pop,
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>%s Iranian-Canadians<br>%.1f%% of total",
      prov_data$province, format(round(prov_data$pop), big.mark = ","),
      prov_data$pop / prov_total * 100),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.01, "#c6dbef"), c(0.1, "#6baed6"),
      c(0.5, "#2171b5"), c(1, "#08306b")),
    showscale = FALSE,
    marker = list(line = list(color = "white", width = 1), opacity = 0.85)
  ) %>% layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lon = -96, lat = 55),
      zoom = 2.5
    ),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# Ontario municipality choropleth — convert sf to GeoJSON for plotly
library(sf)
sf_use_s2(FALSE)
ont_csds <- readRDS(file.path(DATA_DIR, "population/ontario_csds_iranian.rds"))
ont_csds <- ont_csds %>% filter(pop > 0) %>% arrange(desc(pop))

# Convert sf to GeoJSON string for plotly choroplethmapbox alternative:
# Use plotly choropleth with inline GeoJSON
ont_geojson_path <- file.path(DATA_DIR, "population/ontario_csds.geojson")
if (!file.exists(ont_geojson_path)) {
  st_write(ont_csds %>% select(GeoUID, `Region Name`, pop, geometry),
    ont_geojson_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
}

ont_data <- ont_csds %>% st_drop_geometry()

# Ontario map via plotly choroplethmap (plotly.js 3.x with Carto Positron basemap)
ont_geojson <- jsonlite::fromJSON(ont_geojson_path, simplifyVector = FALSE)

p_ont_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = ont_geojson,
    locations = ont_data$GeoUID,
    z = ont_data$pop,
    featureidkey = "properties.GeoUID",
    text = sprintf("<b>%s</b><br>%s residents with Iranian ethnic origin or born in Iran",
      ont_data$`Region Name`, format(round(ont_data$pop), big.mark = ",")),
    hoverinfo = "text",
    colorscale = list(c(0, "#c6dbef"), c(0.05, "#9ecae1"), c(0.15, "#6baed6"),
      c(0.4, "#2171b5"), c(1, "#08306b")),
    showscale = FALSE,
    marker = list(line = list(width = 1, color = "#999"), opacity = 0.85)
  ) %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lon = -79.5, lat = 44),
      zoom = 4.5
    ),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

writeLines(page_template("Canada: Population", paste0(
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Canadian Population</div>',
  sprintf('<div class="number">%s</div>', format(round(sum(pop_included$Population)), big.mark = ",")),
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the <a href="https://www.statcan.gc.ca/census-recensement/2021/ref/questionnaire/index-eng.cfm" style="color:#2774AE;" target="_blank">2021 Canadian Census</a> Public Use Microdata File</div>',
  '<div style="margin:14px auto 0; max-width:460px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">A person is counted if they meet <em>at least one</em> of three census questions:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Place of birth</strong> <span style="color:#888;">&mdash; &ldquo;Where was this person born?&rdquo;</span></li>',
  '<li><strong>Ethnic origin</strong> <span style="color:#888;">&mdash; &ldquo;What are the ethnic or cultural origins of this person&rsquo;s ancestors?&rdquo;</span></li>',
  '<li><strong>Mother tongue or home language</strong> <span style="color:#888;">&mdash; reports Persian as the language first learned in childhood, or as the language spoken most often at home today</span></li>',
  '</ul>',
  '</div>',
  '</div>',
  '<div class="chart-card">',
  plotly_div("ca-waterfall", plotly_to_json(p_waterfall), "430px", source = PUMF_SOURCE),
  '</div>',
  '</div>',
  '<div class="chart-row">',
  '<div class="chart-card">', plotly_div("ca-region", plotly_to_json(p_region), "400px", source = PUMF_SOURCE), '</div>',
  '<div class="chart-card">',
  '<div class="section-title">Geographic Distribution of Iranian-Canadians</div>',
  '<div class="tab-bar"><button class="tab-btn active" onclick="switchTab(\'ca-prov-tab\',this,\'ca-geo\')">By Province</button><button class="tab-btn" onclick="switchTab(\'ca-ont-tab\',this,\'ca-geo\')">By Ontario Municipality</button></div>',
  '<div id="ca-prov-tab" class="tab-panel active" data-group="ca-geo">',
  '<div style="position:relative;">',
  plotly_div("ca-prov-map", plotly_to_json(p_prov_map), "380px", source = PUMF_SOURCE),
  map_overlay_legend(c(
    "#c6dbef" = "1 \u2013 1,000",
    "#6baed6" = "1,000 \u2013 10,000",
    "#2171b5" = "10,000 \u2013 50,000",
    "#08306b" = "50,000 \u2013 150,000")),
  '</div>',
  '<script>if(window.innerWidth<900){setTimeout(function(){var el=document.getElementById("ca-prov-map");if(el&&window.Plotly)Plotly.relayout(el,{"mapbox.zoom":2.0,"mapbox.center.lat":52});},500);}</script>',
  '</div>',
  '<div id="ca-ont-tab" class="tab-panel" data-group="ca-geo">',
  '<div style="position:relative;">',
  plotly_div("ca-ont-map", plotly_to_json(p_ont_map), "380px",
    source = "Source: <a href='https://www.statcan.gc.ca/census-recensement/2021/dp-pd/index-eng.cfm' target='_blank' style='color:#2774AE;'>Statistics Canada</a> \u2014 2021 Census (cancensus aggregate tables, Iranian ethnic origin OR born in Iran, maximum per municipality). Municipal totals differ from the province-level PUMF count because the published aggregate tables are individually suppressed for small census subdivisions and use a different counting rule than the microdata."),
  map_overlay_legend(c(
    "#c6dbef" = "1 \u2013 1,000",
    "#6baed6" = "1,000 \u2013 10,000",
    "#08306b" = "10,000 \u2013 40,000")),
  '</div>',
  '</div>',
  '</div>',
  '</div>'
), has_tabs = TRUE), "docs/pages/ca-population.html")
cat("  Done\n")


# =====================================================
# CANADA LANGUAGE & RELIGION
# =====================================================
cat("Building ca-langrelig...\n")

lang <- read.csv(file.path(DATA_DIR, "language/persian_language_patterns_FINAL.csv"))
relig <- read.csv(file.path(DATA_DIR, "religion/religion_dashboard_ready.csv"))

# --- Language: Horizontal 100% stacked bars by generation ---
# Standardize category names for both generations
lang <- lang %>% mutate(short_cat = case_when(
  persian_status == "Persian mother tongue, Persian home" ~ "Persian (mother tongue), Persian (at home)",
  persian_status == "Persian mother tongue, English/French home" ~ "Persian (mother tongue), Eng/Fr (at home)",
  persian_status == "Iranian minority mother tongue, same home" ~ "Minority (mother tongue), same (at home)",
  persian_status == "Iranian minority mother tongue, English/French home" ~ "Minority (mother tongue), Eng/Fr (at home)",
  persian_status == "Iranian minority mother tongue, various home" ~ "Minority (mother tongue), Eng/Fr (at home)",
  persian_status == "English mother tongue, English home" ~ "English (mother tongue), English (at home)",
  persian_status == "Other languages" ~ "Other languages",
  TRUE ~ persian_status
))

lang_cats <- c("Persian (mother tongue), Persian (at home)", "Persian (mother tongue), Eng/Fr (at home)",
               "Minority (mother tongue), same (at home)", "Minority (mother tongue), Eng/Fr (at home)",
               "English (mother tongue), English (at home)", "Other languages")
lang_colors <- c("Persian (mother tongue), Persian (at home)" = "#2d6a4f",
                 "Persian (mother tongue), Eng/Fr (at home)" = "#74a892",
                 "Minority (mother tongue), same (at home)" = "#8b6c42",
                 "Minority (mother tongue), Eng/Fr (at home)" = "#c4a96a",
                 "English (mother tongue), English (at home)" = "#6c757d",
                 "Other languages" = "#b0b0b0")

lang_gen_levels <- c("1st Generation", "2nd+ Generation")

lang1 <- lang %>% filter(generation == "First generation") %>%
  mutate(gen_label = "1st Generation")
lang2 <- lang %>% filter(generation == "Second+ generation") %>%
  mutate(gen_label = "2nd+ Generation")

# Merge minority MT categories for 2nd gen (it has "Iranian minority mother tongues" instead)
lang2 <- lang2 %>% mutate(short_cat = case_when(
  grepl("^Iranian minority moth", persian_status) ~ "Minority (mother tongue), Eng/Fr (at home)",
  TRUE ~ short_cat
))

lang_all <- bind_rows(lang1, lang2) %>%
  filter(short_cat %in% lang_cats) %>%
  group_by(gen_label, short_cat) %>%
  summarize(percentage = sum(percentage, na.rm = TRUE), .groups = "drop")

# Renormalize to 100% within each generation
lang_all <- lang_all %>%
  group_by(gen_label) %>%
  mutate(percentage = round(percentage / sum(percentage) * 100, 1)) %>%
  ungroup()

lang_all$gen_label <- factor(lang_all$gen_label, levels = lang_gen_levels)
lang_all$short_cat <- factor(lang_all$short_cat, levels = lang_cats)

p_lang <- plot_ly()
for (cat_name in lang_cats) {
  sub <- lang_all %>% filter(short_cat == cat_name)
  if (nrow(sub) > 0) {
    hover_texts <- sprintf("<b>%s</b><br>%s<br>%.1f%%", cat_name, sub$gen_label, sub$percentage)
    p_lang <- p_lang %>% add_bars(data = sub, y = ~gen_label, x = ~percentage, name = cat_name,
      marker = list(color = lang_colors[cat_name]), textposition = "none",
      hovertext = hover_texts, hoverinfo = "text",
      legendgroup = cat_name, showlegend = FALSE, orientation = "h")
  }
}
p_lang <- p_lang %>% layout(
  barmode = "stack",
  hoverlabel = list(showarrow = FALSE),
  title = list(text = "<b>Language of Iranian-Canadians<br>by Generation</b>",
    font = list(size = 16, family = "Montserrat")),
  xaxis = list(title = "", ticksuffix = "%", range = c(0, 105)),
  yaxis = list(title = "", categoryorder = "array", categoryarray = rev(lang_gen_levels),
    ticklabelstandoff = 6),
  margin = list(t = 55, b = 40, l = 120), showlegend = FALSE,
  plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

lang_leg <- make_html_legend(lang_colors, break_after = 3)

# --- Religion: Horizontal 100% stacked bars by generation ---
relig_cats <- c("Muslim", "No religion/Secular", "Christian", "Other religions", "Not stated")
relig_colors <- c("Muslim" = "#1a4e72",
                  "No religion/Secular" = "#d4a943",
                  "Christian" = "#4a8c6f",
                  "Other religions" = "#7b5ea7",
                  "Not stated" = "#b0b0b0")

relig <- relig %>% mutate(short_cat = case_when(
  grepl("^Muslim", religion_category) ~ "Muslim",
  grepl("^No religion", religion_category) ~ "No religion/Secular",
  grepl("^Christian", religion_category) ~ "Christian",
  grepl("^Other", religion_category) ~ "Other religions",
  grepl("^All other", religion_category) ~ "Not stated",
  TRUE ~ religion_category
))

relig1 <- relig %>% filter(generation == "First generation") %>% mutate(gen_label = "1st Generation")
relig2 <- relig %>% filter(generation == "Second+ generation") %>% mutate(gen_label = "2nd+ Generation")

relig_all <- bind_rows(relig1, relig2) %>%
  filter(short_cat %in% relig_cats) %>%
  group_by(gen_label, short_cat) %>%
  summarize(percentage = sum(percentage, na.rm = TRUE), .groups = "drop")

relig_all <- relig_all %>%
  group_by(gen_label) %>%
  mutate(percentage = round(percentage / sum(percentage) * 100, 1)) %>%
  ungroup()

relig_all$gen_label <- factor(relig_all$gen_label, levels = lang_gen_levels)
relig_all$short_cat <- factor(relig_all$short_cat, levels = relig_cats)

p_relig <- plot_ly()
for (cat_name in relig_cats) {
  sub <- relig_all %>% filter(short_cat == cat_name)
  if (nrow(sub) > 0) {
    hover_texts <- sprintf("<b>%s</b><br>%s<br>%.1f%%", cat_name, sub$gen_label, sub$percentage)
    p_relig <- p_relig %>% add_bars(data = sub, y = ~gen_label, x = ~percentage, name = cat_name,
      marker = list(color = relig_colors[cat_name]), textposition = "none",
      hovertext = hover_texts, hoverinfo = "text",
      legendgroup = cat_name, showlegend = FALSE, orientation = "h")
  }
}
p_relig <- p_relig %>% layout(
  barmode = "stack",
  hoverlabel = list(showarrow = FALSE),
  title = list(text = "<b>Religious Identification<br>of Iranian-Canadians by Generation</b>",
    font = list(size = 16, family = "Montserrat")),
  xaxis = list(title = "", ticksuffix = "%", range = c(0, 105)),
  yaxis = list(title = "", categoryorder = "array", categoryarray = rev(lang_gen_levels),
    ticklabelstandoff = 6),
  margin = list(t = 55, b = 40, l = 120), showlegend = FALSE,
  plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

relig_leg <- make_html_legend(relig_colors, break_after = 3)

# --- Factoid cards (4-card grid, same pattern as ca-work) -------------------
# card4() helper is shared with ca-work below.
card4 <- function(cls, big, sentence, note) {
  sprintf('<div class="text-card %s" style="text-align:center;">
    <div style="font-size:32px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:14px; font-weight:500; color:#333; margin-top:10px; line-height:1.45;">%s</div>
    <div style="font-size:12.5px; color:#555; margin-top:10px; line-height:1.5;">%s</div>
  </div>', cls, big, sentence, note)
}

lang_share <- function(gen, cat) {
  v <- lang_all$percentage[lang_all$gen_label == gen & lang_all$short_cat == cat]
  if (length(v) == 0) 0 else round(sum(v))
}
relig_share <- function(gen, cat) {
  v <- relig_all$percentage[relig_all$gen_label == gen & relig_all$short_cat == cat]
  if (length(v) == 0) 0 else round(sum(v))
}

# Language shares: "Persian at home" = mother-tongue-Persian + home-Persian.
# Shift to Eng/Fr = mother-tongue-Persian shifted to Eng/Fr home + English MT/English home.
g1_persian   <- lang_share("1st Generation",  "Persian (mother tongue), Persian (at home)")
g1_engfr     <- lang_share("1st Generation",  "Persian (mother tongue), Eng/Fr (at home)") +
                lang_share("1st Generation",  "English (mother tongue), English (at home)")
g2_persian   <- lang_share("2nd+ Generation", "Persian (mother tongue), Persian (at home)")
g2_engfr     <- lang_share("2nd+ Generation", "Persian (mother tongue), Eng/Fr (at home)") +
                lang_share("2nd+ Generation", "English (mother tongue), English (at home)")

# Religion shares
g1_muslim     <- relig_share("1st Generation",  "Muslim")
g1_noreligion <- relig_share("1st Generation",  "No religion/Secular")
g2_muslim     <- relig_share("2nd+ Generation", "Muslim")
g2_noreligion <- relig_share("2nd+ Generation", "No religion/Secular")
g2_christian  <- relig_share("2nd+ Generation", "Christian")

writeLines(page_template("Canada: Language & Religion", paste0(
  '<div class="page-content-4">',
  card4("p4-t1", sprintf("%d%%", g1_persian),
    "of first-generation Iranian-Canadians still speak Persian at home.",
    sprintf("Another %d%% have shifted to English or French at home.", g1_engfr)),
  card4("p4-t2", sprintf("%d%%", g2_persian),
    "of second-generation Iranian-Canadians still speak Persian at home.",
    sprintf("English or French at home rises to %d%%.", g2_engfr)),
  card4("p4-t3", sprintf("%d%%", g1_muslim),
    "of first-generation Iranian-Canadians identify as Muslim.",
    sprintf("Another %d%% report no religion or a secular identity.", g1_noreligion)),
  card4("p4-t4", sprintf("%d%%", g2_noreligion),
    "of second-generation Iranian-Canadians report no religion or a secular identity.",
    sprintf("Muslim %d%%, Christian %d%%.", g2_muslim, g2_christian)),
  '<div class="chart-card p4-c1">', plotly_div("lang", plotly_to_json(p_lang), "300px", source = PUMF_SOURCE, legend_html = lang_leg, highlight_hover = TRUE), '</div>',
  '<div class="chart-card p4-c2">', plotly_div("relig", plotly_to_json(p_relig), "300px", source = PUMF_SOURCE, legend_html = relig_leg, highlight_hover = TRUE), '</div>',
  '</div>'
), has_tabs = FALSE), "docs/pages/ca-langrelig.html")
cat("  Done\n")


# =====================================================
# CANADA IMMIGRATION & CITIZENSHIP
# =====================================================
cat("Building ca-immigration...\n")

immig_annual <- read.csv(file.path(DATA_DIR, "immigration/immigration_annual.csv"))
cit <- read.csv(file.path(DATA_DIR, "citizenship/citizenship_status.csv"))
immig_cat <- read.csv(file.path(DATA_DIR, "immigration/iranian_immigration_category_trends.csv"),
  check.names = FALSE)

# Annual immigration chart: wide bars for pre-1995 periods, narrow bars for annual
immig_annual <- immig_annual %>% arrange(year)

# Cumulative line (based on period_total, not per-year average)
immig_annual$cumulative <- cumsum(immig_annual$period_total / immig_annual$period_years)
# Recalculate: cumulative of actual counts
immig_annual$cumulative <- cumsum(immig_annual$count)
immig_annual$cum_pct <- round(immig_annual$cumulative / max(immig_annual$cumulative) * 100, 1)

# Bar widths: 5 years wide for periods, 1 year for annual
immig_annual$bar_width <- ifelse(immig_annual$is_period, 4.5, 0.8)

# Hover text
immig_annual$hover <- ifelse(immig_annual$is_period,
  sprintf("<b>%s</b><br>Arrivals: %s (%s/year avg)<br>Cumulative: %.1f%%",
    immig_annual$period_label,
    format(round(immig_annual$period_total), big.mark = ","),
    format(round(immig_annual$count), big.mark = ","),
    immig_annual$cum_pct),
  sprintf("<b>%d</b><br>Arrivals: %s<br>Cumulative: %.1f%%",
    immig_annual$year,
    format(round(immig_annual$count), big.mark = ","),
    immig_annual$cum_pct))

# Lighter color for period bars, darker for annual
immig_annual$bar_color <- ifelse(immig_annual$is_period, "#5a9bd5", "#2774AE")

p_ca_immig <- plot_ly() %>%
  add_bars(data = immig_annual, x = ~year, y = ~count,
    width = ~bar_width, marker = list(color = ~bar_color),
    text = ~hover, hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  add_trace(data = immig_annual, x = ~year,
    y = ~cumulative / max(cumulative) * max(count),
    type = "scatter", mode = "lines",
    yaxis = "y2", line = list(color = "lightblue", width = 2),
    text = ~sprintf("<b>By %d:</b><br>%s arrivals<br>(%.1f%% of total)",
      year, format(round(cumulative), big.mark = ","), cum_pct),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iranian Migration to Canada:<br>Arrivals and Cumulative Trends</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 10), dtick = 5,
      range = c(1948, 2022)),
    yaxis = list(title = "", tickformat = ","),
    yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
      range = c(0, max(immig_annual$count) * 1.05),
      tickvals = round(seq(0, max(immig_annual$count), length.out = 5)),
      ticktext = c("0%", "25%", "50%", "75%", "100%"),
      tickfont = list(size = 11)),
    margin = list(t = 65, b = 50, r = 60),
    plot_bgcolor = "white", paper_bgcolor = "white",
    annotations = list(
      list(text = "Note: Pre-1995 values show 5-year period arrivals, averaged per year.",
        x = 0.5, y = -0.15, xref = "paper", yref = "paper", showarrow = FALSE,
        font = list(size = 9, color = "#888"), xanchor = "center"))
  ) %>% config(displayModeBar = FALSE)

# Citizenship status - ALL generations combined
cit1 <- cit %>%
  group_by(status) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentage = round(count / sum(count) * 100, 1))
cit1$short_label <- case_when(
  grepl("by birth", cit1$status) ~ "Born in\nCanada",
  grepl("naturalization", cit1$status) ~ "Naturalized\ncitizen",
  grepl("Not a", cit1$status) ~ "Not a\ncitizen",
  TRUE ~ cit1$status
)
cit1_order <- c("Naturalized\ncitizen", "Born in\nCanada", "Not a\ncitizen")
cit1$short_label <- factor(cit1$short_label, levels = cit1_order)

p_ca_cit <- plot_ly(data = cit1, x = ~short_label, y = ~count, type = "bar",
    marker = list(color = c("#2774AE", "#5a9bd5", "#e07b54")),
    text = ~sprintf("<b>%s</b><br>%s (%.1f%%)", status, format(round(count), big.mark = ","), percentage),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Citizenship Status:<br>All Iranian-Canadians</b>", font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 10),
      categoryorder = "array", categoryarray = cit1_order),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 55, b = 60), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

# Immigration type chart (for tab)
# iranian_immigration_category_trends.csv has arrival_period x 3 immigration categories (percentages)
# Build 100% stacked bar with 4 period groups and 3 categories

immig_cat_long <- immig_cat %>%
  select(arrival_period, starts_with("percentage_")) %>%
  tidyr::pivot_longer(cols = -arrival_period, names_to = "category", values_to = "pct") %>%
  mutate(category = gsub("^percentage_", "", category))

immtype_cats <- c("Economic immigrants", "Family-sponsored immigrants", "Refugees")
immtype_colors <- c("Economic immigrants" = "#1a4e72",
                    "Family-sponsored immigrants" = "#4a8c6f",
                    "Refugees" = "#d4a943")

period_order_cat <- c("1990-1999", "2000-2009", "2010-2019", "2020-2021")
immig_cat_long$arrival_period <- factor(immig_cat_long$arrival_period, levels = period_order_cat)
immig_cat_long$category <- factor(immig_cat_long$category, levels = immtype_cats)

p_ca_immtype <- plot_ly()
for (cat_name in immtype_cats) {
  sub <- immig_cat_long %>% filter(category == cat_name)
  if (nrow(sub) > 0) {
    hover_texts <- sprintf("<b>%s</b><br>%s<br>%.1f%%", cat_name, sub$arrival_period, sub$pct)
    p_ca_immtype <- p_ca_immtype %>% add_bars(data = sub, x = ~arrival_period, y = ~pct, name = cat_name,
      marker = list(color = immtype_colors[cat_name]), textposition = "none",
      hovertext = hover_texts, hoverinfo = "text",
      legendgroup = cat_name, showlegend = FALSE)
  }
}
p_ca_immtype <- p_ca_immtype %>% layout(
  barmode = "stack",
  title = list(text = "<b>Iranian Immigration to Canada<br>by Type: Post-1990 Arrivals</b>",
    font = list(size = 16, family = "Montserrat")),
  xaxis = list(title = "", tickfont = list(size = 10),
    categoryorder = "array", categoryarray = period_order_cat),
  yaxis = list(title = "", ticksuffix = "%", range = c(0, 105)),
  margin = list(t = 55, b = 60), showlegend = FALSE,
  plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

immtype_leg <- make_html_legend(immtype_colors)

# Build citizenship chart div with tab switching
cit_div <- paste0(
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'cit-status\',this,\'cit-group\')">Citizenship Status</button>',
  '<button class="tab-btn" onclick="switchTab(\'cit-immtype\',this,\'cit-group\')">Immigration Type</button>',
  '</div>',
  '<div id="cit-status" class="tab-panel active" data-group="cit-group">',
  plotly_div("ca-cit", plotly_to_json(p_ca_cit), "400px", source = PUMF_SOURCE),
  '</div>',
  '<div id="cit-immtype" class="tab-panel" data-group="cit-group">',
  plotly_div("ca-immtype", plotly_to_json(p_ca_immtype), "400px", source = PUMF_SOURCE, legend_html = immtype_leg, highlight_hover = TRUE),
  '</div>'
)

# Weighted median year of arrival for first-generation Iranian-Canadians,
# computed dynamically from the immigration_annual.csv we just loaded as
# `immig`. Previously the text hard-coded "over 40% after 2010" which sat
# uncomfortably close to the 40% threshold (actual ~41%) and has the wrong
# natural anchor — the real "half arrived in year X or later" mark is
# several years earlier than 2010.
im_ordered <- immig_annual[order(immig_annual$year), ]
im_total <- sum(im_ordered$period_total, na.rm = TRUE)
im_ordered$cum <- cumsum(im_ordered$period_total) / im_total
im_median_year <- im_ordered$year[which(im_ordered$cum >= 0.5)[1]]
im_1980s_share <- round(
  sum(immig_annual$period_total[immig_annual$year >= 1980 & immig_annual$year < 1990], na.rm = TRUE) /
  im_total * 100)

# Citizenship shares — computed from citizenship_status.csv to match what
# the chart renders (the chart title is "All Iranian-Canadians" so the
# denominator is the full population, not first-gen). Previously the text
# said "About 60% of FIRST-GENERATION Iranian-Canadians are naturalized"
# which was a denominator mismatch — 60% is the total-population share,
# not the first-gen-specific share (which is closer to 69%).
cit_naturalized_pct <- round(
  cit$percentage[grepl("naturalization", cit$status)][1])
cit_not_citizen_pct <- round(
  cit$percentage[grepl("Not a", cit$status)][1])

writeLines(page_template("Canada: Immigration & Citizenship", paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">Half of first-generation Iranian-Canadians arrived in %d or later.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Only %d%% arrived during the 1980s.</div>
  </div>', im_median_year, im_median_year, im_1980s_share),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iranian-Canadians are naturalized Canadian citizens.</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%d%% have not yet obtained citizenship</li>
      <li>Economic immigration has been the most common pathway since the 1990s</li>
    </ul>
  </div>', cit_naturalized_pct, cit_not_citizen_pct),
  '<div class="chart-card pc1">', plotly_div("ca-immig", plotly_to_json(p_ca_immig), "430px", source = PUMF_SRC_IMMIG), '</div>',
  '<div class="chart-card pc2">', cit_div, '</div>',
  '</div>'
), has_tabs = TRUE), "docs/pages/ca-immigration.html")
cat("  Done\n")


# =====================================================
# CANADA EDUCATION (butterfly)
# =====================================================
cat("Building ca-education...\n")

ed1 <- read.csv(file.path(DATA_DIR, "education/education_first_gen_30plus_FINAL.csv"))
fos <- read.csv(file.path(DATA_DIR, "education/field_of_study_DASHBOARD_FINAL.csv"))
fos$field_category[fos$field_category == "STEM"] <- "Science, Technology, Engineering & Math"

# Education butterfly helper (same pattern as build_all.R)
make_ca_educ_butterfly <- function(df, gen_label, id_prefix, height = "450px", source = PUMF_SRC_EDUC_1G) {
  educ_levels <- c("Less than BA degree", "BA degree", "Graduate degree")
  colors <- c("Less than BA degree" = "#1a4e72", "BA degree" = "#2774AE", "Graduate degree" = "#8bbdde")

  d_m <- df %>% filter(gender_label == "Men") %>%
    mutate(education_level = factor(education_level, levels = educ_levels))
  d_f <- df %>% filter(gender_label == "Women") %>%
    mutate(education_level = factor(education_level, levels = educ_levels))

  age_levels <- sort(unique(df$age_cohort))

  p_men <- plot_ly()
  for (ed in educ_levels) {
    sm <- d_m %>% filter(education_level == ed)
    if (nrow(sm) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Men, %s<br>%.1f%%", ed, gen_label, sm$age_cohort, sm$percentage)
      p_men <- p_men %>% add_bars(data = sm, y = ~age_cohort, x = ~percentage, name = ed,
        marker = list(color = colors[ed]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = ed, showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (ed in educ_levels) {
    sf <- d_f %>% filter(education_level == ed)
    if (nrow(sf) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Women, %s<br>%.1f%%", ed, gen_label, sf$age_cohort, sf$percentage)
      p_women <- p_women %>% add_bars(data = sf, y = ~age_cohort, x = ~percentage, name = ed,
        marker = list(color = colors[ed]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = ed, showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = "Men", x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = "Women", x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg <- make_html_legend(colors, break_after = 3)
  plotly_div(id_prefix, plotly_to_json(p), height, source = source, legend_html = leg, highlight_hover = TRUE)
}

# Field of study butterfly: single row "Ages 25+" per gender, 5 field categories
make_fos_butterfly <- function(fos_data, gen_label, id_prefix, height = "350px", source = PUMF_SRC_FOS_1G) {
  fos1 <- fos_data %>% filter(generation == "First-Generation")

  field_cats <- c("Science, Technology, Engineering & Math", "Business & Management", "Health", "Social Sciences & Humanities", "Other")
  colors <- c("Science, Technology, Engineering & Math" = "#1a4e72", "Business & Management" = "#d4a943",
              "Health" = "#4a8c6f", "Social Sciences & Humanities" = "#c4793a",
              "Other" = "#b0b0b0")

  d_m <- fos1 %>% filter(gender_label == "Men")
  d_f <- fos1 %>% filter(gender_label == "Women")

  y_levels <- c("Ages 25+")

  p_men <- plot_ly()
  for (fc in field_cats) {
    sm <- d_m %>% filter(field_category == fc)
    if (nrow(sm) > 0) {
      sm$y_label <- "Ages 25+"
      hover_texts <- sprintf("<b>%s</b><br>%s, Men<br>%.1f%%", fc, gen_label, sm$percentage)
      p_men <- p_men %>% add_bars(data = sm, y = ~y_label, x = ~percentage, name = fc,
        marker = list(color = colors[fc]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = fc, showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = y_levels))

  p_women <- plot_ly()
  for (fc in field_cats) {
    sf <- d_f %>% filter(field_category == fc)
    if (nrow(sf) > 0) {
      sf$y_label <- "Ages 25+"
      hover_texts <- sprintf("<b>%s</b><br>%s, Women<br>%.1f%%", fc, gen_label, sf$percentage)
      p_women <- p_women %>% add_bars(data = sf, y = ~y_label, x = ~percentage, name = fc,
        marker = list(color = colors[fc]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = fc, showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = y_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = "Men", x = 0.22, y = 1.05, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = "Women", x = 0.78, y = 1.05, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 40, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg <- make_html_legend(colors, break_after = 2)
  plotly_div(id_prefix, plotly_to_json(p), height, source = source, legend_html = leg, highlight_hover = TRUE)
}

# Dynamic shares for the two text cards
grad_pct <- function(cohort, gender) {
  v <- ed1$percentage[ed1$age_cohort == cohort & ed1$gender_label == gender & ed1$education_level == "Graduate degree"]
  if (length(v) == 0) NA_real_ else round(v[1])
}
ed_w_30 <- grad_pct("30-34", "Women")
ed_m_30 <- grad_pct("30-34", "Men")
ed_m_55 <- grad_pct("55-64", "Men")
ed_w_55 <- grad_pct("55-64", "Women")
fos_pct <- function(gender, field) {
  v <- fos$percentage[fos$gender_label == gender & fos$field_category == field & fos$generation == "First-Generation"]
  if (length(v) == 0) NA_real_ else round(v[1])
}
fos_m_stem <- fos_pct("Men", "Science, Technology, Engineering & Math")
fos_w_stem <- fos_pct("Women", "Science, Technology, Engineering & Math")
fos_w_ssh  <- fos_pct("Women", "Social Sciences & Humanities")
fos_w_hlth <- fos_pct("Women", "Health")

writeLines(page_template("Canada: Education", paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian-Canadian women aged 30&ndash;34 hold a graduate degree &mdash; slightly ahead of men (%d%%).</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Among those 55&ndash;64, men are more likely to hold graduate degrees (%d%% vs %d%%).</div>
  </div>', ed_w_30, ed_m_30, ed_m_55, ed_w_55),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian-Canadian men studied STEM fields &mdash; vs %d%% of women.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Women are more concentrated in social sciences &amp; humanities (%d%%) and health (%d%%).</div>
  </div>', fos_m_stem, fos_w_stem, fos_w_ssh, fos_w_hlth),
  '<div class="chart-card pc1">',
  '<div class="section-title">Educational Attainment of Iranian-Canadians: First Generation</div>',
  make_ca_educ_butterfly(ed1, "1st Gen", "ca-ed1"),
  '</div>',
  '<div class="chart-card pc2">',
  '<div class="section-title">Fields of Study of Iranian-Canadians: First Generation</div>',
  make_fos_butterfly(fos, "1st Gen", "ca-fos", "300px"),
  '</div>',
  '</div>'
)), "docs/pages/ca-education.html")
cat("  Done\n")


# =====================================================
# CANADA WORK (butterfly with tabs)
# =====================================================
cat("Building ca-work...\n")

work <- read.csv(file.path(DATA_DIR, "work/industry_sectors_dashboard.csv"))
emp <- read.csv(file.path(DATA_DIR, "work/employment_categories_dashboard.csv"))

# Employment category butterfly helper (Private/Public/Self-employed/No work)
make_employment_butterfly <- function(df, gen_val, gen_label, id_prefix, height = "450px", source = PUMF_SRC_WORK_1G) {
  d <- df %>% filter(generation == gen_val)

  emp_cats <- c("Private sector", "Public sector", "Self-employed", "No work in last 5 years")
  colors <- c("Private sector" = "#5a9bd5",
              "Public sector" = "#4a8c6f",
              "Self-employed" = "#d4a943",
              "No work in last 5 years" = "#b0b0b0")

  age_levels <- sort(unique(d$age_cohort))

  d_m <- d %>% filter(gender_label == "Men")
  d_f <- d %>% filter(gender_label == "Women")

  p_men <- plot_ly()
  for (cat_name in emp_cats) {
    sm <- d_m %>% filter(employment_category == cat_name)
    if (nrow(sm) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Men, %s<br>%.1f%%", cat_name, gen_label, sm$age_cohort, sm$percentage)
      p_men <- p_men %>% add_bars(data = sm, y = ~age_cohort, x = ~percentage, name = cat_name,
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_name, showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (cat_name in emp_cats) {
    sf <- d_f %>% filter(employment_category == cat_name)
    if (nrow(sf) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Women, %s<br>%.1f%%", cat_name, gen_label, sf$age_cohort, sf$percentage)
      p_women <- p_women %>% add_bars(data = sf, y = ~age_cohort, x = ~percentage, name = cat_name,
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_name, showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = "Men", x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = "Women", x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg <- make_html_legend(colors)
  plotly_div(id_prefix, plotly_to_json(p), height, source = source, legend_html = leg, highlight_hover = TRUE)
}

# Industry sector butterfly helper
make_industry_butterfly <- function(df, gen_val, gen_label, id_prefix, height = "450px", source = PUMF_SRC_WORK_1G) {
  d <- df %>% filter(generation == gen_val)

  ind_cats <- c("Professional & Technical", "Health & Education", "Trade & Services",
                "Manufacturing & Construction", "Public & Other")
  colors <- c("Professional & Technical" = "#1a4e72",
              "Health & Education" = "#4a8c6f",
              "Trade & Services" = "#c4793a",
              "Manufacturing & Construction" = "#d4a943",
              "Public & Other" = "#b0b0b0")

  age_levels <- sort(unique(d$age_cohort))

  d_m <- d %>% filter(gender_label == "Men")
  d_f <- d %>% filter(gender_label == "Women")

  p_men <- plot_ly()
  for (cat_name in ind_cats) {
    sm <- d_m %>% filter(industry_category == cat_name)
    if (nrow(sm) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Men, %s<br>%.1f%%", cat_name, gen_label, sm$age_cohort, sm$percentage)
      p_men <- p_men %>% add_bars(data = sm, y = ~age_cohort, x = ~percentage, name = cat_name,
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_name, showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (cat_name in ind_cats) {
    sf <- d_f %>% filter(industry_category == cat_name)
    if (nrow(sf) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Women, %s<br>%.1f%%", cat_name, gen_label, sf$age_cohort, sf$percentage)
      p_women <- p_women %>% add_bars(data = sf, y = ~age_cohort, x = ~percentage, name = cat_name,
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_name, showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = "Men", x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = "Women", x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg <- make_html_legend(colors, break_after = 3)
  plotly_div(id_prefix, plotly_to_json(p), height, source = source, legend_html = leg, highlight_hover = TRUE)
}

# Dynamic headline shares for the 2x2 work cards
work_share <- function(gen, gender, sector, age = NULL) {
  d <- work %>% filter(generation == gen, gender_label == gender, industry_category == sector)
  if (!is.null(age)) d <- d %>% filter(age_cohort == age)
  if (nrow(d) == 0) return(NA_real_)
  round(mean(d$percentage))
}
g1m_pt  <- work_share("First-Generation",  "Men",   "Professional & Technical",     "35-44")
g1m_mc  <- work_share("First-Generation",  "Men",   "Manufacturing & Construction", "35-44")
g1w_ts  <- work_share("First-Generation",  "Women", "Trade & Services",             "35-44")
g1w_pt  <- work_share("First-Generation",  "Women", "Professional & Technical",     "35-44")
g1w_he  <- work_share("First-Generation",  "Women", "Health & Education",           "35-44")
g2m_pt  <- work_share("Second-Generation", "Men",   "Professional & Technical")
g2m_ts  <- work_share("Second-Generation", "Men",   "Trade & Services")
g2w_pt  <- work_share("Second-Generation", "Women", "Professional & Technical")
g2w_he  <- work_share("Second-Generation", "Women", "Health & Education")

# card4() helper is defined above in the ca-langrelig section.

writeLines(page_template("Canada: Work", paste0(
  '<div class="page-content-4">',
  card4("p4-t1", sprintf("%d%%", g1m_pt),
    sprintf("of first-generation men aged 35&ndash;44 work in Professional &amp; Technical fields."),
    sprintf("Manufacturing &amp; Construction ties at %d%%.", g1m_mc)),
  card4("p4-t2", sprintf("%d%%", g1w_ts),
    sprintf("of first-generation women aged 35&ndash;44 work in Trade &amp; Services &mdash; the largest sector."),
    sprintf("Professional &amp; Technical (%d%%) and Health &amp; Education (%d%%) are close behind.", g1w_pt, g1w_he)),
  card4("p4-t3", sprintf("%d%%", g2m_pt),
    "of second-generation men work in Professional &amp; Technical fields.",
    sprintf("Trade &amp; Services follows at %d%%.", g2m_ts)),
  card4("p4-t4", sprintf("%d%%", g2w_pt),
    "of second-generation women work in Professional &amp; Technical fields.",
    sprintf("Health &amp; Education (%d%%) is the second-largest sector.", g2w_he)),
  # First-gen: tabs for Employment Categories / Industry Sectors
  '<div class="chart-card p4-c1">',
  '<div class="section-title">Work of Iranian-Canadians: First Generation</div>',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'wk1-emp\',this,\'wk1\')">Employment Categories</button>',
  '<button class="tab-btn" onclick="switchTab(\'wk1-ind\',this,\'wk1\')">Industry Sectors</button>',
  '</div>',
  '<div id="wk1-emp" class="tab-panel active" data-group="wk1">',
  make_employment_butterfly(emp, "First-Generation", "1st Gen", "ca-wk1-emp"),
  '</div>',
  '<div id="wk1-ind" class="tab-panel" data-group="wk1">',
  make_industry_butterfly(work, "First-Generation", "1st Gen", "ca-wk1-ind"),
  '</div>',
  '</div>',
  # Second-gen: tabs for Employment Categories / Industry Sectors
  '<div class="chart-card p4-c2">',
  '<div class="section-title">Work of Iranian-Canadians: Second Generation</div>',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'wk2-emp\',this,\'wk2\')">Employment Categories</button>',
  '<button class="tab-btn" onclick="switchTab(\'wk2-ind\',this,\'wk2\')">Industry Sectors</button>',
  '</div>',
  '<div id="wk2-emp" class="tab-panel active" data-group="wk2">',
  make_employment_butterfly(emp, "Second-Generation", "2nd Gen", "ca-wk2-emp", "300px", source = PUMF_SRC_WORK_2G),
  '</div>',
  '<div id="wk2-ind" class="tab-panel" data-group="wk2">',
  make_industry_butterfly(work, "Second-Generation", "2nd Gen", "ca-wk2-ind", "300px", source = PUMF_SRC_WORK_2G),
  '</div>',
  '</div>',
  '</div>'
), has_tabs = TRUE), "docs/pages/ca-work.html")
cat("  Done\n")


# =====================================================
# CANADA INCOME
# =====================================================
cat("Building ca-income...\n")

inc <- read.csv(file.path(DATA_DIR, "work/income_distribution_FINAL.csv"))
inc_age <- read.csv(file.path(DATA_DIR, "work/income_by_age_first_gen.csv"))

# Left: Decile chart (first gen, ages 25-54)
inc1 <- inc %>% filter(generation == "First")

# Sort by decile number
inc1$sort_key <- as.numeric(gsub("\\D", "", inc1$income_category))
inc1 <- inc1 %>% arrange(sort_key)
decile_labels <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
inc1$short_label <- decile_labels

p_inc_decile <- plot_ly(data = inc1, x = ~short_label, y = ~percentage, type = "scatter",
    mode = "markers+lines",
    marker = list(color = "#4A90D9", size = 8),
    line = list(color = "#4A90D9", width = 1),
    text = ~sprintf("<b>%s</b><br>Households: %s<br>%.1f%%",
      income_category, format(round(households), big.mark = ","), percentage),
    hoverinfo = "text", textposition = "none") %>%
  add_trace(y = ~percentage, type = "scatter", mode = "none",
    fill = "tozeroy", fillcolor = "rgba(173,216,230,0.3)",
    marker = list(size = 0, opacity = 0),
    hoverinfo = "skip", showlegend = FALSE) %>%
  add_trace(x = inc1$short_label, y = rep(10, nrow(inc1)), type = "scatter",
    mode = "lines", line = list(color = "#cc0000", width = 1.5, dash = "dot"),
    marker = list(size = 0, opacity = 0),
    hoverinfo = "skip", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Position in Canadian<br>Household Income Distribution:<br>First Generation (Ages 25-54)</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "Income Decile (Lowest to Highest)", titlefont = list(size = 11),
      categoryorder = "array", categoryarray = inc1$short_label),
    yaxis = list(title = "", ticksuffix = "%", range = c(0, max(inc1$percentage) + 3)),
    showlegend = FALSE,
    margin = list(t = 75, b = 50),
    plot_bgcolor = "white", paper_bgcolor = "white",
    annotations = list(
      list(text = "10% =<br>national<br>baseline", x = inc1$short_label[nrow(inc1) - 2], y = 12,
        showarrow = FALSE, font = list(size = 8, color = "#cc0000"), xanchor = "center"))
  ) %>% config(displayModeBar = FALSE)

# Add share labels — below the dot, extra space near the 10% baseline
for (i in seq_len(nrow(inc1))) {
  val <- inc1$percentage[i]
  if (val >= 8 && val <= 12) {
    y_pos <- val - 3
  } else {
    y_pos <- val - 1.5
  }
  p_inc_decile <- p_inc_decile %>% add_annotations(
    x = inc1$short_label[i], y = y_pos,
    text = sprintf("%.1f%%", val),
    showarrow = FALSE, font = list(size = 10, color = "#4A90D9"))
}

# Right: Income Distribution by Age - horizontal 100% stacked bars
# income_by_age_first_gen.csv has 5 age groups x 5 income bands for first-gen

age_band_order <- c("Under CA$20k", "CA$20k-CA$40k", "CA$40k-CA$60k", "CA$60k-CA$100k", "CA$100k+")
age_band_colors <- c("Under CA$20k" = "#d4e6f1", "CA$20k-CA$40k" = "#8bbdde",
                      "CA$40k-CA$60k" = "#5a9bd5", "CA$60k-CA$100k" = "#2774AE",
                      "CA$100k+" = "#1a4e72")

age_levels_inc <- c("25-34", "35-44", "45-54", "55-64", "65-74")
inc_age$age_group <- factor(inc_age$age_group, levels = age_levels_inc)
inc_age$income_band <- factor(inc_age$income_band, levels = age_band_order)

p_inc_age <- plot_ly()
for (band in age_band_order) {
  sub <- inc_age %>% filter(income_band == band)
  if (nrow(sub) > 0) {
    hover_texts <- sprintf("<b>%s</b><br>Age %s<br>%.1f%%", band, sub$age_group, sub$pct)
    p_inc_age <- p_inc_age %>% add_bars(data = sub, y = ~age_group, x = ~pct, name = band,
      marker = list(color = age_band_colors[band]), textposition = "none",
      hovertext = hover_texts, hoverinfo = "text",
      legendgroup = band, showlegend = FALSE, orientation = "h")
  }
}
p_inc_age <- p_inc_age %>% layout(
  barmode = "stack",
  title = list(text = "<b>Personal Income by Age:<br>First-Generation Iranian-Canadians</b>",
    font = list(size = 16, family = "Montserrat")),
  xaxis = list(title = "", ticksuffix = "%", range = c(0, 105)),
  yaxis = list(title = "", categoryorder = "array", categoryarray = rev(age_levels_inc)),
  margin = list(t = 55, b = 40, l = 60), showlegend = FALSE,
  plot_bgcolor = "white", paper_bgcolor = "white"
) %>% config(displayModeBar = FALSE)

inc_age_leg <- make_html_legend(age_band_colors, break_after = 3)

# Dynamic shares for the income cards
ca_d1  <- round(inc1$percentage[inc1$income_category == "Decile 1"])
ca_d10 <- round(inc1$percentage[inc1$income_category == "Decile 10"])
ca_100k_peak <- round(max(inc_age$pct[inc_age$income_band == "CA$100k+"], na.rm = TRUE))
ca_100k_peak_age <- inc_age$age_group[inc_age$income_band == "CA$100k+"][
  which.max(inc_age$pct[inc_age$income_band == "CA$100k+"])]

writeLines(page_template("Canada: Income", paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian-Canadian households fall in the lowest Canadian income decile &mdash; nearly double the 10%% national baseline.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Only %d%% reach the top decile.</div>
  </div>', ca_d1, ca_d10),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian-Canadian earners aged %s make CA$100,000 or more &mdash; the peak age bracket for high earners.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Reports each individual&rsquo;s own pre-tax earnings by age. Household income, by contrast, sums every earner in a household into one total before ranking it against the national distribution.</div>
  </div>', ca_100k_peak, ca_100k_peak_age),
  '<div class="chart-card pc1">', plotly_div("ca-inc1", plotly_to_json(p_inc_decile), "500px", source = PUMF_SRC_INCOME), '</div>',
  '<div class="chart-card pc2">', plotly_div("ca-inc-age", plotly_to_json(p_inc_age), "400px", source = PUMF_SRC_INCOME_AGE, legend_html = inc_age_leg, highlight_hover = TRUE), '</div>',
  '</div>'
)), "docs/pages/ca-income.html")
cat("  Done\n")


cat("\nAll Canada pages built.\n")

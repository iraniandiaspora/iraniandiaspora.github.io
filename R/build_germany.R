# Build all Germany pages from Mikrozensus 2024 Iran extracts + BAMF/IW figures.
# Run from deployment repo root:
#   Rscript R/build_germany.R
#
# Input:  data/germany/*.csv (see R/germany_export/extract_mikrozensus.R)
#         data/germany/bundeslaender.geojson
# Output: docs/pages/de-*.html

library(plotly)
library(dplyr)
library(jsonlite)

DATA_DIR <- "data/germany"

# --- Helpers (canonical, copied from R/build_canada.R) ---
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
  b$x$data <- strip_internal_classes(b$x$data)
  b$x$layout <- strip_internal_classes(b$x$layout)
  if (is.null(b$x$layout$font)) b$x$layout$font <- list()
  b$x$layout$font$family <- "Montserrat, sans-serif"
  b$x$layout$hoverlabel <- list(
    bgcolor = "white", bordercolor = "#ccc",
    font = list(family = "Montserrat, sans-serif", size = 13, color = "#333"))
  list(data = toJSON(b$x$data, auto_unbox = TRUE),
       layout = toJSON(b$x$layout, auto_unbox = TRUE),
       config = toJSON(b$x$config, auto_unbox = TRUE))
}

plotly_div <- function(id, json, height = "500px", source = NULL, legend_html = NULL, highlight_hover = FALSE) {
  init_js <- sprintf('var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox",showTips:true});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  if (highlight_hover) {
    init_js <- paste0(init_js, sprintf('
var el=document.getElementById("%s");
function hlOn(lg){
  el.data.forEach(function(t,i){
    Plotly.restyle(el,{opacity:t.legendgroup===lg?1:0.15,"marker.line.width":t.legendgroup===lg?3:0,"marker.line.color":"#000"},[i]);
  });
  var wrap=el.closest(".chart-card");
  if(wrap){wrap.querySelectorAll("[data-lg]").forEach(function(s){s.style.opacity=s.getAttribute("data-lg")===lg?1:0.3;});}
}
function hlOff(){
  el.data.forEach(function(t,i){Plotly.restyle(el,{opacity:1,"marker.line.width":0},[i]);});
  var wrap=el.closest(".chart-card");
  if(wrap){wrap.querySelectorAll("[data-lg]").forEach(function(s){s.style.opacity=1;});}
}
el.__hlOn=hlOn;el.__hlOff=hlOff;
el.on("plotly_hover",function(d){hlOn(d.points[0].data.legendgroup);});
el.on("plotly_unhover",hlOff);
var _lastLg=null;
el.on("plotly_click",function(d){var lg=d.points[0].data.legendgroup;if(_lastLg===lg){hlOff();_lastLg=null;}else{hlOn(lg);_lastLg=lg;}});', id))
  }
  chart <- sprintf('<div id="%s" style="width:100%%;height:%s;touch-action:manipulation;"></div>\n<script>(function(){%s})();</script>',
    id, height, init_js)
  if (!is.null(legend_html)) {
    chart <- paste0(chart, '\n', legend_html)
  }
  if (!is.null(source)) {
    chart <- paste0(chart, sprintf('\n<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0; padding-right:2px;">%s</p>', source))
  }
  chart
}

make_html_legend <- function(colors, labels = names(colors), break_after = NULL) {
  items <- mapply(function(col, lab) {
    html_lab <- gsub("&", "&amp;", lab)
    sprintf('<span data-lg="%s" style="display:inline-flex; align-items:center; gap:4px; cursor:pointer; transition:opacity 0.2s;" onmouseenter="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOn)el.__hlOn(this.getAttribute(\'data-lg\'));" onmouseleave="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOff)el.__hlOff();"><span style="width:12px; height:12px; background:%s; border-radius:2px; display:inline-block;"></span> %s</span>',
      lab, col, html_lab)
  }, colors, labels, SIMPLIFY = TRUE)
  sprintf('<div style="display:flex; justify-content:center; flex-wrap:wrap; gap:6px 14px; font-size:12px; color:#444; margin:6px 0 2px; line-height:2;">%s</div>',
    paste(items, collapse = ""))
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
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }', tab_css, '
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .text-row-4 { grid-template-columns:1fr 1fr; }
  .text-row, .text-row-4 { order:1; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .headline .number { font-size:34px; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
  .measure-num { font-size:28px !important; }
}
@media (max-width:600px) {
  .text-row-4 { grid-template-columns:1fr !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .headline .number { font-size:28px; }
  .chart-card { padding:10px; }
  .measure-num { font-size:24px !important; }
}
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

# --- Source citation strings ---
MZ_LINK <- "<a href='https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Migration-Integration/Publikationen/Downloads-Migration/statistischer-bericht-migrationshintergrund-erst-2010220247005.html' target='_blank' style='color:#2774AE;'>Destatis</a>"
MZ_SOURCE <- paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024 Erstergebnisse<br>",
  "German household survey. Counts residents with migration background (first or second generation), Iran.")

blues <- c("#1a4e72", "#2774AE", "#5a9bd5", "#8bbdde", "#d4e6f1")

# --- Load all extracts ---
cat("Loading Mikrozensus extracts from ", DATA_DIR, "...\n", sep = "")
hl <- read.csv(file.path(DATA_DIR, "de_headline.csv"))
bund <- read.csv(file.path(DATA_DIR, "de_bundesland.csv"))
school <- read.csv(file.path(DATA_DIR, "de_school.csv"))
prof <- read.csv(file.path(DATA_DIR, "de_profqual.csv"))
emp <- read.csv(file.path(DATA_DIR, "de_employment.csv"))
lang <- read.csv(file.path(DATA_DIR, "de_language.csv"))
motive <- read.csv(file.path(DATA_DIR, "de_motive.csv"))
nat <- read.csv(file.path(DATA_DIR, "de_naturalization.csv"))
duration <- read.csv(file.path(DATA_DIR, "de_duration.csv"))
annual_arrivals <- read.csv(file.path(DATA_DIR, "de_annual_arrivals.csv"))

# Headline lookups (values are in thousands; convert to persons)
hl_total <- hl$value_thousands[hl$category == "total"] * 1000
hl_fg    <- hl$value_thousands[hl$category == "first_gen"] * 1000
hl_sg    <- hl$value_thousands[hl$category == "second_gen"] * 1000
hl_deu   <- hl$value_thousands[hl$category == "german_citizens"] * 1000
hl_for   <- hl$value_thousands[hl$category == "foreign_citizens"] * 1000


# =====================================================
# DE POPULATION (headline + matrix + bar + Bundesland map)
# =====================================================
cat("Building de-population...\n")

# Bundesland bar (all_gens), sorted descending.
# Suppressed states (Destatis rule: <5K) are kept in the chart with a grey
# fill and "Suppressed (<5K)" hover instead of being silently coerced to 0.
# The 10 visible states sum to ~294K vs the 319K headline; the ~25K gap
# lives in 6 suppressed Bundesländer and is labeled as such below the chart.
bund_all <- bund %>% filter(gen == "all_gens") %>%
  mutate(is_suppressed = is.na(value_k),
         value = ifelse(is_suppressed, 0, value_k * 1000)) %>%
  arrange(desc(value), is_suppressed)
# Visible states first (by size), then suppressed ones grouped at the end.
bar_df <- bund_all %>% select(land, value, is_suppressed)
bar_df$pct <- round(bar_df$value / hl_total * 100, 1)
bar_df$hover <- ifelse(bar_df$is_suppressed,
  sprintf("<b>%s</b><br>Suppressed (<5,000)<br>Destatis does not publish counts below 5,000", bar_df$land),
  sprintf("<b>%s</b><br>%s (%.1f%%)", bar_df$land, format(bar_df$value, big.mark = ","), bar_df$pct))
bar_df$fill_color <- ifelse(bar_df$is_suppressed, "#c8c8c8", "#2774AE")
bar_df$display_value <- ifelse(bar_df$is_suppressed, 2500, bar_df$value)
bar_df$land <- factor(bar_df$land, levels = bar_df$land)

p_bund_bar <- plot_ly(bar_df, x = ~land, y = ~display_value, type = "bar",
    marker = list(color = ~fill_color),
    text = ~hover,
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Origin Population<br>by German State</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickangle = -30, tickfont = list(size = 10)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 60, b = 100),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

# Bundesland choropleth (load GeoJSON server-side, pass inline).
# Suppressed states are rendered in neutral grey (z=NA dropped by plotly and
# filled via the default trace color) with a "Suppressed" hover rather than
# shading at 0 on the blue ramp.
bund_map_data <- bund %>% filter(gen == "all_gens") %>%
  mutate(is_suppressed = is.na(value_k),
         value = ifelse(is_suppressed, NA_real_, value_k * 1000))
bund_map_visible <- bund_map_data %>% filter(!is_suppressed)
bund_map_suppressed <- bund_map_data %>% filter(is_suppressed)

de_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "bundeslaender.geojson"),
  simplifyVector = FALSE)

p_de_map <- plot_ly() %>%
  # Suppressed polygons: neutral grey with "Suppressed" hover
  add_trace(type = "choroplethmapbox",
    geojson = de_geojson,
    locations = bund_map_suppressed$land,
    z = rep(1, nrow(bund_map_suppressed)),
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>Suppressed (<5,000)<br>Destatis does not publish counts below 5,000",
      bund_map_suppressed$land),
    hoverinfo = "text",
    colorscale = list(c(0, "#d0d0d0"), c(1, "#d0d0d0")),
    zmin = 0, zmax = 1,
    showscale = FALSE,
    marker = list(line = list(color = "white", width = 1), opacity = 0.7)
  ) %>%
  add_trace(type = "choroplethmapbox",
    geojson = de_geojson,
    locations = bund_map_visible$land,
    z = bund_map_visible$value,
    featureidkey = "properties.name",
    text = sprintf("<b>%s</b><br>%s Iranian-origin residents<br>%.1f%% of Germany total",
      bund_map_visible$land, format(bund_map_visible$value, big.mark = ","),
      bund_map_visible$value / hl_total * 100),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"), c(0.1, "#6baed6"),
      c(0.4, "#2171b5"), c(1, "#08306b")),
    showscale = TRUE,
    colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
    marker = list(line = list(color = "white", width = 1), opacity = 0.9)
  ) %>% layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lon = 10.5, lat = 51.2),
      zoom = 4.3
    ),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# Generation breakdown (two colored boxes)
make_mig_box <- function(val, pct, label, sublabel, color) {
  sprintf('<div style="background:%s; border-radius:6px; padding:28px 18px; text-align:center; color:white; flex:1; min-width:0; display:flex; flex-direction:column; justify-content:center; min-height:170px;">
    <div class="measure-num" style="font-size:38px; font-weight:700; line-height:1.1;">%s</div>
    <div class="measure-label" style="font-size:14px; margin-top:6px; font-weight:600;">%s</div>
    <div style="font-size:12px; opacity:0.9; margin-top:3px;">%s</div>
    <div style="font-size:11px; opacity:0.85; margin-top:4px;">%s</div>
  </div>',
    color, format(val, big.mark = ","), label, pct, sublabel)
}

mig_grid <- paste0(
  '<div style="display:flex; flex-direction:column; gap:12px; width:100%;">',
  '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">Iranian-Origin Population by Generation</div>',
  '<div style="display:flex; gap:12px;">',
  make_mig_box(hl_fg, paste0(round(hl_fg/hl_total*100), "% of total"), "First generation", "Born in Iran", "#1a4e72"),
  make_mig_box(hl_sg, paste0(round(hl_sg/hl_total*100), "% of total"), "Second generation", "Born in Germany with at least one Iran-born parent", "#5a9bd5"),
  '</div>',
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0;">%s</p>', MZ_SOURCE),
  '</div>'
)

pop_body <- paste0(
  # Top row: headline block + migration grid
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Origin Population in Germany</div>',
  '<div class="number">', format(hl_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the <a href="https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Migration-Integration/Methoden/mikrozensus.html" style="color:#2774AE;" target="_blank">2024 Mikrozensus</a>, an annual 1% household survey by Destatis, the German Federal Statistical Office</div>',
  '<div style="margin:14px auto 0; max-width:460px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">A person is counted if they meet <em>at least one</em> of two survey questions:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Place of birth</strong> <span style="color:#888;">&mdash; &ldquo;In which country were you born?&rdquo; (Iran)</span></li>',
  '<li><strong>Parental origin</strong> <span style="color:#888;">&mdash; &ldquo;In which country was your mother / father born?&rdquo; (Iran, for at least one parent)</span></li>',
  '</ul>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; align-items:center;">', mig_grid, '</div>',
  '</div>',

  # Bottom row: bar chart + choropleth map
  '<div class="chart-row">',
  '<div class="chart-card">', plotly_div("de-bund-bar", plotly_to_json(p_bund_bar), "450px",
    source = paste0(MZ_SOURCE, "<br>Six states with fewer than 5,000 Iranian-origin residents are suppressed by Destatis (shown in grey).")), '</div>',
  '<div class="chart-card">',
  '<div class="section-title">Geographic Distribution in Germany</div>',
  plotly_div("de-bund-map", plotly_to_json(p_de_map), "450px",
    source = paste0(MZ_SOURCE, "<br>Grey states: suppressed (<5,000). Published state counts sum to ~294,000 of the 319,000 national total.")),
  '</div>',
  '</div>'
)

writeLines(page_template("Germany: Population", pop_body), "docs/pages/de-population.html")
cat("  Done\n")


# =====================================================
# DE IMMIGRATION & CITIZENSHIP (page-content layout)
# =====================================================
cat("Building de-immigration...\n")

motive_clean <- motive %>%
  mutate(short = case_when(
    motive == "Flucht, Asyl, internationaler Schutz" ~ "Flight / asylum",
    motive == "Familienzusammenf\u00fchrung" ~ "Family reunification",
    motive == "Familiengr\u00fcndung" ~ "Family formation",
    motive == "Studium, Ausbildung, Weiterbildung" ~ "Study / training",
    motive == "Arbeit/Besch\u00e4ftigung zusammen" ~ "Work",
    motive == "sonstige Gr\u00fcnde" ~ "Other",
    motive == "Hauptmotiv f\u00fcr die Zuwanderung Insgesamt" ~ "TOTAL",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(short), short != "TOTAL") %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)

motive_total <- hl_fg
motive_clean$pct <- round(motive_clean$value / motive_total * 100, 1)
motive_clean <- motive_clean %>% arrange(desc(value))
motive_clean$short <- factor(motive_clean$short, levels = motive_clean$short)

motive_colors <- c(
  "Flight / asylum" = "#c4793a",
  "Study / training" = "#d4a943",
  "Family reunification" = "#8a5a3a",
  "Work" = "#b05050",
  "Family formation" = "#e8b878",
  "Other" = "#b0b0b0"
)

p_motive <- plot_ly(motive_clean, x = ~short, y = ~value, type = "bar",
    marker = list(color = motive_colors[as.character(motive_clean$short)]),
    text = ~sprintf("<b>%s</b><br>%s (%.1f%%)", short, format(value, big.mark = ","), pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Main Reason Iran-Born Immigrants<br>Came to Germany</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 65, b = 110),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

# Residence duration — Iran-born residents, Mikrozensus 2024 Table 12211-08
# Two cohorts are visible: a recent wave (under 10 years) and an established
# 1980s/90s cohort (25+ years).
dur_labels <- c(
  "under_5"  = "< 5",
  "5_to_10"  = "5\u201310",
  "10_to_15" = "10\u201315",
  "15_to_20" = "15\u201320",
  "20_to_25" = "20\u201325",
  "25_to_30" = "25\u201330",
  "30_to_40" = "30\u201340",
  "40_plus"  = "40+"
)
dur_df <- duration %>%
  mutate(
    value = ifelse(is.na(value_k), 0, value_k) * 1000,
    label = dur_labels[bucket]
  )
dur_df$label <- factor(dur_df$label, levels = unname(dur_labels))
dur_df$pct <- round(dur_df$value / hl_fg * 100, 1)
# Monochromatic blue — consistent with other countries' duration charts.
p_duration <- plot_ly(dur_df, x = ~label, y = ~value, type = "bar",
    marker = list(color = "#2774AE"),
    text = ~sprintf("<b>%s years</b><br>%s (%.1f%%)",
      label, format(value, big.mark = ","), pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Years Iran-Born Residents<br>Have Lived in Germany</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "Years in Germany", tickfont = list(size = 11)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 65, b = 70),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

# Annual arrivals chart — Iranian Zuzüge 1991-2023 from BAMF/Destatis
# Migrationsberichte. This is a flow series (annual arrivals in that year),
# not a stock-based cohort view. No cumulative line here — unlike the US
# (FY1978+) or Canada, the data starts at 1991 when Germany already had
# ~108K Iran-born residents, so a cumulative % starting at 0 is misleading.
p_annual <- plot_ly() %>%
  add_bars(
    data = annual_arrivals,
    x = ~year, y = ~iran_arrivals,
    marker = list(color = "#2774AE", line = list(color = "#1a4e72", width = 0.3)),
    text = ~sprintf("<b>%d</b><br>%s Iranian arrivals",
      year, format(iran_arrivals, big.mark = ",")),
    hoverinfo = "text", textposition = "none",
    showlegend = FALSE,
    name = "Annual arrivals"
  ) %>%
  layout(
    title = list(text = "<b>Annual Iranian Arrivals to Germany,<br>1991\u20132023</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 10), dtick = 4),
    yaxis = list(title = "", tickformat = ",", tickfont = list(size = 10)),
    margin = list(t = 55, b = 40, l = 55, r = 20),
    plot_bgcolor = "white", paper_bgcolor = "white",
    hovermode = "closest"
  ) %>%
  config(displayModeBar = FALSE)

p_citizen <- plot_ly(data.frame(
    status = c("German citizens", "Iranian nationals"),
    count  = c(hl_deu, hl_for),
    pct    = round(c(hl_deu, hl_for) / hl_total * 100)
  ),
  x = ~status, y = ~count, type = "bar",
  # Blue family on the left side of the Immigration page; the motive and
  # duration charts on the right use a warm-earth palette so colors don't
  # clash across the two sides.
  marker = list(color = c("#2774AE", "#e07b54")),
  text = ~sprintf("<b>%s</b><br>%s (%s%%)",
    status, format(count, big.mark = ","), pct),
  hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Citizenship of Iranian-Origin<br>Residents in Germany</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 12)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 60, b = 60),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

# Dynamic shares for the factoid cards
de_cit_pct <- round(hl_deu / hl_total * 100)
dur_10plus_k <- sum(duration$value_k[duration$bucket %in% c(
  "10_to_15","15_to_20","20_to_25","25_to_30","30_to_40","40_plus")], na.rm = TRUE)
dur_30plus_k <- sum(duration$value_k[duration$bucket %in% c(
  "30_to_40","40_plus")], na.rm = TRUE)
fg_k <- hl_fg / 1000
dur_10plus_pct <- round(dur_10plus_k / fg_k * 100)
dur_30plus_pct <- round(dur_30plus_k / fg_k * 100)

immig_body <- paste0(
  '<div class="text-row">',
  sprintf('<div class="text-card" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iranian-origin residents in Germany hold German citizenship &mdash; about %s people.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">About %s hold Iranian citizenship. 7,840 Iranian naturalizations were recorded in 2024.</div>
  </div>', de_cit_pct, format(hl_deu, big.mark = ","), format(hl_for, big.mark = ",")),
  sprintf('<div class="text-card" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian residents have lived in Germany for a decade or more.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Roughly %d%% have lived in Germany for 30 or more years.</div>
  </div>', dur_10plus_pct, dur_30plus_pct),
  '</div>',

  '<div class="chart-row">',
  # Left: citizenship chart
  '<div class="chart-card">',
  plotly_div("de-cit", plotly_to_json(p_citizen), "480px",
    source = MZ_SOURCE),
  '</div>',

  # Right: tabbed annual arrivals + motive + duration
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'de-immig-annual\',this,\'de-immig-tabs\')">Annual Arrivals</button>',
  '<button class="tab-btn" onclick="switchTab(\'de-immig-motive\',this,\'de-immig-tabs\')">Main Reason for Coming</button>',
  '<button class="tab-btn" onclick="switchTab(\'de-immig-duration\',this,\'de-immig-tabs\')">Years in Germany</button>',
  '</div>',
  '<div id="de-immig-annual" class="tab-panel active" data-group="de-immig-tabs">',
  plotly_div("de-annual", plotly_to_json(p_annual), "430px",
    source = "Source: BAMF Migrationsb&#228;richte (2005, 2015, 2020, 2023 editions). Annual Iranian Zuz&#252;ge (arrivals to Germany) as reported in the official migration flow statistics. Pre-1991 data exists only for West Germany and is not included here."),
  '</div>',
  '<div id="de-immig-motive" class="tab-panel" data-group="de-immig-tabs">',
  plotly_div("de-motive", plotly_to_json(p_motive), "440px",
    source = paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024. First generation only (born in Iran).")),
  '</div>',
  '<div id="de-immig-duration" class="tab-panel" data-group="de-immig-tabs">',
  plotly_div("de-duration", plotly_to_json(p_duration), "430px",
    source = paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024. Iran-born residents only.")),
  '</div>',
  '</div>',
  '</div>'
)

writeLines(page_template("Germany: Immigration & Citizenship", immig_body, has_tabs = TRUE), "docs/pages/de-immigration.html")
cat("  Done\n")


# =====================================================
# DE EDUCATION (page-content layout)
# =====================================================
cat("Building de-education...\n")

school_cats <- c(
  "Ohne Schulabschluss" = "No school certificate",
  "Darunter: Hauptschule" = "Basic secondary",
  "Realschule o. \u00e4." = "Intermediate secondary",
  "Fachhochschulreife" = "Applied-university entrance",
  "Abitur" = "University entrance"
)
school_clean <- school %>%
  mutate(cat = school_cats[school_level]) %>%
  filter(!is.na(cat)) %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)

# Denominator = full Iranian-origin population (matches the text cards)
school_all <- school_clean %>% filter(gen == "all_gens") %>%
  mutate(pct = round(value / hl_total * 100, 1))
school_all$cat <- factor(school_all$cat, levels = unname(school_cats))

school_colors <- c(
  "No school certificate" = "#b0b0b0",
  "Basic secondary" = "#e8b878",
  "Intermediate secondary" = "#d4a943",
  "Applied-university entrance" = "#c4793a",
  "University entrance" = "#8a5a3a"
)

p_school <- plot_ly(school_all, x = ~cat, y = ~pct, type = "bar",
    marker = list(color = school_colors[as.character(school_all$cat)]),
    text = ~sprintf("<b>%s</b><br>%s (%.1f%%)",
      cat, format(value, big.mark = ","), pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Highest School Qualification in Germany</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickangle = -15, tickfont = list(size = 10)),
    yaxis = list(title = "", ticksuffix = "%", range = c(0, 60)),
    margin = list(t = 60, b = 120),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

prof_clean <- prof %>%
  mutate(cat = case_when(
    prof_level == "Ohne berufsqualifizierenden Abschluss" ~ "No vocational qualification",
    prof_level == "darunter: nicht-akademischer Abschluss" ~ "Vocational (non-academic)",
    prof_level == "akademischer Abschluss" ~ "Academic degree",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(cat)) %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)

prof_all <- prof_clean %>% filter(gen == "all_gens") %>%
  mutate(pct = round(value / hl_total * 100, 1))
prof_cat_order <- c("No vocational qualification", "Vocational (non-academic)", "Academic degree")
prof_all$cat <- factor(prof_all$cat, levels = prof_cat_order)
prof_all <- prof_all %>% arrange(cat)

prof_colors <- c(
  "No vocational qualification" = "#b0b0b0",
  "Vocational (non-academic)" = "#d4a943",
  "Academic degree" = "#8a5a3a"
)

p_prof <- plot_ly(prof_all, x = ~cat, y = ~pct, type = "bar",
    marker = list(color = prof_colors[as.character(prof_all$cat)]),
    text = ~sprintf("<b>%s</b><br>%s (%.1f%%)",
      cat, format(value, big.mark = ","), pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Highest Vocational or Academic<br>Qualification in Germany</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 11)),
    yaxis = list(title = "", ticksuffix = "%", range = c(0, 40)),
    margin = list(t = 60, b = 70),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

# (de-education charts p_school and p_prof are built above; the education
# writeLines now happens inside the combined Language & Education page below,
# which is assembled after the language chart is built.)
cat("  (de-education charts prepared, will be combined with language)\n")


# =====================================================
# DE WORK (page-content layout)
# =====================================================
cat("Building de-work...\n")

emp_status <- emp %>% filter(section == "status", gen == "all_gens") %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)
employed    <- emp_status$value[emp_status$category == "Erwerbst\u00e4tige"]
unemployed  <- emp_status$value[emp_status$category == "Erwerbslose"]
inactive    <- emp_status$value[emp_status$category == "Nichterwerbspersonen"]
emp_total   <- employed + unemployed + inactive

emp_bar_df <- data.frame(
  status = c("Employed", "Unemployed", "Not in labour force"),
  count  = c(employed, unemployed, inactive),
  stringsAsFactors = FALSE
)
emp_bar_df$pct <- round(emp_bar_df$count / emp_total * 100, 1)
emp_bar_df$status <- factor(emp_bar_df$status, levels = emp_bar_df$status)

p_emp_status <- plot_ly(emp_bar_df, x = ~status, y = ~count, type = "bar",
    # Warm-earth palette for the Work side of the page; the income chart
    # on the right uses a blue gradient so the two sides stay distinct.
    marker = list(color = c("#c4793a", "#b05050", "#b0b0b0")),
    text = ~sprintf("<b>%s</b><br>%s (%.1f%%)",
      status, format(count, big.mark = ","), pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Labour Force Status in Germany</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 11)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 55, b = 70),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

# True employed total ("Erwerbstätige") — the denominator for industry %.
# Previously percentages were normalized to the sum of visible industry
# bars (~164K), which understated suppressed sectors. Now % is computed
# against the full published employed total (~170K), and suppressed
# sectors are shown as grey bars labeled "Suppressed (<5K)".
hl_employed <- emp$value_k[emp$section == "status" &
                           emp$category == "Erwerbst\u00e4tige" &
                           emp$gen == "all_gens"] * 1000

ind <- emp %>% filter(section == "industry", gen == "all_gens") %>%
  mutate(is_suppressed = is.na(value_k),
         value = ifelse(is_suppressed, 0, value_k * 1000),
         label = case_when(
           category == "Land- und Forstwirtschaft, Fischerei" ~ "Agriculture & fishing",
           category == "Produzierendes Gewerbe, Baugewerbe" ~ "Industry & construction",
           category == "Handel, Gastgewerbe und Verkehr" ~ "Trade, hospitality & transport",
           category == "\u00d6ffentliche Verwaltung" ~ "Public administration",
           category == "Sonstige Dienstleistungen" ~ "Other services",
           TRUE ~ category
         )) %>%
  arrange(desc(value), is_suppressed)
# Percentages always use the true employed total; visible bars don't sum to
# 100% because two sectors are Destatis-suppressed.
ind$pct <- round(ind$value / hl_employed * 100, 1)
ind$display_value <- ifelse(ind$is_suppressed, 3000, ind$value)
ind$hover <- ifelse(ind$is_suppressed,
  sprintf("<b>%s</b><br>Suppressed (<5,000)<br>Destatis does not publish counts below 5,000", ind$label),
  sprintf("<b>%s</b><br>%s (%.1f%% of all employed)", ind$label,
          format(ind$value, big.mark = ","), ind$pct))
# Horizontal bars: suppressed rows at the bottom, then non-suppressed
# sorted ascending so the longest bar appears at the top.
ind <- ind %>% arrange(desc(is_suppressed), value)
ind$label <- factor(ind$label, levels = ind$label)
# Warm-earth palette for the industry chart; matches the labour-force
# chart in the same tab card. Suppressed sectors are grey.
ind_palette_visible <- c("#c4793a", "#d4a943", "#8a5a3a", "#e8b878", "#b0b0b0")
n_visible <- sum(!ind$is_suppressed)
ind$fill_color <- c(ind_palette_visible[seq_len(n_visible)],
                    rep("#c8c8c8", nrow(ind) - n_visible))

p_industry <- plot_ly(ind, y = ~label, x = ~display_value, type = "bar",
    orientation = "h",
    marker = list(color = ~fill_color),
    text = ~hover,
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Origin Employment by Industry</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 11),
      automargin = TRUE),
    margin = list(t = 55, b = 40, l = 200),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

# (work_body and the separate de-work page are no longer built here; the work
# charts are now combined with income into the de-workinc page below.)
cat("  (de-work charts prepared, will be combined with income)\n")


# =====================================================
# DE INCOME (page-content layout)
# =====================================================
cat("Building de-income...\n")

inc <- emp %>% filter(section == "income", gen == "all_gens") %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000,
         bracket = case_when(
           grepl("unter 500", category) ~ "Under \u20ac500",
           category == "500 - 1 000" ~ "\u20ac500\u2013\u20ac1,000",
           category == "1 000 - 1 500" ~ "\u20ac1,000\u2013\u20ac1,500",
           category == "1 500 - 2 000" ~ "\u20ac1,500\u2013\u20ac2,000",
           category == "2 000 - 2 500" ~ "\u20ac2,000\u2013\u20ac2,500",
           category == "2 500 - 3 000" ~ "\u20ac2,500\u2013\u20ac3,000",
           category == "3 000 - 3 500" ~ "\u20ac3,000\u2013\u20ac3,500",
           category == "3 500 und mehr" ~ "\u20ac3,500+",
           category == "Kein Einkommen" ~ "No income",
           TRUE ~ category
         )) %>%
  filter(bracket != "No income")

inc_denom <- sum(inc$value, na.rm = TRUE)
inc$pct <- round(inc$value / inc_denom * 100, 1)
inc_order <- c("Under \u20ac500", "\u20ac500\u2013\u20ac1,000", "\u20ac1,000\u2013\u20ac1,500",
               "\u20ac1,500\u2013\u20ac2,000", "\u20ac2,000\u2013\u20ac2,500",
               "\u20ac2,500\u2013\u20ac3,000", "\u20ac3,000\u2013\u20ac3,500", "\u20ac3,500+")
inc$bracket <- factor(inc$bracket, levels = inc_order)
inc <- inc %>% arrange(bracket)

inc_colors <- c("#d4e6f1", "#bcdcec", "#a3d2e6", "#8bbdde", "#5a9bd5", "#2774AE", "#1a4e72", "#0d2f4a")

p_income <- plot_ly(inc, x = ~bracket, y = ~pct, type = "bar",
    marker = list(color = inc_colors),
    text = ~sprintf("<b>%s per month</b><br>%s workers (%.1f%%)",
      bracket, format(value, big.mark = ","), pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Monthly Net Income of<br>Iranian-Origin Workers in Germany</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickangle = -25, tickfont = list(size = 10)),
    yaxis = list(title = "", ticksuffix = "%", range = c(0, 25)),
    margin = list(t = 65, b = 110),
    plot_bgcolor = "white", paper_bgcolor = "white",
    showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

# Headline share: middle brackets (1,000-2,500)
middle_share <- sum(inc$pct[inc$bracket %in% c("\u20ac1,000\u2013\u20ac1,500", "\u20ac1,500\u2013\u20ac2,000", "\u20ac2,000\u2013\u20ac2,500")])
top_share    <- sum(inc$pct[inc$bracket %in% c("\u20ac3,000\u2013\u20ac3,500", "\u20ac3,500+")])
low_share    <- sum(inc$pct[inc$bracket %in% c("Under \u20ac500", "\u20ac500\u2013\u20ac1,000")])

# =====================================================
# DE WORK & INCOME (combined page)
# =====================================================
cat("Building de-workinc...\n")

# Dynamic shares for work factoid card
emp_all <- emp %>% filter(gen == "all_gens")
employed_k   <- emp_all$value_k[grepl("^Erwerbstätige$", emp_all$category)][1]
unemployed_k <- emp_all$value_k[grepl("Erwerbslose|^Arbeitslose", emp_all$category)][1]
nilf_k       <- emp_all$value_k[grepl("Nichterwerbspersonen", emp_all$category)][1]
services_k   <- sum(emp_all$value_k[emp_all$category %in% c(
  "Handel, Gastgewerbe und Verkehr", "Sonstige Dienstleistungen")], na.rm = TRUE)
services_pct <- round(services_k / employed_k * 100)
total_15plus_k <- hl_total / 1000
emp_pct      <- round(employed_k / total_15plus_k * 100)
unemp_pct    <- round(unemployed_k / total_15plus_k * 100)
nilf_pct     <- round(nilf_k / total_15plus_k * 100)

workinc_body <- paste0(
  '<div class="text-row">',
  sprintf('<div class="text-card" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of employed Iranian-origin residents work in the broad services sector (trade, hospitality, transport, and other services).</div>',
    services_pct),
  sprintf('    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%d%% of all Iranian-origin residents aged 15+ are employed; %d%% are unemployed and %d%% are not in the labour force (students, retirees, carers).</div>
  </div>',
    emp_pct, unemp_pct, nilf_pct),
  sprintf('<div class="text-card" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%.0f%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of employed Iranian-origin residents earn between %s1,000 and %s2,500 net per month.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">About %.0f%% earn %s3,000 or more per month; %.0f%% earn under %s1,000.</div>
  </div>', middle_share, "\u20ac", "\u20ac", top_share, "\u20ac", low_share, "\u20ac"),
  '</div>',
  '<div class="chart-row">',
  # LEFT: work with 2 tabs (labour force status + industry)
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'de-wk-status\',this,\'de-wk-tabs\')">Labour Force Status</button>',
  '<button class="tab-btn" onclick="switchTab(\'de-wk-industry\',this,\'de-wk-tabs\')">Employment by Industry</button>',
  '</div>',
  '<div id="de-wk-status" class="tab-panel active" data-group="de-wk-tabs">',
  plotly_div("de-empstatus", plotly_to_json(p_emp_status), "460px",
    source = paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024")),
  '</div>',
  '<div id="de-wk-industry" class="tab-panel" data-group="de-wk-tabs">',
  plotly_div("de-industry", plotly_to_json(p_industry), "460px",
    source = paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024. Iranian-origin residents employed across five broad sectors. Agriculture and Public administration are suppressed by Destatis (shown in grey). Percentages are computed against the full published employed total (~170,000), so visible bars do not sum to 100%.")),
  '</div>',
  '</div>',
  # RIGHT: income chart
  '<div class="chart-card">',
  plotly_div("de-income", plotly_to_json(p_income), "510px",
    source = paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024. Net monthly personal income of employed Iranian-origin residents, all generations combined.")),
  '</div>',
  '</div>'
)

writeLines(page_template("Germany: Work & Income", workinc_body, has_tabs = TRUE), "docs/pages/de-workinc.html")
cat("  Done\n")


# =====================================================
# DE LANGUAGE (page-content layout)
# =====================================================
cat("Building de-language...\n")

lang_rollup <- function(g) {
  sub <- lang %>% filter(gen == g) %>%
    mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)
  total <- sub$value[sub$language == "Zu Hause vorwiegend gesprochene Sprache Insgesamt"]
  only_de   <- sub$value[sub$language == "nur Deutsch"]
  pred_de   <- sub$value[sub$language == "vorwiegend deutsch"]
  persian   <- sub$value[sub$language == "Persisch"]
  pred_nde  <- sub$value[sub$language == "vorwiegend nicht-deutsch"]
  other_nde <- max(pred_nde - persian, 0)
  data.frame(
    gen = g,
    category = c("German only", "Mostly German", "Mostly Persian",
                 "Mostly other non-German"),
    value = c(only_de, pred_de, persian, other_nde),
    stringsAsFactors = FALSE
  ) %>% mutate(pct = round(value / total * 100, 1))
}

lang_all <- bind_rows(lang_rollup("all_gens"), lang_rollup("first_gen"))
lang_all$gen_label <- factor(ifelse(lang_all$gen == "all_gens",
  "All Iranian-origin", "First generation only"),
  levels = c("All Iranian-origin", "First generation only"))
lang_cat_order <- c("German only", "Mostly German", "Mostly Persian", "Mostly other non-German")
lang_all$category <- factor(lang_all$category, levels = lang_cat_order)
lang_colors <- c(
  "German only" = "#1a4e72",
  "Mostly German" = "#5a9bd5",
  "Mostly Persian" = "#4a8c6f",
  "Mostly other non-German" = "#b0b0b0"
)

p_lang <- plot_ly()
for (cat_name in lang_cat_order) {
  sub <- lang_all %>% filter(category == cat_name)
  p_lang <- p_lang %>% add_bars(
    data = sub, y = ~gen_label, x = ~pct, name = cat_name,
    marker = list(color = lang_colors[cat_name]), textposition = "none",
    hovertext = sprintf("<b>%s</b><br>%s<br>%s residents (%.1f%%)",
      cat_name, sub$gen_label, format(sub$value, big.mark = ","), sub$pct),
    hoverinfo = "text",
    legendgroup = cat_name, showlegend = FALSE, orientation = "h")
}
p_lang <- p_lang %>% layout(
  barmode = "stack",
  title = list(text = "<b>Main Language Spoken at Home in Germany</b>",
    font = list(size = 16, family = "Montserrat")),
  xaxis = list(title = "", ticksuffix = "%", range = c(0, 105)),
  yaxis = list(title = "",
    # Put "First generation only" on top, "All Iranian-origin" underneath.
    # categoryarray is bottom-to-top for horizontal bars.
    categoryorder = "array", categoryarray = levels(lang_all$gen_label),
    ticklabelstandoff = 6),
  margin = list(t = 55, b = 40, l = 140), showlegend = FALSE,
  plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

lang_leg <- make_html_legend(lang_colors, break_after = 2)

# =====================================================
# DE LANGUAGE & EDUCATION (combined page)
# =====================================================
cat("Building de-langedu...\n")

# Dynamic shares for the factoid cards
lang_total <- lang$value_k[lang$language == "Zu Hause vorwiegend gesprochene Sprache Insgesamt" & lang$gen == "all_gens"]
persian_k <- lang$value_k[lang$language == "Persisch" & lang$gen == "all_gens"]
only_de_k <- lang$value_k[lang$language == "nur Deutsch" & lang$gen == "all_gens"]
persian_pct <- round(persian_k / lang_total * 100)
only_de_pct <- round(only_de_k / lang_total * 100)

abitur_k <- school$value_k[school$school_level == "Abitur" & school$gen == "all_gens"]
abitur_pct <- round(abitur_k / hl_total * 1000 * 100)
academic_k <- prof$value_k[prof$prof_level == "akademischer Abschluss" & prof$gen == "all_gens"]
academic_pct <- round(academic_k / hl_total * 1000 * 100)

langedu_body <- paste0(
  '<div class="text-row">',
  sprintf('<div class="text-card" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iranian-origin residents speak Persian as the main language at home &mdash; about %s people.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Another %d%% speak only German at home, and 9%% use mainly German.</div>
  </div>', persian_pct, format(persian_k * 1000, big.mark = ","), only_de_pct),
  sprintf('<div class="text-card" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iranian-origin residents in Germany hold the Abitur, the secondary-school qualification that grants university entry.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">About %d%% (~%s) hold an academic degree.</div>
  </div>', abitur_pct, academic_pct, format(academic_k * 1000, big.mark = ",")),
  '</div>',
  '<div class="chart-row" style="align-items:stretch;">',
  # LEFT: language chart (standalone, no persian sidebar)
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  plotly_div("de-lang", plotly_to_json(p_lang), "320px",
    source = paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024"),
    legend_html = lang_leg, highlight_hover = TRUE),
  '</div>',
  # RIGHT: education with 2 tabs (school qualification + vocational/academic)
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'de-ed-school\',this,\'de-ed-tabs\')">Highest School Qualification</button>',
  '<button class="tab-btn" onclick="switchTab(\'de-ed-prof\',this,\'de-ed-tabs\')">Vocational / Academic Qualification</button>',
  '</div>',
  '<div id="de-ed-school" class="tab-panel active" data-group="de-ed-tabs">',
  plotly_div("de-school", plotly_to_json(p_school), "450px",
    source = paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024")),
  '</div>',
  '<div id="de-ed-prof" class="tab-panel" data-group="de-ed-tabs">',
  plotly_div("de-prof", plotly_to_json(p_prof), "450px",
    source = paste0("Source: ", MZ_LINK, " \u2014 Mikrozensus 2024")),
  '</div>',
  '</div>',
  '</div>'
)

writeLines(page_template("Germany: Language & Education", langedu_body, has_tabs = TRUE), "docs/pages/de-langedu.html")
cat("  Done\n")

cat("\nAll Germany pages built.\n")

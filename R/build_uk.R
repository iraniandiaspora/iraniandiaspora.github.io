# UK pages — ONS Census 2021 (E+W) + Scotland Census 2022 + NI 2021.
#
# Consumes:
#   data/uk/iran_uk_summary_2021.csv  -- 4 UK nations + totals
#   data/uk/uk_region_iran.csv        -- 11 GB regions (aggregated from LADs)
#   data/uk/uk_regions.geojson        -- 11-region GeoJSON
#   data/uk/uk_age_sex.csv            -- Age (6 bands) x Sex
#   data/uk/uk_year_of_arrival.csv    -- Year of arrival (11 periods)
#   data/uk/uk_economic_activity.csv  -- Economic activity (6 categories)
#   data/uk/uk_qualification.csv      -- Highest qualification (6 categories)
#   data/uk/uk_religion.csv           -- Religion (9 categories)
#
# Produces:
#   docs/pages/uk-population.html
#   docs/pages/uk-workedu.html
#
# Run from the deployment repo root:
#   Rscript R/build_uk.R
#
# (Regenerate clean data first via:
#    Rscript R/uk_export/clean_nomis_data.R
#    Rscript R/uk_export/clean_ons_custom.R)

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

# ============================================================================
# HELPERS (canonical pattern)
# ============================================================================
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
  list(data   = toJSON(b$x$data, auto_unbox = TRUE, null = "null", na = "null"),
       layout = toJSON(b$x$layout, auto_unbox = TRUE, null = "null", na = "null"),
       config = toJSON(b$x$config, auto_unbox = TRUE))
}

plotly_div <- function(id, json, height = "500px", source = NULL, legend_html = NULL) {
  init_js <- sprintf(
    'var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox"});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  chart <- sprintf(
    '<div id="%s" style="width:100%%;height:%s;touch-action:pan-y;"></div>\n<script>(function(){%s})();</script>',
    id, height, init_js)
  if (!is.null(legend_html)) chart <- paste0(chart, '\n', legend_html)
  if (!is.null(source)) {
    chart <- paste0(chart, sprintf(
      '\n<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0; padding-right:2px;">%s</p>',
      source))
  }
  chart
}

make_html_legend <- function(colors, labels = names(colors), break_after = NULL) {
  items <- mapply(function(col, lab) {
    sprintf('<span style="display:inline-flex;align-items:center;gap:4px;margin:0 8px;"><span style="display:inline-block;width:14px;height:14px;background:%s;border-radius:2px;flex-shrink:0;"></span><span style="font-size:12px;color:#333;">%s</span></span>', col, lab)
  }, colors, labels, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if (!is.null(break_after) && break_after < length(items)) {
    row1 <- paste(items[1:break_after], collapse = "")
    row2 <- paste(items[(break_after + 1):length(items)], collapse = "")
    inner <- paste0('<div style="display:flex;flex-wrap:wrap;justify-content:center;gap:2px;">', row1, '</div>',
                    '<div style="display:flex;flex-wrap:wrap;justify-content:center;gap:2px;">', row2, '</div>')
  } else {
    inner <- paste(items, collapse = "")
  }
  sprintf('<div style="text-align:center;margin:6px 0 2px;">%s</div>', inner)
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
function switchTab(tabId,btn,groupId){
  var panels = document.querySelectorAll(".tab-panel[data-group=\'" + groupId + "\']");
  panels.forEach(function(p) { p.classList.remove("active"); });
  document.getElementById(tabId).classList.add("active");
  var btns = btn.parentElement.querySelectorAll(".tab-btn");
  btns.forEach(function(b) { b.classList.remove("active"); });
  btn.classList.add("active");
  var active = document.getElementById(tabId);
  if (active) {
    active.querySelectorAll(".js-plotly-plot").forEach(function(p) {
      if (window.Plotly) Plotly.Plots.resize(p);
    });
  }
  reportHeight();
}
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
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:hidden; min-width:0; }
.text-card { background:white; border-radius:8px; padding:20px; border:1px solid #e0e0e0; font-size:14px; color:#444; line-height:1.7; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.headline { background:white; border-radius:8px; padding:30px; text-align:center; border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }
.tab-bar { display:flex; justify-content:center; gap:0; margin:12px 0 0; }
.tab-btn { padding:6px 16px; border:1px solid #ddd; background:#f0f0f0; cursor:pointer;
  font-family:"Montserrat",sans-serif; font-size:13px; color:#333; border-radius:4px; margin:0 2px; transition:background 0.15s; white-space:nowrap; }
.tab-btn.active { background:#2774AE; color:white; font-weight:600; border-color:#2774AE; }
.tab-btn:hover:not(.active) { background:#e0e0e0; }
.tab-panel { display:none; }
.tab-panel.active { display:block; }
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row { grid-template-columns:1fr !important; }
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
  .tab-btn { font-size:11px; padding:4px 8px; }
}
</style>
</head>
<body>
', body_html, '
', iframe_resize_script, '
</body>
</html>')
}

# ============================================================================
# DATA
# ============================================================================
cat("Building UK pages...\n")

summary_df <- read.csv("data/uk/iran_uk_summary_2021.csv", stringsAsFactors = FALSE)
region_df  <- read.csv("data/uk/uk_region_iran.csv", stringsAsFactors = FALSE)
age_sex    <- read.csv("data/uk/uk_age_sex.csv", stringsAsFactors = FALSE)
arrival    <- read.csv("data/uk/uk_year_of_arrival.csv", stringsAsFactors = FALSE)
econ       <- read.csv("data/uk/uk_economic_activity.csv", stringsAsFactors = FALSE)
qual       <- read.csv("data/uk/uk_qualification.csv", stringsAsFactors = FALSE)
relig      <- read.csv("data/uk/uk_religion.csv", stringsAsFactors = FALSE)

uk_total <- summary_df$iran_born[summary_df$country == "UK total"]
nireland <- summary_df$iran_born[summary_df$country == "Northern Ireland"]

ONS_LINK  <- "<a href='https://www.nomisweb.co.uk/sources/census_2021' target='_blank' style='color:#2774AE;'>ONS Census 2021</a>"
SCOT_LINK <- "<a href='https://www.scotlandscensus.gov.uk/' target='_blank' style='color:#2774AE;'>Scotland&rsquo;s Census 2022</a>"
NISRA_LINK <- "<a href='https://www.nisra.gov.uk/statistics/census/2021-census' target='_blank' style='color:#2774AE;'>NISRA Census 2021</a>"
ONS_CUSTOM <- paste0("Source: ", ONS_LINK, " \u2014 England and Wales, custom dataset")

# ============================================================================
# UK-POPULATION
# ============================================================================
cat("Building uk-population...\n")

# --- Choropleth map ---
uk_geojson <- jsonlite::fromJSON("data/uk/uk_regions.geojson", simplifyVector = FALSE)

p_uk_map <- plot_ly() %>%
  add_trace(type = "choroplethmapbox",
    geojson = uk_geojson,
    locations = region_df$region_code,
    z = region_df$iran_born,
    featureidkey = "properties.AREACD",
    text = sprintf("<b>%s</b><br>%s Iran-born residents<br>%.1f%% of UK total",
      region_df$region_name,
      format(region_df$iran_born, big.mark = ","),
      region_df$iran_born / uk_total * 100),
    hoverinfo = "text",
    colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"), c(0.08, "#6baed6"),
      c(0.35, "#2171b5"), c(1, "#08306b")),
    showscale = FALSE,
    marker = list(line = list(color = "white", width = 1), opacity = 0.92)
  ) %>% layout(
    mapbox = list(style = "carto-positron",
      center = list(lon = -2.5, lat = 54.7), zoom = 4.3),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

# --- Age by sex grouped bar ---
age_order <- c("15 years and under", "16 to 24 years", "25 to 34 years",
               "35 to 49 years", "50 to 64 years", "65 years and over")
age_sex$age_band <- factor(age_sex$age_band, levels = age_order)

males   <- age_sex %>% filter(sex == "Male") %>% arrange(age_band)
females <- age_sex %>% filter(sex == "Female") %>% arrange(age_band)

p_age <- plot_ly() %>%
  add_bars(x = males$age_band, y = males$count, name = "Male",
    marker = list(color = "#2171b5"),
    text = sprintf("<b>%s</b><br>Male: %s", males$age_band,
      format(males$count, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  add_bars(x = females$age_band, y = females$count, name = "Female",
    marker = list(color = "#c4793a"),
    text = sprintf("<b>%s</b><br>Female: %s", females$age_band,
      format(females$count, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iran-Born in England and Wales<br>by Age and Sex, 2021</b>",
      font = list(size = 14, family = "Montserrat")),
    barmode = "group",
    xaxis = list(title = "", tickangle = -30, tickfont = list(size = 10)),
    yaxis = list(title = "", tickformat = ","),
    margin = list(t = 55, b = 80), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

age_leg <- make_html_legend(c("Male" = "#2171b5", "Female" = "#c4793a"))

# --- Year of arrival bars + cumulative line ---
# Use categorical x-axis (period labels) for even bar spacing
arrival <- arrival %>% arrange(mid_year)
arrival$label <- factor(arrival$label, levels = arrival$label)

p_arrival <- plot_ly() %>%
  add_bars(data = arrival, x = ~label, y = ~count,
    marker = list(color = "#2774AE"),
    text = sprintf("<b>%s</b><br>%s arrivals<br>Cumulative: %.1f%%",
      arrival$label, format(arrival$count, big.mark = ","), arrival$cum_pct),
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  add_trace(data = arrival, x = ~label, y = ~cum_pct, type = "scatter",
    mode = "lines", yaxis = "y2",
    line = list(color = "lightblue", width = 2),
    hoverinfo = "skip", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Year of Arrival in the UK,<br>Iran-Born Residents</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickangle = -45, tickfont = list(size = 9)),
    yaxis = list(title = "", tickformat = ","),
    yaxis2 = list(title = "", overlaying = "y", side = "right",
      ticksuffix = "%", range = c(0, 105), showgrid = FALSE,
      tickfont = list(size = 10, color = "#888")),
    margin = list(t = 55, b = 80, r = 40),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Assemble uk-population ---
pop_body <- paste0(
  # Top row: headline (left) + tabbed age/arrival (right)
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iran-Born Population in the United Kingdom</div>',
  '<div class="number">', format(uk_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the most recent ', ONS_LINK,
  ', ', SCOT_LINK, ', and ', NISRA_LINK, '</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">The UK uses a decennial census. A person is counted as Iran-born based on:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Country of birth</strong> <span style="color:#888;">&mdash; &ldquo;What is your country of birth?&rdquo;</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">The UK census does not ask about ancestry or parental origin, so British-born children of Iran-born parents are not counted. Northern Ireland (', format(nireland, big.mark = ","), ' Iran-born) is included in the total but not shown on the regional map.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'uk-tab-age\',this,\'pop-tabs\')">Age &amp; Sex</button>',
  '<button class="tab-btn" onclick="switchTab(\'uk-tab-arrival\',this,\'pop-tabs\')">Year of Arrival</button>',
  '</div>',
  '<div id="uk-tab-age" class="tab-panel active" data-group="pop-tabs">',
  plotly_div("uk-age", plotly_to_json(p_age), "430px", source = ONS_CUSTOM, legend_html = age_leg),
  '</div>',
  '<div id="uk-tab-arrival" class="tab-panel" data-group="pop-tabs">',
  plotly_div("uk-arrival", plotly_to_json(p_arrival), "430px", source = ONS_CUSTOM),
  '</div>',
  '</div>',
  '</div>',

  # Bottom row: religion (left) + map (right)
  '<div class="chart-row">',
  '<div class="chart-card">',
  plotly_div("uk-relig", plotly_to_json({
    # Include "Not answered" as a bar; use same color palette as Australia
    uk_rel_colors <- c(
      "No religion"    = "#d4a943",
      "Muslim"         = "#1a4e72",
      "Christian"      = "#4a8c6f",
      "Other religion" = "#7b5ea7",
      "Jewish"         = "#e07b54",
      "Buddhist"       = "#c4793a",
      "Sikh"           = "#2ca089",
      "Hindu"          = "#8bbdde",
      "Not answered"   = "#b0b0b0"
    )
    relig_sorted <- relig %>% arrange(count)
    relig_sorted$religion <- factor(relig_sorted$religion, levels = relig_sorted$religion)
    relig_total <- sum(relig_sorted$count)
    bar_colors <- uk_rel_colors[as.character(relig_sorted$religion)]
    bar_colors[is.na(bar_colors)] <- "#b0b0b0"
    plot_ly(relig_sorted, y = ~religion, x = ~count, type = "bar",
      orientation = "h", marker = list(color = bar_colors),
      text = sprintf("<b>%s</b><br>%s (%.1f%%)",
        relig_sorted$religion,
        format(relig_sorted$count, big.mark = ","),
        relig_sorted$count / relig_total * 100),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = "<b>Religion of Iran-Born Residents<br>in England and Wales, 2021</b>",
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = "", tickfont = list(size = 11)),
      margin = list(l = 110, r = 20, t = 55, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)
  }), "430px", source = ONS_CUSTOM),
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in the United Kingdom</div>',
  plotly_div("uk-region-map", plotly_to_json(p_uk_map), "430px",
    source = paste0("Source: ", ONS_LINK, "; ", SCOT_LINK)),
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("United Kingdom: Population", pop_body), "docs/pages/uk-population.html")
cat("  Done\n")

# ============================================================================
# UK-WORKEDU
# ============================================================================
cat("Building uk-workedu...\n")

# --- Economic activity horizontal bar ---
# Combine three student categories into one; match Germany's 4-category scheme
econ_agg <- econ %>%
  mutate(group = case_when(
    category == "Employed" ~ "Employed",
    category == "Unemployed" ~ "Unemployed",
    category == "Inactive" ~ "Inactive",
    grepl("Student", category) ~ "Student"
  )) %>%
  group_by(group) %>%
  summarize(count = sum(count), .groups = "drop")

# Order: Student at bottom, then Inactive, Unemployed, Employed at top
econ_order <- c("Student", "Inactive", "Unemployed", "Employed")
econ_agg$group <- factor(econ_agg$group, levels = econ_order)
econ_agg <- econ_agg %>% arrange(group)
econ_total_16plus <- sum(econ_agg$count)

# Colors matching Germany pattern: employed=coral, unemployed=red, inactive=grey, student=light grey
econ_colors <- c("Student" = "#c8c8c8", "Inactive" = "#b0b0b0",
                 "Unemployed" = "#b05050", "Employed" = "#c4793a")

p_econ <- plot_ly(econ_agg, y = ~group, x = ~count, type = "bar",
  orientation = "h", marker = list(color = econ_colors[as.character(econ_agg$group)]),
  text = sprintf("<b>%s</b><br>%s (%.1f%%)",
    econ_agg$group,
    format(econ_agg$count, big.mark = ","),
    econ_agg$count / econ_total_16plus * 100),
  hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Economic Activity of Iran-Born<br>Residents in England and Wales, 2021</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 11)),
    margin = list(l = 100, r = 20, t = 55, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Qualification horizontal bar ---
qual_order <- c("Other qualifications", "No qualifications", "Level 1 (GCSEs)",
                "Level 2 (5+ GCSEs / O levels)", "Level 3 (A levels)",
                "Level 4+ (Degree or higher)")
qual$category <- factor(qual$category, levels = qual_order)
qual <- qual %>% arrange(category)
qual_total <- sum(qual$count)

# Blue gradient from light (low) to dark (high); grey for "Other"
qual_colors <- c("#b0b0b0", "#d4e6f1", "#c6dbef", "#8bbdde", "#5a9bd5", "#1a4e72")

p_qual <- plot_ly(qual, y = ~category, x = ~count, type = "bar",
  orientation = "h", marker = list(color = qual_colors),
  text = sprintf("<b>%s</b><br>%s (%.1f%%)",
    qual$category,
    format(qual$count, big.mark = ","),
    qual$count / qual_total * 100),
  hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Highest Qualification of Iran-Born<br>Residents in England and Wales, 2021</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", tickfont = list(size = 10)),
    margin = list(l = 200, r = 20, t = 55, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# Key stats for text cards
employed <- econ_agg$count[econ_agg$group == "Employed"]
unemployed <- econ_agg$count[econ_agg$group == "Unemployed"]
inactive <- econ_agg$count[econ_agg$group == "Inactive"]
students <- econ_agg$count[econ_agg$group == "Student"]
active_total <- employed + unemployed
employment_rate <- round(employed / active_total * 100, 1)

degree_plus <- qual$count[qual$category == "Level 4+ (Degree or higher)"]
degree_pct <- round(degree_plus / qual_total * 100, 1)

workedu_body <- paste0(
  '<div class="page-content">',
  '<div class="text-card pt1" style="text-align:center;">',
  sprintf('<div style="font-size:32px; font-weight:700; color:#1a4e72;">%s%%</div>', employment_rate),
  '<div style="font-size:14px; color:#555; margin-top:4px; font-weight:600;">Employment Rate</div>',
  sprintf('<div style="font-size:12px; color:#888; margin-top:6px; line-height:1.5;">Among economically active Iran-born residents aged 16+ in England and Wales, 2021.</div>'),
  '</div>',
  '<div class="text-card pt2" style="text-align:center;">',
  sprintf('<div style="font-size:32px; font-weight:700; color:#1a4e72;">%s%%</div>', degree_pct),
  '<div style="font-size:14px; color:#555; margin-top:4px; font-weight:600;">Bachelor\u2019s Degree or Higher</div>',
  sprintf('<div style="font-size:12px; color:#888; margin-top:6px; line-height:1.5;">Iran-born residents aged 16+ in England and Wales, 2021.</div>'),
  '</div>',
  '<div class="chart-card pc1">',
  plotly_div("uk-econ", plotly_to_json(p_econ), "380px", source = ONS_CUSTOM),
  '</div>',
  '<div class="chart-card pc2">',
  plotly_div("uk-qual", plotly_to_json(p_qual), "380px", source = ONS_CUSTOM),
  '</div>',
  '</div>'
)

writeLines(page_template("United Kingdom: Work & Education", workedu_body),
           "docs/pages/uk-workedu.html")
cat("  Done\n")

cat(sprintf("\nUK: %s Iran-born, %s%% employed, %s%% degree+\n",
  format(uk_total, big.mark = ","), employment_rate, degree_pct))

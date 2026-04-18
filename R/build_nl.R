# Build Netherlands pages from CBS StatLine Iran extracts.
# Run from deployment repo root:
#   Rscript R/build_nl.R
#
# Input:  data/netherlands/*.csv, data/netherlands/nl_provinces.geojson
# Output: docs/pages/nl-population.html, nl-immigration.html, nl-workinc.html
#
# Extract first via: Rscript R/nl_export/extract_cbs.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/netherlands"

# --- Helpers (canonical pattern, from build_germany.R / build_uk.R) ----------
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
  list(data   = toJSON(b$x$data, auto_unbox = TRUE),
       layout = toJSON(b$x$layout, auto_unbox = TRUE),
       config = toJSON(b$x$config, auto_unbox = TRUE))
}

plotly_div <- function(id, json, height = "500px", source = NULL, legend_html = NULL) {
  init_js <- sprintf(
    'var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox",showTips:true});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  chart <- sprintf(
    '<div id="%s" style="width:100%%;height:%s;touch-action:manipulation;"></div>\n<script>(function(){%s})();</script>',
    id, height, init_js)
  if (!is.null(legend_html)) {
    chart <- paste0(chart, '\n', legend_html)
  }
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
CBS_LINK <- "<a href='https://opendata.cbs.nl/statline/' target='_blank' style='color:#2774AE;'>CBS StatLine</a>"
CBS_SOURCE <- paste0("Source: ", CBS_LINK, " &mdash; Statistics Netherlands, Population Register 2025")
CBS_LF_SOURCE <- paste0("Source: ", CBS_LINK, " &mdash; Statistics Netherlands, Labour Force Survey 2021&ndash;2024")

# --- Load data ---------------------------------------------------------------
cat("Loading Netherlands CBS extracts...\n")
hl    <- read.csv(file.path(DATA_DIR, "nl_headline.csv"), stringsAsFactors = FALSE)
trend <- read.csv(file.path(DATA_DIR, "nl_trend.csv"), stringsAsFactors = FALSE)
prov  <- read.csv(file.path(DATA_DIR, "nl_province.csv"), stringsAsFactors = FALSE)
lf    <- read.csv(file.path(DATA_DIR, "nl_labourforce.csv"), stringsAsFactors = FALSE)

nl_total <- hl$count[hl$category == "total"]
nl_gen1  <- hl$count[hl$category == "gen1"]
nl_gen2  <- hl$count[hl$category == "gen2"]


# =============================================================================
# NL-POPULATION
# =============================================================================
cat("Building nl-population...\n")

# --- Province choropleth map -------------------------------------------------
geojson_path <- file.path(DATA_DIR, "nl_provinces.geojson")
has_geojson <- file.exists(geojson_path)

if (has_geojson) {
  nl_geojson <- fromJSON(geojson_path, simplifyVector = FALSE)

  p_nl_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = nl_geojson,
      locations = prov$province_code,
      z = prov$count,
      featureidkey = "properties.statcode",
      text = sprintf("<b>%s</b><br>%s Iranian-origin<br>%.1f%% of total",
        prov$province_name,
        format(prov$count, big.mark = ","),
        prov$count / nl_total * 100),
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                        c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.92)
    ) %>% layout(
      mapbox = list(
        style = "carto-positron",
        center = list(lon = 5.3, lat = 52.15),
        zoom = 5.8
      ),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  map_html <- plotly_div("nl-prov-map", plotly_to_json(p_nl_map), "500px",
    source = CBS_SOURCE)
} else {
  # Fallback: horizontal bar chart if GeoJSON is missing
  prov$province_name <- factor(prov$province_name, levels = rev(prov$province_name))
  p_nl_bar <- plot_ly(prov, y = ~province_name, x = ~count, type = "bar",
    orientation = "h", marker = list(color = "#2774AE"),
    text = sprintf("<b>%s</b><br>%s (%.1f%%)", prov$province_name,
      format(prov$count, big.mark = ","), prov$count / nl_total * 100),
    hoverinfo = "text", textposition = "none") %>%
    layout(
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = ""),
      margin = list(l = 120, r = 20, t = 10, b = 20),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)
  map_html <- plotly_div("nl-prov-bar", plotly_to_json(p_nl_bar), "500px",
    source = CBS_SOURCE)
}

# --- Historical Iran-born trend 1990-2025 (UN + Eurostat) --------------------
# Eurostat migr_pop3ctb gives Iran-born stock annually 1999-2025.
# Extend back to 1990 with UN Migrant Stock.
eurostat_all <- read.csv("data/europe/iran_born_combined.csv",
                         stringsAsFactors = FALSE)
euro_nl <- eurostat_all %>%
  filter(geo == "NL") %>%
  mutate(year = as.integer(year), iran_born = as.integer(value)) %>%
  select(year, iran_born) %>%
  arrange(year)

# Add UN 1990, 1995 data points
un_global <- read.csv("data/global/stocks_countries.csv", stringsAsFactors = FALSE)
un_nl <- un_global[grepl("Netherlands", un_global$destination), ]
if (nrow(un_nl) > 0) {
  un_pts <- data.frame(year = c(1990L, 1995L),
    iran_born = c(un_nl$X1990, un_nl$X1995))
  un_pts <- un_pts[!un_pts$year %in% euro_nl$year, ]
  euro_nl <- bind_rows(un_pts, euro_nl) %>% arrange(year)
}

p_hist <- plot_ly(euro_nl, x = ~year, y = ~iran_born, type = "scatter",
    mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2.5),
    marker = list(color = "#1a4e72", size = 5),
    text = sprintf("<b>%d</b><br>%s Iran-born",
      euro_nl$year, format(euro_nl$iran_born, big.mark = ",")),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iran-Born Population in the Netherlands,<br>1990\u20132025</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
HIST_SOURCE <- paste0("Source: ", EURO_LINK, ", Iran-born population stock, 1999\u20132025")

# --- Arrival year chart (from residence duration) ----------------------------
arrival <- read.csv(file.path(DATA_DIR, "nl_arrival_year.csv"),
                    stringsAsFactors = FALSE)
arrival$label <- ifelse(arrival$arrival_year <= 1965,
                        "\u22641965", as.character(arrival$arrival_year))
arrival_agg <- aggregate(count ~ label, data = arrival, FUN = sum)
arrival_agg$year_num <- ifelse(arrival_agg$label == "\u22641965", 1965L,
                               as.integer(arrival_agg$label))
arrival_agg <- arrival_agg[order(arrival_agg$year_num), ]
total_arr <- sum(arrival_agg$count)
arrival_agg$cumulative <- cumsum(arrival_agg$count)
arrival_agg$cum_pct <- round(arrival_agg$cumulative / total_arr * 100, 1)
arrival_agg$x_num <- seq_len(nrow(arrival_agg)) - 1L
max_bar <- max(arrival_agg$count)

p_arrival <- plot_ly() %>%
  add_bars(data = arrival_agg, x = ~x_num, y = ~count,
    marker = list(color = "#2774AE",
      line = list(color = "#1a4e72", width = 0.3)),
    text = sprintf("<b>%s</b><br>%s Iran-born residents<br>Cumulative: %.0f%%",
      arrival_agg$label, format(arrival_agg$count, big.mark = ","),
      arrival_agg$cum_pct),
    hoverinfo = "text", textposition = "none", showlegend = FALSE, name = "Residents") %>%
  add_trace(data = arrival_agg, x = ~x_num,
    y = ~cum_pct / 100 * max_bar,
    type = "scatter", mode = "lines",
    yaxis = "y2",
    line = list(color = "lightblue", width = 2),
    text = sprintf("<b>By %s:</b> %.0f%% cumulative",
      arrival_agg$label, arrival_agg$cum_pct),
    hoverinfo = "text", showlegend = FALSE, name = "Cumulative",
    inherit = FALSE) %>%
  layout(
    title = list(
      text = "<b>Iran-Born Residents in the Netherlands<br>by Implied Year of Arrival</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickangle = -45, tickfont = list(size = 9),
      tickvals = arrival_agg$x_num[seq(1, nrow(arrival_agg), by = 5)],
      ticktext = arrival_agg$label[seq(1, nrow(arrival_agg), by = 5)]),
    yaxis = list(title = "", tickformat = ",", tickfont = list(size = 10)),
    yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
      range = c(0, max_bar * 1.05),
      tickvals = seq(0, max_bar, length.out = 5),
      ticktext = c("0%", "25%", "50%", "75%", "100%"),
      tickfont = list(size = 10)),
    margin = list(t = 55, b = 70, r = 40),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

CBS_DUR_SOURCE <- paste0("Source: ", CBS_LINK,
  " &mdash; Statistics Netherlands, residence duration, January 2025")

# --- Generation boxes ---------------------------------------------------------
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
  make_gen_box(nl_gen1, paste0(round(nl_gen1 / nl_total * 100), "% of total"),
    "First generation", "Born in Iran", "#1a4e72"),
  make_gen_box(nl_gen2, paste0(round(nl_gen2 / nl_total * 100), "% of total"),
    "Second generation", "Born in NL, parent(s) born in Iran", "#5a9bd5"),
  '</div>')

# --- Assemble nl-population page (Germany layout) ----------------------------
# Top row: headline (left) + generation grid (right)
# Bottom row: historical trend (left) + province map (right)
randstad <- sum(prov$count[prov$province_code %in% c("PV27", "PV28", "PV26")])
randstad_pct <- round(randstad / nl_total * 100)

# No legend override needed — single trace with showlegend=FALSE

pop_body <- paste0(
  # Top row: headline + generation grid
  '<div class="chart-row">',
  '<div class="headline">',
  '<div class="label">Estimated Iranian-Origin Population in the Netherlands</div>',
  '<div class="number">', format(nl_total, big.mark = ","), '</div>',
  '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on population registers maintained by ',
  CBS_LINK, ', Statistics Netherlands, January 2025</div>',
  '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
  '<p style="margin-bottom:8px;">The Netherlands uses population registers rather than a traditional census. A person is classified as Iranian-origin if they meet at least one of:</p>',
  '<ul style="padding-left:20px; margin:0; line-height:2;">',
  '<li><strong>Born in Iran</strong> <span style="color:#888;">&mdash; first generation</span></li>',
  '<li><strong>Born in the Netherlands</strong> with a mother or father born in Iran <span style="color:#888;">&mdash; second generation</span></li>',
  '</ul>',
  '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">Third-generation residents (Dutch-born with Dutch-born parents) are not counted as Iranian-origin in CBS statistics. The classification is derived from birth records, not self-reported ethnicity or language.</p>',
  '</div>',
  '</div>',
  '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
  gen_boxes,
  sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', CBS_SOURCE),
  '</div>',
  '</div>',

  # Bottom row: tabbed (historical trend | arrival year) + province map
  '<div class="chart-row">',
  '<div class="chart-card">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'nl-tab-hist\',this,\'pop-tabs\')">Population Over Time</button>',
  '<button class="tab-btn" onclick="switchTab(\'nl-tab-arrival\',this,\'pop-tabs\')">Implied Arrival Year</button>',
  '</div>',
  '<div id="nl-tab-hist" class="tab-panel active" data-group="pop-tabs">',
  plotly_div("nl-hist", plotly_to_json(p_hist), "430px", source = HIST_SOURCE),
  '</div>',
  '<div id="nl-tab-arrival" class="tab-panel" data-group="pop-tabs">',
  plotly_div("nl-arrival", plotly_to_json(p_arrival), "430px", source = CBS_DUR_SOURCE),
  '</div>',
  '</div>',
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Geographic Distribution in the Netherlands</div>',
  map_html,
  '</div>',
  '</div>'
)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)
writeLines(page_template("Netherlands: Population", pop_body, has_tabs = TRUE),
           "docs/pages/nl-population.html")
cat("  Done\n")


# =============================================================================
# NL-WORKINC (page-content: 2 text + 2 tabbed chart cells)
# =============================================================================
cat("Building nl-workinc...\n")

latest <- lf[lf$year == max(lf$year), ]
income <- read.csv(file.path(DATA_DIR, "nl_income.csv"), stringsAsFactors = FALSE)

# --- Labour participation trend -----------------------------------------------
p_participation <- plot_ly(lf, x = ~year, y = ~participation_rate,
  type = "scatter", mode = "lines+markers",
  line = list(color = "#1a4e72", width = 3),
  marker = list(color = "#1a4e72", size = 8),
  text = sprintf("<b>%d</b><br>Participation: %d%%<br>%s employed / %s total (15\u201374)",
    lf$year, lf$participation_rate,
    format(lf$employed, big.mark = ","),
    format(lf$total_15_74, big.mark = ",")),
  hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Labour Force Participation<br>in the Netherlands</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickmode = "array", tickvals = lf$year, dtick = 1),
    yaxis = list(title = "", range = c(50, 70), ticksuffix = "%"),
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Employment type stacked bar ----------------------------------------------
p_emp <- plot_ly() %>%
  add_trace(x = lf$year, y = lf$permanent_pct, type = "bar",
    name = "Permanent", marker = list(color = "#1a4e72"),
    text = sprintf("<b>%d</b><br>Permanent: %d%%", lf$year, lf$permanent_pct),
    hoverinfo = "text", textposition = "none") %>%
  add_trace(x = lf$year, y = lf$flexible_pct, type = "bar",
    name = "Flexible", marker = list(color = "#5a9bd5"),
    text = sprintf("<b>%d</b><br>Flexible: %d%%", lf$year, lf$flexible_pct),
    hoverinfo = "text", textposition = "none") %>%
  add_trace(x = lf$year, y = lf$selfemployed_pct, type = "bar",
    name = "Self-employed", marker = list(color = "#e6a756"),
    text = sprintf("<b>%d</b><br>Self-employed: %d%%", lf$year, lf$selfemployed_pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    barmode = "stack",
    title = list(text = "<b>Employment Type in the Netherlands</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", tickmode = "array", tickvals = lf$year, dtick = 1),
    yaxis = list(title = "", range = c(0, 105), ticksuffix = "%"),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

emp_leg <- make_html_legend(c("Permanent" = "#1a4e72", "Flexible" = "#5a9bd5",
                              "Self-employed" = "#e6a756"))

# --- Income comparison --------------------------------------------------------
p_income <- plot_ly() %>%
  add_trace(x = income$year, y = income$nl_avg_income_k, type = "scatter",
    mode = "lines+markers", name = "NL average",
    line = list(color = "#999999", width = 2, dash = "dash"),
    marker = list(color = "#999999", size = 5),
    text = sprintf("<b>%d</b><br>NL average: \u20ac%s",
      income$year, format(income$nl_avg_income_k * 1000, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  add_trace(x = income$year, y = income$iran_avg_income_k, type = "scatter",
    mode = "lines+markers", name = "Iranian",
    line = list(color = "#c4793a", width = 3),
    marker = list(color = "#c4793a", size = 7),
    text = sprintf("<b>%d</b><br>Iranian-origin: \u20ac%s<br>%d%% of NL average",
      income$year,
      format(income$iran_avg_income_k * 1000, big.mark = ","),
      round(income$iran_avg_income_k / income$nl_avg_income_k * 100)),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(
      text = "<b>Disposable Household Income<br>in the Netherlands (nominal \u20ac thousands)</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2),
    yaxis = list(title = "", tickprefix = "\u20ac", ticksuffix = "k"),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

inc_leg <- make_html_legend(c("Iranian-origin" = "#c4793a", "NL average" = "#999999"))

# --- Low-income share ---------------------------------------------------------
inc_valid <- income[!is.na(income$iran_low_income_pct) &
                    !is.na(income$nl_low_income_pct), ]
p_lowinc <- plot_ly() %>%
  add_trace(x = inc_valid$year, y = inc_valid$nl_low_income_pct,
    type = "scatter", mode = "lines+markers", name = "NL average",
    line = list(color = "#999999", width = 2, dash = "dash"),
    marker = list(color = "#999999", size = 5),
    text = sprintf("<b>%d</b><br>NL average: %.1f%%",
      inc_valid$year, inc_valid$nl_low_income_pct),
    hoverinfo = "text", textposition = "none") %>%
  add_trace(x = inc_valid$year, y = inc_valid$iran_low_income_pct,
    type = "scatter", mode = "lines+markers", name = "Iranian",
    line = list(color = "#c4793a", width = 3),
    marker = list(color = "#c4793a", size = 7),
    text = sprintf("<b>%d</b><br>Iranian-origin: %.1f%%",
      inc_valid$year, inc_valid$iran_low_income_pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Households with Low Income<br>in the Netherlands</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 2),
    yaxis = list(title = "", ticksuffix = "%"),
    showlegend = FALSE,
    margin = list(t = 40, b = 30),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

lowinc_leg <- make_html_legend(c("Iranian-origin" = "#c4793a", "NL average" = "#999999"))

CBS_INC_SOURCE <- paste0("Source: ", CBS_LINK,
  " &mdash; Statistics Netherlands, household income and wealth, 2011&ndash;2024")

latest_inc <- income[income$year == max(income$year[!is.na(income$iran_avg_income_k)]), ]
inc_ratio  <- round(latest_inc$iran_avg_income_k / latest_inc$nl_avg_income_k * 100)
# Low-income trend (share of Iranian-origin households under the
# national low-income threshold). Has fallen substantially since the
# early 2010s peak.
li_latest_yr <- max(income$year[!is.na(income$iran_low_income_pct)])
li_latest    <- round(income$iran_low_income_pct[income$year == li_latest_yr])
li_peak_row  <- income[which.max(income$iran_low_income_pct), ]
li_peak_yr   <- li_peak_row$year
li_peak      <- round(li_peak_row$iran_low_income_pct)

workinc_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of Iranian-origin residents aged 15&ndash;74 in the Netherlands are in the labour force (%d).</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Of those employed, %d%% hold permanent contracts, %d%% flexible, and %d%% are self-employed.</div>
  </div>', latest$participation_rate, latest$year,
    latest$permanent_pct, latest$flexible_pct, latest$selfemployed_pct),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">Iranian-origin households in the Netherlands earn about %d%% of the national average household disposable income (%d).</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">The share of Iranian-origin households below the low-income threshold has fallen from %d%% in %d to %d%% in %d.</div>
  </div>', inc_ratio, inc_ratio, latest_inc$year,
    li_peak, li_peak_yr, li_latest, li_latest_yr),

  # Chart cell 1: tabbed (Participation | Employment Type)
  '<div class="chart-card pc1">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'nl-tab-part\',this,\'work-tabs\')">Participation</button>',
  '<button class="tab-btn" onclick="switchTab(\'nl-tab-emp\',this,\'work-tabs\')">Employment Type</button>',
  '</div>',
  '<div id="nl-tab-part" class="tab-panel active" data-group="work-tabs">',
  plotly_div("nl-participation", plotly_to_json(p_participation), "380px",
    source = CBS_LF_SOURCE),
  '</div>',
  '<div id="nl-tab-emp" class="tab-panel" data-group="work-tabs">',
  plotly_div("nl-emptype", plotly_to_json(p_emp), "340px",
    source = CBS_LF_SOURCE, legend_html = emp_leg),
  '</div>',
  '</div>',

  # Chart cell 2: tabbed (Income | Low Income)
  '<div class="chart-card pc2">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'nl-tab-inc\',this,\'inc-tabs\')">Disposable Income</button>',
  '<button class="tab-btn" onclick="switchTab(\'nl-tab-low\',this,\'inc-tabs\')">Low Income Share</button>',
  '</div>',
  '<div id="nl-tab-inc" class="tab-panel active" data-group="inc-tabs">',
  plotly_div("nl-income", plotly_to_json(p_income), "340px",
    source = CBS_INC_SOURCE, legend_html = inc_leg),
  '</div>',
  '<div id="nl-tab-low" class="tab-panel" data-group="inc-tabs">',
  plotly_div("nl-lowinc", plotly_to_json(p_lowinc), "340px",
    source = CBS_INC_SOURCE, legend_html = lowinc_leg),
  '</div>',
  '</div>',
  '</div>'
)

writeLines(page_template("Netherlands: Work & Income", workinc_body, has_tabs = TRUE),
           "docs/pages/nl-workinc.html")
cat("  Done\n")


# --- Summary ------------------------------------------------------------------
cat(sprintf("\nNetherlands: %s Iranian-origin (%s 1st gen + %s 2nd gen)\n",
  format(nl_total, big.mark = ","),
  format(nl_gen1, big.mark = ","),
  format(nl_gen2, big.mark = ",")))
cat(sprintf("Province map: %d provinces, Randstad share: %d%%\n",
  nrow(prov), randstad_pct))
cat(sprintf("Labour force: %d%% participation rate (%d)\n",
  latest$participation_rate, latest$year))
cat(sprintf("Income: Iranian avg \u20ac%sk vs NL avg \u20ac%sk (%d%%)\n",
  latest_inc$iran_avg_income_k, latest_inc$nl_avg_income_k, inc_ratio))

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

# --- Helpers (canonical pattern, from build_nl.R) ----------------------------
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
    title = list(text = "<b>Age Distribution of Iranian-Origin<br>Population in Israel, 2024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", autorange = TRUE, categoryorder = "array",
      categoryarray = age_order),
    barmode = "group",
    showlegend = FALSE,
    margin = list(t = 50, b = 30, l = 50),
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
    text = sprintf("<b>%s</b><br>%s (%s%%)",
      gen1_det$age_group, format(gen1_det$count, big.mark = ","), gen1_det$pct),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iran-Born Population in Israel<br>by Age, 2024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = "", categoryorder = "array", categoryarray = gen1_order),
    showlegend = FALSE,
    margin = list(t = 50, b = 30, l = 50),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

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
    text = sprintf("<b>%s</b><br>Total: %s<br>1st gen: %s<br>2nd gen: %s",
      comp$country, format(comp$total, big.mark = ","),
      format(comp$gen1, big.mark = ","), format(comp$gen2, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Asian-Origin Groups in Israel, 2024</b>",
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", tickformat = ","),
    yaxis = list(title = ""),
    showlegend = FALSE,
    margin = list(t = 40, b = 30, l = 120),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

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
  '<div class="chart-card">',
  '<div class="section-title" style="margin-top:0;">Comparative Context</div>',
  plotly_div("il-comp", plotly_to_json(p_comp), "430px", source = CBS_SOURCE),
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

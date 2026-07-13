# Build Finland pages from Statistics Finland (StatFin) extracts.
# Run from deployment repo root:
#   Rscript R/build_finland.R
#
# Input:  data/finland/fi_trend.csv, fi_age_2025.csv, fi_headline.csv
# Output: docs/pages/fi-population.html      + fi-population.fa.html
#
# Bilingual (en + fa), following the build_armenia.R pattern. All user-facing
# strings come from R/i18n/strings_finland.R via tr(); numbers go through
# fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while the Persian
# edition renders RTL with Persian digits and the Vazirmatn face. Finland keeps
# its LOCAL page_template() for English; the fa edition is that same shell run
# through fa_shell(). The local template already uses the canonical body font
# token (font-family:"Montserrat",sans-serif) that fa_shell() substitutes on,
# so no font-token alignment step is needed (unlike build_armenia.R).
#
# Extract first via: Rscript R/fi_export/extract_statfin.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/finland"

source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# Finland string table (defines the global STR consumed by tr()).
source("R/i18n/strings_finland.R")

iframe_resize_script <- "<script>
window.addEventListener('load', function(){
  // ResizeObserver to communicate height to parent iframe
  if (window.parent !== window) {
    var send = function() {
      var h = document.documentElement.scrollHeight;
      window.parent.postMessage({type:'iframeHeight', height:h}, '*');
    };
    new ResizeObserver(send).observe(document.body);
    send();
  }
});
</script>"

page_template <- function(title, body_html) {
  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>', title, '</title>
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap" rel="stylesheet">
', plotly_script(body_html), '
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
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row, .text-row { grid-template-columns:1fr !important; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .chart-card { padding:10px; }
  .text-card { font-size:13px; padding:14px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
</head>
<body>
', body_html, '
', iframe_resize_script, '
</body>
</html>')
}

# --- i18n formatting helpers (build_nl.R / build_armenia.R pattern) -----------
# lnk():  in fa, isolate a Latin agency link/URL in <bdi> so bidi ordering is
#         correct; in en, pass through unchanged (keeps English byte-identical).
lnk <- function(x) if (is_fa()) bdi(x) else x

# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — so it reproduces format()'s common-width
#         PADDING (leading spaces) that the committed hover text relies on. In
#         fa it Persian-digits that same padded string.
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)   # keep ASCII "," thousands (Iranian economic-press convention)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles). Idempotent on already-Persian digits, so
#         safe to wrap fa_num() output. NEVER apply to HTML that carries CSS —
#         only to plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source links (language-independent) --------------------------------
STATFIN_LINK <- "<a href='https://stat.fi/en/' target='_blank' style='color:#2774AE;'>Statistics Finland</a>"

# --- Load data (ONCE — language-independent) ---------------------------------
cat("Loading Finland extracts...\n")
trend  <- read.csv(file.path(DATA_DIR, "fi_trend.csv"), stringsAsFactors = FALSE)
hl     <- read.csv(file.path(DATA_DIR, "fi_headline.csv"), stringsAsFactors = FALSE)
region <- read.csv(file.path(DATA_DIR, "fi_regions.csv"), stringsAsFactors = FALSE,
                   colClasses = c(geo_code = "character"))
# read.csv strips quotes and would coerce "01".."09" to integers 1..9,
# which then fail Plotly's string match against properties.maakunta = "01"..
region$geo_code <- sprintf("%02d", as.integer(region$geo_code))
trend  <- trend[order(trend$year), ]

fi_total  <- hl$count[hl$category == "total"]
fi_gen1   <- hl$count[hl$category == "gen1"]
fi_gen2   <- hl$count[hl$category == "gen2"]
data_yr   <- hl$year[hl$category == "total"]
fi_min_yr <- min(trend$year)

# Region geojson + shares (loaded/computed once — language-independent).
fi_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "fi_regions.geojson"),
                                 simplifyVector = FALSE)
region$pct <- round(region$count / sum(region$count) * 100, 1)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Finland [%s] ===\n", LANG))

  # --- Source citation strings (per language) --------------------------------
  STATFIN_BOX_SOURCE <- sprintf(tr("fi_src_box"),
    lnk(STATFIN_LINK), fa_num(data_yr, 0, big = FALSE))
  TREND_SOURCE <- sprintf(tr("fi_src_trend"),
    lnk(STATFIN_LINK), fa_num(fi_min_yr, 0, big = FALSE),
    fa_num(data_yr, 0, big = FALSE))
  MAP_SOURCE <- sprintf(tr("fi_src_map"),
    lnk(STATFIN_LINK), fa_num(data_yr, 0, big = FALSE))

  # ===========================================================================
  # FI-POPULATION
  # ===========================================================================
  cat("Building fi-population...\n")

  # --- Stacked-area trend: 1st gen + 2nd gen by year -------------------------
  # Stacked area (not bars) is the site standard for generation-split population
  # over time: it shows each generation's relative size as a continuous band.
  fi_hover <- htxt(sprintf(tr("fi_hist_hover"),
    fa_num(trend$year, 0, big = FALSE), fmtv(trend$gen1),
    fmtv(trend$gen2), fmtv(trend$gen1 + trend$gen2)))
  p_hist <- plot_ly(trend) %>%
    add_trace(x = ~year, y = ~gen1, type = "scatter", mode = "lines",
      stackgroup = "one", name = tr("fi_gen1_label"),
      fillcolor = "rgba(26,78,114,0.75)", line = list(color = "#1a4e72", width = 1),
      text = fi_hover, hoverinfo = "text") %>%
    add_trace(x = ~year, y = ~gen2, type = "scatter", mode = "lines",
      stackgroup = "one", name = tr("fi_gen2_label"),
      fillcolor = "rgba(90,155,213,0.75)", line = list(color = "#5a9bd5", width = 1),
      text = fi_hover, hoverinfo = "text") %>%
    layout(
      title = list(
        text = htxt(sprintf(tr("fi_hist_title"),
          fa_num(fi_min_yr, 0, big = FALSE), fa_num(data_yr, 0, big = FALSE))),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 5),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 50, b = 50),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Region choropleth map (Iran-born by maakunta, 2025) -------------------
  p_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = fi_geojson,
      locations = region$geo_code, z = region$count,
      featureidkey = "properties.maakunta",
      text = htxt(sprintf(tr("fi_map_hover"),
        region$name, fmtv(region$count), fa_num(region$pct, 0))),
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                        c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron",
        center = list(lon = 26, lat = 65), zoom = 3.6),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Generation boxes ------------------------------------------------------
  gen_boxes <- paste0(
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">', tr("fi_gen_box_title"), '</div>',
    '<div style="display:flex; gap:12px; margin-top:12px;">',
    make_gen_box(fmtv(fi_gen1),
      sprintf(tr("fi_gen_pct_of_total"), fa_num(round(fi_gen1 / fi_total * 100), 0)),
      tr("fi_gen1_label"), tr("fi_gen1_sub"), "#1a4e72"),
    make_gen_box(fmtv(fi_gen2),
      sprintf(tr("fi_gen_pct_of_total"), fa_num(round(fi_gen2 / fi_total * 100), 0)),
      tr("fi_gen2_label"), tr("fi_gen2_sub"), "#5a9bd5"),
    '</div>')

  # --- Assemble fi-population page --------------------------------------------
  pop_body <- paste0(
    # Top row: headline + generation boxes
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("fi_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(fi_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("fi_pop_headline_caption"),
      lnk(STATFIN_LINK), fa_num(data_yr, 0, big = FALSE)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("fi_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("fi_pop_idbox_bullet1"), '</li>',
    '<li>', tr("fi_pop_idbox_bullet2"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("fi_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
    gen_boxes,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>',
            STATFIN_BOX_SOURCE),
    '</div>',
    '</div>',
    # Bottom row: trend (left) + region map (right) — map always bottom-right
    # per .claude/rules/new-country-framework.md.
    '<div class="chart-row">',
    '<div class="chart-card">',
    plotly_div("fi-hist", pj(p_hist), "430px", source = TREND_SOURCE),
    '</div>',
    '<div class="chart-card">',
    '<div class="section-title">', tr("fi_geo_section_title"), '</div>',
    plotly_div("fi-map", pj(p_map), "430px", source = MAP_SOURCE),
    '</div>',
    '</div>'
  )

  html <- page_template(tr("fi_pop_title"), pop_body)
  if (is_fa()) html <- fa_shell(html)
  fname_pop <- if (is_fa()) "docs/pages/fi-population.fa.html" else "docs/pages/fi-population.html"
  writeLines(html, fname_pop)
  cat("  Done\n")
}

cat(sprintf("\nFinland: %s Iranian-origin (%s 1st gen + %s 2nd gen), %d; %d-year series\n",
  format(fi_total, big.mark = ","),
  format(fi_gen1, big.mark = ","),
  format(fi_gen2, big.mark = ","),
  data_yr, nrow(trend)))

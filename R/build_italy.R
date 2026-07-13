# Build Italy pages from ISTAT + Eurostat extracts.
# Run from deployment repo root:
#   Rscript R/build_italy.R
#
# Input:  data/italy/*.csv
# Output: docs/pages/it-population.html      + it-population.fa.html
#
# Bilingual (en + fa), following the build_armenia.R / build_nl.R pattern. All
# user-facing strings come from R/i18n/strings_italy.R via tr(); numbers go
# through fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while the
# Persian edition renders RTL with Persian digits and the Vazirmatn face.
# Italy keeps its LOCAL page_template() for English; the fa edition is that same
# shell run through fa_shell() (Italy's body font token already matches the
# canonical anchor fa_shell() expects, so no realignment is needed).
#
# Extract first via: Rscript R/it_export/extract_istat.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/italy"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, make_gen_box().
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# Italy string table (defines the global STR consumed by tr()).
source("R/i18n/strings_italy.R")

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
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row, .text-row { grid-template-columns:1fr !important; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
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
#         format(x, big.mark = ",") — reproducing format()'s common-width
#         PADDING that the committed hover text relies on. In fa it
#         Persian-digits that same string (ASCII "," thousands kept).
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles). Idempotent; safe to wrap fa_num() output.
#         NEVER apply to HTML that carries CSS — only to plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source links (language-independent) --------------------------------
ISTAT_LINK <- "<a href='https://demo.istat.it/' target='_blank' style='color:#2774AE;'>ISTAT</a>"
EURO_LINK  <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"

# --- Load data (ONCE — language-independent) ----------------------------------
cat("Loading Italy extracts...\n")
hl     <- read.csv(file.path(DATA_DIR, "it_headline.csv"), stringsAsFactors = FALSE)
trend  <- read.csv(file.path(DATA_DIR, "it_trend.csv"), stringsAsFactors = FALSE)
region <- read.csv(file.path(DATA_DIR, "it_region.csv"), stringsAsFactors = FALSE,
                   colClasses = c(region_code = "character"))
# Ensure zero-padded 2-digit codes to match GeoJSON
region$region_code <- sprintf("%02d", as.integer(region$region_code))

geojson_path <- file.path(DATA_DIR, "it_regions.geojson")
has_geojson  <- file.exists(geojson_path)
if (!has_geojson) cat("  WARNING: it_regions.geojson not found, map will be skipped\n")
if (has_geojson) it_geojson <- fromJSON(geojson_path, simplifyVector = FALSE)

it_total  <- hl$count[hl$category == "total"]
it_male   <- hl$count[hl$category == "male"]
it_female <- hl$count[hl$category == "female"]
data_yr   <- hl$year[1]

# Regional shares (language-independent) --------------------------------------
region <- region %>% arrange(iran_born)
region$pct <- round(region$iran_born / it_total * 100, 1)

# Top region share (for console summary) --------------------------------------
top_region <- region$region_name[region$iran_born == max(region$iran_born)]
top_region_pct <- round(max(region$iran_born) / it_total * 100)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Italy [%s] ===\n", LANG))

  # --- Source citation strings (per language) ---------------------------------
  ISTAT_SOURCE <- sprintf(tr("it_src_istat"), lnk(ISTAT_LINK))
  HIST_SOURCE  <- sprintf(tr("it_src_hist"),  lnk(EURO_LINK))

  # --- Historical trend (Eurostat, line + markers) ----------------------------
  p_hist <- plot_ly(trend, x = ~year, y = ~iran_born, type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 5),
      text = htxt(sprintf(tr("it_hist_hover"),
        fa_num(trend$year, 0, big = FALSE), fmtv(trend$iran_born))),
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(tr("it_hist_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 2),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Regional choropleth map ------------------------------------------------
  if (has_geojson) {
    p_map <- plot_ly() %>%
      add_trace(type = "choroplethmapbox",
        geojson = it_geojson,
        locations = region$region_code,
        z = region$iran_born,
        featureidkey = "properties.reg_istat_code",
        text = htxt(sprintf(tr("it_map_hover"),
          region$region_name,
          fmtv(region$iran_born),
          fa_num(region$pct, 1))),
        hoverinfo = "text",
        colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                          c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
        showscale = TRUE,
        colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
        marker = list(line = list(color = "white", width = 1), opacity = 0.85)
      ) %>% layout(
        mapbox = list(style = "carto-positron",
          center = list(lon = 12.5, lat = 42.5), zoom = 4.5),
        margin = list(t = 10, b = 10, l = 0, r = 0),
        paper_bgcolor = "white"
      ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

    map_html <- plotly_div("it-map", pj(p_map), "500px", source = ISTAT_SOURCE)
  } else {
    map_html <- paste0('<div class="section-title">', tr("it_map_unavailable"), '</div>')
  }

  # --- Sex breakdown boxes ----------------------------------------------------
  sex_boxes <- paste0(
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">', tr("it_sexbox_title"), '</div>',
    '<div style="display:flex; gap:12px; margin-top:12px;">',
    make_gen_box(fmtv(it_male),
      sprintf(tr("it_gen_pct_of_total"), fa_num(round(it_male / it_total * 100), 0)),
      tr("it_male_label"), tr("it_male_sub"), "#1a4e72"),
    make_gen_box(fmtv(it_female),
      sprintf(tr("it_gen_pct_of_total"), fa_num(round(it_female / it_total * 100), 0)),
      tr("it_female_label"), tr("it_female_sub"), "#5a9bd5"),
    '</div>')

  # --- Assemble it-population page --------------------------------------------
  pop_body <- paste0(
    # Top row: headline (left) + sex boxes (right)
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("it_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(it_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("it_pop_headline_caption"), lnk(ISTAT_LINK),
      fa_num(data_yr, 0, big = FALSE)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("it_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("it_pop_idbox_bullet1"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("it_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
    sex_boxes,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', ISTAT_SOURCE),
    '</div>',
    '</div>',

    # Bottom row: trend (left) + map (right)
    '<div class="chart-row">',
    '<div class="chart-card">',
    plotly_div("it-hist", pj(p_hist), "430px", source = HIST_SOURCE),
    '</div>',
    '<div class="chart-card">',
    '<div class="section-title" style="margin-top:0;">', tr("it_geo_section_title"), '</div>',
    map_html,
    '</div>',
    '</div>'
  )

  html <- page_template(tr("it_pop_title"), pop_body)
  if (is_fa()) html <- fa_shell(html)
  fname_pop <- if (is_fa()) "docs/pages/it-population.fa.html" else "docs/pages/it-population.html"
  writeLines(html, fname_pop)
  cat("  Done\n")
}

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nItaly: %s Iran-born (%s male, %s female)\n",
  format(it_total, big.mark = ","),
  format(it_male, big.mark = ","),
  format(it_female, big.mark = ",")))
cat(sprintf("Regions: %d, top: %s (%d%%)\n",
  nrow(region), top_region, top_region_pct))
cat(sprintf("Trend: %d-%d (%s to %s)\n",
  min(trend$year), max(trend$year),
  format(min(trend$iran_born), big.mark = ","),
  format(max(trend$iran_born), big.mark = ",")))

# Build Austria pages from Eurostat Iran-born data.
# Run from deployment repo root:
#   Rscript R/build_austria.R
#
# Input:  data/austria/at_trend.csv, at_bundesland.csv, at_bundesland.geojson
# Output: docs/pages/at-population.html   + at-population.fa.html
#
# Bilingual (en + fa), following the build_armenia.R / build_nl.R pattern. All
# user-facing strings come from R/i18n/strings_austria.R via tr(); numbers go
# through fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while the
# Persian edition renders RTL with Persian digits and the Vazirmatn face.
#
# Austria uses the SHARED standard-cluster page_template() from R/_helpers.R
# (it already emits the exact anchors fa_shell() substitutes on:
# `<html lang="en">`, `font-family:"Montserrat",sans-serif`, `</head>`,
# `</body>`), so — unlike build_armenia.R — no local template and no font-token
# realignment are needed. The en path calls page_template() exactly as before;
# the fa path is that same canonical shell run through fa_shell().
#
# Extract first via: Rscript R/at_export/extract_eurostat_at.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/austria"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# make_gen_box(), page_template(), iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# Austria string table (defines the global STR consumed by tr()).
source("R/i18n/strings_austria.R")

# has_tabs = FALSE, but the shared page_template() resolves tab_switch_script
# from the global env at call time, so define the (unused) hook to be safe.
tab_switch_script <- ""

# --- i18n formatting helpers (build_armenia.R pattern) ------------------------
# lnk():  in en, pass through unchanged (keeps English byte-identical); in fa,
#         isolate a Latin agency link in <bdi> so bidi ordering stays correct.
lnk <- function(x) if (is_fa()) bdi(x) else x

# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — so it reproduces format()'s common-width
#         PADDING (leading spaces) that the committed vectorized hover text
#         relies on. In fa it Persian-digits that same padded string.
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)   # keep ASCII "," thousands (Iranian economic-press convention)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles). Idempotent, and a no-op in en.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source links (language-independent) --------------------------------
STAT_AT_LINK <- "<a href='https://www.statistik.at/en/' target='_blank' style='color:#2774AE;'>Statistics Austria</a>"
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"

# --- Load data (ONCE — language-independent) ----------------------------------
cat("Loading Austria trend data...\n")
trend <- read.csv(file.path(DATA_DIR, "at_trend.csv"), stringsAsFactors = FALSE)
trend <- trend[order(trend$year), ]

at_latest <- trend$iran_born[nrow(trend)]
at_latest_yr <- trend$year[nrow(trend)]
at_min_yr <- min(trend$year)

# Bundesland census data (bars/map + sex breakdown)
bl <- read.csv(file.path(DATA_DIR, "at_bundesland.csv"), stringsAsFactors = FALSE)
bl <- bl[order(bl$iran_born), ]
bl$bundesland <- factor(bl$bundesland, levels = bl$bundesland)
bl_total <- sum(bl$iran_born)
bl$pct <- round(bl$iran_born / bl_total * 100, 1)

at_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "at_bundesland.geojson"),
                                 simplifyVector = FALSE)

at_male   <- sum(bl$male)
at_female <- sum(bl$female)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Austria [%s] ===\n", LANG))

  # --- Source citation strings (per language) ---------------------------------
  TREND_SOURCE  <- sprintf(tr("at_src_trend"),  lnk(EURO_LINK))
  CENSUS_SOURCE <- sprintf(tr("at_src_census"), lnk(EURO_LINK))

  # --- Historical trend line chart --------------------------------------------
  p_hist <- plot_ly(trend, x = ~year, y = ~iran_born, type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 5),
      text = htxt(sprintf(tr("at_hist_hover"),
        fa_num(trend$year, 0, big = FALSE), fmtv(trend$iran_born))),
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(
        text = htxt(sprintf(tr("at_hist_title"),
          fa_num(at_min_yr, 0, big = FALSE), fa_num(at_latest_yr, 0, big = FALSE))),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 5),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 50, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Bundesland choropleth map ----------------------------------------------
  p_bl_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = at_geojson,
      locations = bl$bundesland, z = bl$iran_born,
      featureidkey = "properties.name",
      text = htxt(sprintf(tr("at_map_hover"),
        bl$bundesland, fmtv(bl$iran_born), fa_num(bl$pct, 1))),
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                        c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron",
        center = list(lon = 13.5, lat = 47.6), zoom = 5.4),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Sex breakdown boxes (from Bundesland census data) ----------------------
  sex_boxes <- paste0(
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">',
    tr("at_sex_box_title"), '</div>',
    '<div style="display:flex; gap:12px; margin-top:12px;">',
    make_gen_box(at_male,
      sprintf(tr("at_sex_pct_of_total"), fa_num(round(at_male / bl_total * 100), 0)),
      tr("at_sex_male_label"), tr("at_sex_male_sub"), "#1a4e72"),
    make_gen_box(at_female,
      sprintf(tr("at_sex_pct_of_total"), fa_num(round(at_female / bl_total * 100), 0)),
      tr("at_sex_female_label"), tr("at_sex_female_sub"), "#5a9bd5"),
    '</div>')

  # --- Assemble at-population page (Italy format) -----------------------------
  pop_body <- paste0(
    # Top row: headline (left) + sex boxes card (right)
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("at_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(at_latest), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("at_pop_headline_caption"),
      lnk(STAT_AT_LINK), fa_num(at_latest_yr, 0, big = FALSE)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("at_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("at_pop_idbox_bullet1"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("at_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
    sex_boxes,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', CENSUS_SOURCE),
    '</div>',
    '</div>',

    # Bottom row: trend line chart (left) + choropleth map (right)
    '<div class="chart-row">',
    '<div class="chart-card">',
    plotly_div("at-hist", pj(p_hist), "430px", source = TREND_SOURCE),
    '</div>',
    '<div class="chart-card">',
    '<div class="section-title" style="margin-top:0;">', tr("at_geo_section_title"), '</div>',
    plotly_div("at-map", pj(p_bl_map), "430px", source = CENSUS_SOURCE),
    '</div>',
    '</div>'
  )

  html <- page_template(tr("at_pop_title"), pop_body, has_tabs = FALSE)
  if (is_fa()) html <- fa_shell(html)
  fname_pop <- if (is_fa()) "docs/pages/at-population.fa.html" else "docs/pages/at-population.html"
  writeLines(html, fname_pop)
  cat("  Done\n")
}

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nAustria: %s Iran-born (%d)\n",
  format(at_latest, big.mark = ","), at_latest_yr))
cat(sprintf("Trend: %d years (%d-%d)\n", nrow(trend), at_min_yr, at_latest_yr))

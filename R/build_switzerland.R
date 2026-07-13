# Build Switzerland page from BFS (Swiss Federal Statistical Office) extracts.
# Run from deployment repo root:
#   Rscript R/build_switzerland.R
#
# Input:  data/switzerland/*.csv, data/switzerland/ch_cantons.geojson
# Output: docs/pages/ch-population.html       + ch-population.fa.html
#
# Bilingual (en + fa), following the build_nl.R / build_norway.R pattern. All
# user-facing strings come from R/i18n/strings_switzerland.R via tr(); numbers
# go through fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while
# the Persian edition renders RTL with Persian digits and the Vazirmatn face.
# Switzerland is a standard-cluster builder (shared page_template), so the fa
# edition is produced by page_template_i18n(..., lang = "fa"). See .claude/rules
# for context.
#
# Extract first via: Rscript R/ch_export/extract_bfs.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/switzerland"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), page_template_i18n().
source("R/_helpers_i18n.R")
# Switzerland string table (defines the global STR consumed by tr()).
source("R/i18n/strings_switzerland.R")

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

# --- i18n formatting helpers (build_nl.R pattern) -----------------------------
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
  fa_digits(s)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles). Idempotent on already-Persian digits, so
#         safe to wrap fa_num() output. NEVER apply to HTML that carries CSS —
#         only to plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Source link (language-independent) --------------------------------------
BFS_LINK <- "<a href='https://www.bfs.admin.ch/bfs/en/home.html' target='_blank' style='color:#2774AE;'>BFS</a>"

# --- Load data (ONCE — language-independent) ---------------------------------
cat("Loading Switzerland BFS extracts...\n")
hl       <- read.csv(file.path(DATA_DIR, "ch_headline.csv"), stringsAsFactors = FALSE)
trend    <- read.csv(file.path(DATA_DIR, "ch_trend.csv"), stringsAsFactors = FALSE)
canton   <- read.csv(file.path(DATA_DIR, "ch_canton.csv"), stringsAsFactors = FALSE)
arrivals <- read.csv(file.path(DATA_DIR, "ch_arrivals.csv"), stringsAsFactors = FALSE)

ch_total   <- hl$count[hl$category == "total"]
ch_male    <- hl$count[hl$category == "male"]
ch_female  <- hl$count[hl$category == "female"]
ch_swiss   <- hl$count[hl$category == "swiss_citizen"]
ch_foreign <- hl$count[hl$category == "foreign"]
ch_iranian <- hl$count[hl$category == "iranian_citizen"]
data_yr    <- hl$year[1]

# --- Derived (language-independent) ------------------------------------------
# The cumulative line is computed against the *stock* of Iran-born residents
# (ch_trend), not against arrivals alone. Iran-born residents existed in
# Switzerland well before 2011, so a cumsum(arrivals) starting at 0 would
# misleadingly imply no Iranians were present in 2010. Instead: at year y,
# line value = stock[y] / stock[latest] * 100 = "share of current stock
# already present by end of year y".
arrivals <- arrivals[order(arrivals$year), ]
trend <- trend[order(trend$year), ]
total_arr <- sum(arrivals$count)
stock_final <- trend$total[trend$year == max(trend$year)]
# Line is anchored at arrivals years (2011-2024). At year Y, value =
# stock[Y] / stock[latest] * 100 = "share of current stock already present
# by end of year Y". The first point (2011) naturally lands above zero
# because a substantial Iran-born community existed before the chart
# starts; the line has no 2010 point so it aligns with the first bar.
stock_line <- merge(data.frame(year = arrivals$year), trend, by = "year", all.x = TRUE)
stock_line <- stock_line[order(stock_line$year), ]
stock_line$cum_pct <- round(stock_line$total / stock_final * 100, 1)
max_bar <- max(arrivals$count)

# Canton shares.
canton$pct <- round(canton$iran_born / ch_total * 100, 1)
ch_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "ch_cantons.geojson"),
                                 simplifyVector = FALSE)

swiss_pct  <- round(ch_swiss / ch_total * 100)
zurich_pct <- round(canton$iran_born[canton$canton_name == "Zürich"] / ch_total * 100)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Switzerland [%s] ===\n", LANG))

  # --- Source citation strings (per language) --------------------------------
  BFS_SOURCE     <- sprintf(tr("ch_src_bfs"),     lnk(BFS_LINK))
  BFS_IMM_SOURCE <- sprintf(tr("ch_src_bfs_imm"), lnk(BFS_LINK))

  pct_suffix <- tr("ch_axis_pct_suffix")

  # =========================================================================
  # CH-POPULATION
  # =========================================================================
  cat("Building ch-population...\n")

  # --- Population trend 2010-2024 (line + markers) -------------------------
  p_trend <- plot_ly(trend, x = ~year, y = ~total, type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 5),
      text = htxt(sprintf(tr("ch_trend_hover"),
        fa_num(trend$year, 0, big = FALSE), fmtv(trend$total))),
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(tr("ch_trend_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", dtick = 2),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      showlegend = FALSE,
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Annual arrivals 2011-2024 (bars + stock-share % line) ---------------
  p_arrivals <- plot_ly() %>%
    add_bars(data = arrivals, x = ~year, y = ~count,
      marker = list(color = "#2774AE",
        line = list(color = "#1a4e72", width = 0.3)),
      text = htxt(sprintf(tr("ch_arr_hover_bar"),
        fa_num(arrivals$year, 0, big = FALSE), fmtv(arrivals$count))),
      hoverinfo = "text", textposition = "none", showlegend = FALSE,
      name = tr("ch_arr_name_bar")) %>%
    add_trace(data = stock_line, x = ~year,
      y = ~cum_pct / 100 * max_bar,
      type = "scatter", mode = "lines",
      yaxis = "y2",
      line = list(color = "lightblue", width = 2),
      text = htxt(sprintf(tr("ch_arr_hover_line"),
        fa_num(stock_line$year, 0, big = FALSE), fa_num(stock_line$cum_pct, 0))),
      hoverinfo = "text", showlegend = FALSE, name = tr("ch_arr_name_line"),
      inherit = FALSE) %>%
    layout(
      title = list(
        text = htxt(tr("ch_arr_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", dtick = 2, range = c(2010.5, 2024.5)),
      yaxis = list(title = "", tickformat = ","),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
        range = c(0, max_bar * 1.05),
        tickvals = seq(0, max_bar, length.out = 5),
        ticktext = paste0(c("0", "25", "50", "75", "100"), pct_suffix),
        tickfont = list(size = 10)),
      showlegend = FALSE,
      margin = list(t = 40, b = 30, r = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Canton choropleth map -----------------------------------------------
  p_canton_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = ch_geojson,
      locations = canton$canton_name, z = canton$iran_born,
      featureidkey = "properties.name",
      text = htxt(sprintf(tr("ch_map_hover"),
        canton$canton_name, fmtv(canton$iran_born), fa_num(canton$pct, 1))),
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                        c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron",
        center = list(lon = 8.2, lat = 46.8), zoom = 5.8),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Assemble ch-population page -----------------------------------------
  pop_body <- paste0(
    # Top row: headline + canton choropleth
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("ch_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(ch_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("ch_pop_headline_caption"), lnk(BFS_LINK),
      fa_num(data_yr, 0, big = FALSE)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("ch_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("ch_pop_idbox_bullet1"), '</li>',
    '</ul>',
    sprintf('<p style="margin-top:10px; font-size:12px; color:#555; line-height:1.5;">%s</p>',
      sprintf(tr("ch_pop_cit_sentence"),
        fmtv(ch_total), fmtv(ch_swiss), fa_num(swiss_pct, 0),
        fmtv(ch_iranian), fmtv(ch_total - ch_swiss - ch_iranian))),
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("ch_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card">',
    plotly_div("ch-trend", pj(p_trend), "430px", source = BFS_SOURCE),
    '</div>',
    '</div>',

    # Bottom row: arrivals (left) + map (right)
    '<div class="chart-row">',
    '<div class="chart-card">',
    plotly_div("ch-arrivals", pj(p_arrivals), "430px", source = BFS_IMM_SOURCE),
    '</div>',
    '<div class="chart-card">',
    '<div class="section-title" style="margin-top:0;">', tr("ch_geo_section_title"), '</div>',
    plotly_div("ch-map", pj(p_canton_map), "430px", source = BFS_SOURCE),
    '</div>',
    '</div>'
  )

  fname_pop <- if (is_fa()) "docs/pages/ch-population.fa.html" else "docs/pages/ch-population.html"
  writeLines(page_template_i18n(tr("ch_pop_title"), pop_body, has_tabs = TRUE, lang = LANG),
             fname_pop)
  cat("  Done\n")
}

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nSwitzerland: %s Iran-born (M: %s, F: %s)\n",
  format(ch_total, big.mark = ","),
  format(ch_male, big.mark = ","),
  format(ch_female, big.mark = ",")))
cat(sprintf("Swiss citizens: %s (%d%%), Foreign: %s\n",
  format(ch_swiss, big.mark = ","), swiss_pct,
  format(ch_foreign, big.mark = ",")))
cat(sprintf("Cantons: %d, Zurich share: %d%%\n", nrow(canton), zurich_pct))
cat(sprintf("Trend: %s (%d) -> %s (%d)\n",
  format(trend$total[1], big.mark = ","), trend$year[1],
  format(trend$total[nrow(trend)], big.mark = ","), trend$year[nrow(trend)]))
cat(sprintf("Total arrivals 2011-2024: %s\n", format(total_arr, big.mark = ",")))

# Build Armenia page.
# Run from deployment repo root:
#   Rscript R/build_armenia.R
#
# Input:  data/armenia/am_trend.csv, am_headline.csv
# Output: docs/pages/am-population.html      + am-population.fa.html
#
# Bilingual (en + fa), following the build_nl.R pattern. All user-facing
# strings come from R/i18n/strings_armenia.R via tr(); numbers go through
# fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while the
# Persian edition renders RTL with Persian digits and the Vazirmatn face.
# Armenia keeps its LOCAL page_template() for English; the fa edition is that
# same shell run through fa_shell() (after aligning the local font token to
# the canonical anchor fa_shell() expects — fa path only, so English bytes
# never change).
#
# Extract first via: Rscript R/am_export/extract_armenia.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/armenia"

source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# Armenia string table (defines the global STR consumed by tr()).
source("R/i18n/strings_armenia.R")

# Match the standard page_template helper from other builders.
page_template <- function(title, body_html, has_tabs = FALSE) {
  extra_head <- ""
  tab_switch_script <- ""
  paste0('<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>', title, '</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700&display=swap" rel="stylesheet">
', plotly_script(body_html), '
<style>
body { font-family:"Montserrat",system-ui,sans-serif; color:#222; background:#f6f7f9; margin:0; padding:16px 14px 28px; box-sizing:border-box; overflow-x:hidden; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; overflow:hidden; min-width:0; }
.headline { background:white; border-radius:8px; padding:24px 22px; border:1px solid #e0e0e0; text-align:center; display:flex; flex-direction:column; justify-content:center; }
.headline .label { font-size:14px; color:#555; margin-bottom:4px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em; }
@media (max-width:900px) {
  .chart-row { grid-template-columns:1fr; }
  .headline { padding:18px; }
  .chart-card { padding:12px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
', extra_head, '
</head>
<body>
', body_html, '
', iframe_resize_script, '
</body>
</html>')
}

# --- i18n formatting helpers (build_nl.R pattern) -----------------------------
# lnk():  in fa, isolate a Latin agency link/URL in <bdi> so bidi ordering is
#         correct; in en, pass through unchanged (keeps English byte-identical).
lnk <- function(x) if (is_fa()) bdi(x) else x

# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — so it reproduces format()'s common-width
#         PADDING (leading spaces) that the committed hover text relies on. In
#         fa it Persian-digits that same padded string (separators -> U+066C).
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  gsub(",", "٬", fa_digits(s), fixed = TRUE)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles). Idempotent on already-Persian digits, so
#         safe to wrap fa_num() output. NEVER apply to HTML that carries CSS —
#         only to plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source links (language-independent) --------------------------------
ARMSTAT_LINK <- "<a href='https://armstat.am/en/?nid=82&id=2623' target='_blank' style='color:#2774AE;'>Armstat</a>"
UN_LINK <- "<a href='https://www.un.org/development/desa/pd/content/international-migrant-stock' target='_blank' style='color:#2774AE;'>UN DESA</a>"

# --- Load data (ONCE — language-independent) ----------------------------------
cat("Loading Armenia extracts...\n")
trend <- read.csv(file.path(DATA_DIR, "am_trend.csv"), stringsAsFactors = FALSE)
hl    <- read.csv(file.path(DATA_DIR, "am_headline.csv"), stringsAsFactors = FALSE)

am_total <- hl$count[hl$category == "iran_born"]
am_year  <- hl$year[hl$category == "iran_born"]
iran_cit <- hl$count[hl$category == "iranian_citizens"]

# UN 5-yearly series vs Armstat census-day points (split once; hover padding is
# computed per subset, matching the original per-trace format() call).
is_armstat <- grepl("Armstat", trend$source)
trend_un   <- trend %>% filter(!is_armstat)
trend_arm  <- trend %>% filter(is_armstat)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Armenia [%s] ===\n", LANG))

  # --- Source citation strings (per language) ---------------------------------
  TREND_SOURCE <- sprintf(tr("am_src_trend"), lnk(UN_LINK), lnk(ARMSTAT_LINK))

  # --- Historical trend chart (UN snapshots + Armstat censuses) ---------------
  # Use line+markers because the UN series is 5-yearly and the Armstat census
  # points are single census-day snapshots; bars would imply continuous
  # coverage. Both Armstat census points (2011 and 2022) align closely with
  # the UN series, corroborating the long-run decline.
  p_hist <- plot_ly() %>%
    add_trace(
      data = trend_un,
      x = ~year, y = ~iran_born,
      type = "scatter", mode = "lines+markers",
      name = tr("am_leg_un"),
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 8),
      text = htxt(sprintf(tr("am_hist_hover_un"),
        fa_num(trend_un$year, 0, big = FALSE), fmtv(trend_un$iran_born))),
      hoverinfo = "text"
    ) %>%
    add_trace(
      data = trend_arm,
      x = ~year, y = ~iran_born,
      type = "scatter", mode = "markers",
      name = tr("am_leg_armstat"),
      marker = list(color = "#c4793a", size = 12, symbol = "diamond",
                    line = list(color = "#1a4e72", width = 1.5)),
      text = htxt(sprintf(tr("am_hist_hover_armstat"),
        fa_num(trend_arm$year, 0, big = FALSE), fmtv(trend_arm$iran_born))),
      hoverinfo = "text"
    ) %>%
    layout(
      title = list(
        text = htxt(tr("am_hist_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 5),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 55, b = 60, l = 60, r = 40),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18,
                    font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Assemble am-population page ---------------------------------------------
  pop_body <- paste0(
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("am_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(am_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("am_pop_headline_caption"),
      fa_num(am_year, 0, big = FALSE), lnk(ARMSTAT_LINK)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("am_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:2;">',
    '<li>', tr("am_pop_idbox_bullet1"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("am_pop_thirdgen_note"), '</p>',
    sprintf('<p style="margin-top:8px; font-size:11px; color:#999; line-height:1.5;">%s</p>',
      sprintf(tr("am_pop_cit_intro"), fmtv(am_total))),
    '<ul style="padding-left:18px; margin:4px 0 0; font-size:11px; color:#999; line-height:1.7;">',
    sprintf('<li>%s</li>', sprintf(tr("am_pop_cit_iranian"), fmtv(iran_cit))),
    '<li>', tr("am_pop_cit_dual"), '</li>',
    sprintf('<li>%s</li>',
      sprintf(tr("am_pop_cit_naturalized"), fmtv(am_total - iran_cit - 249))),
    '</ul>',
    '<p style="margin-top:8px; font-size:11px; color:#999; line-height:1.5;">', tr("am_pop_un_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card">',
    plotly_div("am-hist", pj(p_hist), "430px", source = TREND_SOURCE),
    '</div>',
    '</div>'
  )

  html <- page_template(tr("am_pop_title"), pop_body)
  if (is_fa()) {
    # Align the local template's body font token to the canonical anchor that
    # fa_shell() substitutes on (fa path only — English bytes never change).
    html <- gsub('font-family:"Montserrat",system-ui,sans-serif',
                 'font-family:"Montserrat",sans-serif', html, fixed = TRUE)
    html <- fa_shell(html)
  }
  fname_pop <- if (is_fa()) "docs/pages/am-population.fa.html" else "docs/pages/am-population.html"
  writeLines(html, fname_pop)
  cat("  Done\n")
}

cat(sprintf("\nArmenia: %s Iran-born (Armstat %d), %s Iranian citizens; UN trend %d-%d\n",
  format(am_total, big.mark = ","), am_year,
  format(iran_cit, big.mark = ","),
  min(trend$year), max(trend$year)))

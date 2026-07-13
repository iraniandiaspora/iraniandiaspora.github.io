# Build France pages from INSEE extract.
# Run from deployment repo root:
#   Rscript R/build_france.R
#
# Input:  data/france/fr_trend.csv, fr_headline.csv
# Output: docs/pages/fr-population.html      + fr-population.fa.html
#
# Bilingual (en + fa), following the build_armenia.R pattern. All user-facing
# strings come from R/i18n/strings_france.R via tr(); numbers go through
# fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while the Persian
# edition renders RTL with Persian digits and the Vazirmatn face. France keeps
# its LOCAL page_template() (note the .wide-card class) for English; the fa
# edition is that same shell run through fa_shell(). The local body font token
# is already the canonical "Montserrat",sans-serif anchor fa_shell() expects, so
# no font realignment is needed (fa path only never touches English bytes).
#
# Extract first via: Rscript R/fr_export/extract_insee.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/france"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# France string table (defines the global STR consumed by tr()).
source("R/i18n/strings_france.R")

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
.wide-card { background:white; border-radius:8px; padding:24px 28px; border:1px solid #e0e0e0; margin-bottom:20px; font-size:14px; line-height:1.7; color:#333; }
.wide-card h3 { font-size:16px; font-weight:600; color:#1a4e72; margin-bottom:10px; }
.wide-card ul { padding-left:20px; margin:6px 0 10px; }
.wide-card li { margin-bottom:4px; }
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

# --- i18n formatting helpers (build_armenia.R pattern) ------------------------
# lnk():  in fa, isolate a Latin agency link/URL in <bdi> so bidi ordering is
#         correct; in en, pass through unchanged (keeps English byte-identical).
lnk <- function(x) if (is_fa()) bdi(x) else x

# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — so it reproduces format()'s common-width
#         PADDING (leading spaces) that the committed hover text relies on. In
#         fa it Persian-digits that same string (ASCII "," thousands kept).
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles). Idempotent, so safe to wrap fa_num()
#         output. NEVER apply to HTML that carries CSS — only plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source link (language-independent) --------------------------------
INSEE_LINK <- "<a href='https://www.insee.fr/fr/statistiques/6478089' target='_blank' style='color:#2774AE;'>INSEE</a>"

# --- Load data (ONCE — language-independent) ---------------------------------
cat("Loading France extracts...\n")
trend <- read.csv(file.path(DATA_DIR, "fr_trend.csv"), stringsAsFactors = FALSE)
hl    <- read.csv(file.path(DATA_DIR, "fr_headline.csv"), stringsAsFactors = FALSE)
trend <- trend[order(trend$year), ]

fr_total  <- hl$count[hl$category == "total"]
data_yr   <- hl$year[hl$category == "total"]
fr_min_yr <- min(trend$year)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building France [%s] ===\n", LANG))

  # --- Source citation string (per language) ----------------------------------
  INSEE_SOURCE <- sprintf(tr("fr_src_insee"), lnk(INSEE_LINK))

  # --- Historical trend (INSEE, line + markers; gap at 2018) -----------------
  p_hist <- plot_ly(trend, x = ~year, y = ~iran_born, type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 6),
      text = htxt(sprintf(tr("fr_hist_hover"),
        fa_num(trend$year, 0, big = FALSE), fmtv(trend$iran_born))),
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(
        text = htxt(sprintf(tr("fr_hist_title"),
          fa_num(fr_min_yr, 0, big = FALSE), fa_num(data_yr, 0, big = FALSE))),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 2),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 50, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Assemble fr-population page ---------------------------------------------
  pop_body <- paste0(
    # Top row: headline with identification box (left) + trend chart (right)
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("fr_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(fr_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("fr_pop_headline_caption"),
      lnk(INSEE_LINK), fa_num(data_yr, 0, big = FALSE)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("fr_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("fr_pop_idbox_bullet1"), '</li>',
    '</ul>',
    sprintf('<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">%s</p>',
      sprintf(tr("fr_pop_law_note"), fa_num(data_yr, 0, big = FALSE))),
    '</div>',
    '</div>',
    '<div class="chart-card">',
    plotly_div("fr-hist", pj(p_hist), "430px", source = INSEE_SOURCE),
    '</div>',
    '</div>'
  )

  html <- page_template(tr("fr_pop_title"), pop_body)
  if (is_fa()) html <- fa_shell(html)
  fname_pop <- if (is_fa()) "docs/pages/fr-population.fa.html" else "docs/pages/fr-population.html"
  writeLines(html, fname_pop)
  cat("  Done\n")
}

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nFrance: %s Iran-born (%d)\n",
  format(fr_total, big.mark = ","), data_yr))
cat(sprintf("Trend: %d rows (%d-%d)\n",
  nrow(trend), fr_min_yr, data_yr))

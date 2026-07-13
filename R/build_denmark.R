# Build Denmark pages from DST (Statistics Denmark) extracts.
# Run from deployment repo root:
#   Rscript R/build_denmark.R
#
# Input:  data/denmark/*.csv, data/europe/iran_born_combined.csv
# Output: docs/pages/dk-population.html      + dk-population.fa.html
#         docs/pages/dk-workinc.html         + dk-workinc.fa.html
#
# Bilingual (en + fa), following the build_nl.R pattern. All user-facing
# strings come from R/i18n/strings_denmark.R via tr(); numbers go through
# fa_num()/fmtv() so the English editions stay BYTE-IDENTICAL while the Persian
# editions render RTL with Persian digits and the Vazirmatn face. Denmark uses
# the shared standard-cluster page_template(), so the fa edition is produced via
# page_template_i18n() (en delegates to page_template() byte-for-byte; fa runs
# that same shell through fa_shell()).
#
# Extract first via: Rscript R/dk_export/extract_dst.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/denmark"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, make_gen_box(),
# make_html_legend(), hbar_over_labels(), pct_lab(), OKABE_ITO, CAT_OTHER.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), page_template_i18n(), fa_shell().
source("R/_helpers_i18n.R")
# Denmark string table (defines the global STR consumed by tr() + DK_SECTOR_FA).
source("R/i18n/strings_denmark.R")

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

# --- Source citation links (language-independent; Danish official names kept) -
DST_LINK <- "<a href='https://www.dst.dk/en/' target='_blank' style='color:#2774AE;'>Statistics Denmark (DST)</a>"
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"

# --- Load data (ONCE — language-independent) ---------------------------------
cat("Loading Denmark DST extracts...\n")
hl      <- read.csv(file.path(DATA_DIR, "dk_headline.csv"), stringsAsFactors = FALSE)
trend   <- read.csv(file.path(DATA_DIR, "dk_trend.csv"), stringsAsFactors = FALSE)
region  <- read.csv(file.path(DATA_DIR, "dk_region.csv"), stringsAsFactors = FALSE)
industry <- read.csv(file.path(DATA_DIR, "dk_industry.csv"), stringsAsFactors = FALSE)

dk_total <- hl$count[hl$category == "total"]
dk_gen1  <- hl$count[hl$category == "gen1"]
dk_gen2  <- hl$count[hl$category == "gen2"]
data_yr  <- hl$year[1]

# Clean region names: strip "Region " prefix
region$region_name <- sub("^Region ", "", region$region_name)

# Translate industry names from Danish to English
industry_en <- c(
  "Landbrug, skovbrug og fiskeri" = "Agriculture & Fishing",
  "Råstofindvinding" = "Mining & Quarrying",
  "Industri" = "Manufacturing",
  "Energiforsyning" = "Energy Supply",
  "Vandforsyning og renovation" = "Water & Waste",
  "Bygge og anlæg" = "Construction",
  "Handel" = "Trade & Retail",
  "Transport" = "Transport",
  "Hoteller og restauranter" = "Hotels & Restaurants",
  "Information og kommunikation" = "IT & Telecom",
  "Finansiering og forsikring" = "Finance & Insurance",
  "Ejendomshandel og udlejning" = "Real Estate",
  "Videnservice" = "Professional Services",
  "Rejsebureauer, rengøring og anden operationel service" = "Admin & Support",
  "Offentlig administration, forsvar og politi" = "Public Admin & Defense",
  "Undervisning" = "Education",
  "Sundhed og socialvæsen" = "Health & Social Work",
  "Kultur og fritid" = "Arts & Recreation",
  "Andre serviceydelser mv." = "Other Services"
)
industry$sector_en <- ifelse(industry$sector_name %in% names(industry_en),
                             industry_en[industry$sector_name],
                             industry$sector_name)

# --- Region derived fields (language-independent) ----------------------------
dk_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "dk_regions.geojson"),
                                 simplifyVector = FALSE)
region$join_name <- gsub("^Region ", "", region$region_name)
region$pct <- round(region$count / dk_total * 100, 1)
hovedstaden_pct <- round(region$count[region$region_name == "Hovedstaden"] / dk_total * 100)

# --- Industry prep (ordering / factoring / palette — language-independent) ---
industry <- industry[order(industry$count), ]  # ascending for horizontal bar
industry$sector_en <- factor(industry$sector_en, levels = industry$sector_en)
total_employed <- industry$total_employed[1]

# Shared Okabe-Ito categorical palette for the 8 largest sectors; smaller
# sectors muted to grey so the palette never repeats a hue (best practice caps
# categorical color at ~8). `industry` is ascending-count, so the LAST 8 rows
# are the largest — those get color, the smaller head is grey.
n_ind <- nrow(industry)
dk_ind_colors <- c(rep(CAT_OTHER, max(0, n_ind - 8)), rev(OKABE_ITO)[seq_len(min(8, n_ind))])
dk_ord <- match(levels(industry$sector_en), as.character(industry$sector_en))
dk_xmax <- max(industry$count) * 1.15
top3 <- industry[order(-industry$count), ][1:3, ]

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# LANG is the loop variable; at top-level Rscript scope it lives in the global
# env, which is where the i18n helpers read it.
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Denmark [%s] ===\n", LANG))

  # --- Source citation strings (per language) --------------------------------
  DST_SOURCE     <- sprintf(tr("dk_src_pop"), lnk(DST_LINK))
  DST_EMP_SOURCE <- sprintf(tr("dk_src_emp"), lnk(DST_LINK))

  # Industry sector display labels (en unchanged; fa translated per-sector).
  sector_levels    <- levels(industry$sector_en)                 # ascending order
  sector_disp      <- if (is_fa()) unname(DK_SECTOR_FA[sector_levels]) else sector_levels
  row_sector_disp  <- if (is_fa()) unname(DK_SECTOR_FA[as.character(industry$sector_en)]) else as.character(industry$sector_en)
  top_sector_disp  <- if (is_fa()) unname(DK_SECTOR_FA[as.character(top3$sector_en)]) else as.character(top3$sector_en)

  # ===========================================================================
  # DK-POPULATION
  # ===========================================================================
  cat("Building dk-population...\n")

  # --- Historical trend 1980-2026 (stacked area: gen1 + gen2) ----------------
  p_hist <- plot_ly() %>%
    add_trace(data = trend, x = ~year, y = ~gen1, type = "scatter",
      mode = "lines", stackgroup = "one", fillcolor = "rgba(26,78,114,0.7)",
      line = list(color = "#1a4e72", width = 1),
      name = "Immigrants",
      text = htxt(sprintf(tr("dk_hist_hover_gen1"),
        fa_num(trend$year, 0, big = FALSE), fmtv(trend$gen1),
        fmtv(trend$gen2), fmtv(trend$total))),
      hoverinfo = "text", textposition = "none") %>%
    add_trace(data = trend, x = ~year, y = ~gen2, type = "scatter",
      mode = "lines", stackgroup = "one", fillcolor = "rgba(90,155,213,0.7)",
      line = list(color = "#5a9bd5", width = 1),
      name = "Descendants",
      text = htxt(sprintf(tr("dk_hist_hover_gen2"),
        fa_num(trend$year, 0, big = FALSE), fmtv(trend$gen2))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("dk_hist_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", dtick = 5),
      yaxis = list(title = "", tickformat = ","),
      showlegend = FALSE,
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  dk_trend_leg <- make_html_legend(setNames(c("#1a4e72", "#5a9bd5"),
                                   c(tr("dk_gen1_label"), tr("dk_gen2_label"))))

  # --- Region choropleth map -------------------------------------------------
  p_region <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = dk_geojson,
      locations = region$join_name, z = region$count,
      featureidkey = "properties.name",
      text = htxt(sprintf(tr("dk_region_hover"),
        region$region_name, fmtv(region$count), fa_num(region$pct, 1))),
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                        c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron",
        center = list(lon = 10.5, lat = 56), zoom = 5.5),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Generation boxes ------------------------------------------------------
  gen_boxes <- paste0(
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">', tr("dk_gen_box_title"), '</div>',
    '<div style="display:flex; gap:12px; margin-top:12px;">',
    make_gen_box(fmtv(dk_gen1),
      sprintf(tr("dk_gen_pct_of_total"), fa_num(round(dk_gen1 / dk_total * 100), 0)),
      tr("dk_gen1_label"), tr("dk_gen1_sub"), "#1a4e72"),
    make_gen_box(fmtv(dk_gen2),
      sprintf(tr("dk_gen_pct_of_total"), fa_num(round(dk_gen2 / dk_total * 100), 0)),
      tr("dk_gen2_label"), tr("dk_gen2_sub"), "#5a9bd5"),
    '</div>')

  # --- Assemble dk-population page --------------------------------------------
  pop_body <- paste0(
    # Top row: headline + generation grid
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("dk_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(dk_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("dk_pop_headline_caption"), lnk(DST_LINK),
      htxt(fa_num(data_yr, 0, big = FALSE))), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("dk_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("dk_pop_idbox_bullet1"), '</li>',
    '<li>', tr("dk_pop_idbox_bullet2"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("dk_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
    gen_boxes,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', DST_SOURCE),
    '</div>',
    '</div>',

    # Bottom row: historical trend + region bar
    '<div class="chart-row">',
    '<div class="chart-card">',
    plotly_div("dk-hist", pj(p_hist), "400px", source = DST_SOURCE,
      legend_html = dk_trend_leg),
    '</div>',
    '<div class="chart-card">',
    '<div class="section-title" style="margin-top:0;">', tr("dk_geo_section_title"), '</div>',
    plotly_div("dk-region", pj(p_region), "500px", source = DST_SOURCE),
    '</div>',
    '</div>'
  )

  fname_pop <- if (is_fa()) "docs/pages/dk-population.fa.html" else "docs/pages/dk-population.html"
  writeLines(page_template_i18n(tr("dk_pop_title"), pop_body, lang = LANG),
             fname_pop)
  cat("  Done\n")


  # ===========================================================================
  # DK-WORKINC (industry employment)
  # ===========================================================================
  cat("Building dk-workinc...\n")

  # --- Industry horizontal bar chart -----------------------------------------
  dk_end_text <- pct_lab(industry$pct[dk_ord])
  if (is_fa()) dk_end_text <- gsub("%", "٪", fa_digits(dk_end_text))  # N% -> ۲۴٪ (axis-style suffix)
  ov_dk <- hbar_over_labels(sector_disp,
    ends = industry$count[dk_ord],
    end_text = dk_end_text)
  dk_xrange <- if (isTRUE(ov_dk$xreversed)) c(dk_xmax, 0) else c(0, dk_xmax)
  p_industry <- plot_ly(industry, y = ~sector_en, x = ~count, type = "bar",
      orientation = "h", marker = list(color = dk_ind_colors),
      text = htxt(sprintf(tr("dk_industry_hover"),
        row_sector_disp, fmtv(industry$count), fa_num(industry$pct, 1))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("dk_industry_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE, range = dk_xrange),
      yaxis = ov_dk$yaxis,
      annotations = ov_dk$annotations, bargap = ov_dk$bargap,
      margin = list(l = ov_dk$margin_l, r = 20, t = ov_dk$margin_t, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Text card (precompute pieces so the HTML/CSS template stays identical) -
  w_big  <- sprintf(tr("dk_wi_bignum"), fa_num(top3$pct[1], 0))
  w_prim <- htxt(sprintf(tr("dk_wi_primary"), top_sector_disp[1]))
  w_next <- tr("dk_wi_next")
  w_b1   <- htxt(sprintf(tr("dk_wi_bullet"), top_sector_disp[2], fa_num(top3$pct[2], 0)))
  w_b2   <- htxt(sprintf(tr("dk_wi_bullet"), top_sector_disp[3], fa_num(top3$pct[3], 0)))
  w_foot <- tr("dk_wi_footnote")

  workinc_body <- paste0(
    '<div class="chart-row">',
    sprintf('<div class="text-card pt1" style="text-align:center; display:flex; flex-direction:column; justify-content:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:380px; margin-left:auto; margin-right:auto;">%s</div>
    <ul style="margin:6px auto 0; padding-left:18px; max-width:380px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
    <div style="font-size:11px; color:#999; margin-top:14px; line-height:1.5; max-width:380px; margin-left:auto; margin-right:auto;">%s</div>
  </div>',
      w_big, w_prim, w_next, w_b1, w_b2, w_foot),
    '<div class="chart-card">',
    plotly_div("dk-industry", pj(p_industry), ov_dk$height, source = DST_EMP_SOURCE),
    '</div>',
    '</div>'
  )

  fname_wi <- if (is_fa()) "docs/pages/dk-workinc.fa.html" else "docs/pages/dk-workinc.html"
  writeLines(page_template_i18n(tr("dk_workinc_title"), workinc_body, lang = LANG),
             fname_wi)
  cat("  Done\n")
}

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nDenmark: %s Iranian-origin (%s immigrants + %s descendants)\n",
  format(dk_total, big.mark = ","),
  format(dk_gen1, big.mark = ","),
  format(dk_gen2, big.mark = ",")))
cat(sprintf("Regions: %d, Hovedstaden share: %d%%\n",
  nrow(region), hovedstaden_pct))
cat(sprintf("Employed: %s across %d sectors\n",
  format(total_employed, big.mark = ","), nrow(industry)))

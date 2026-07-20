# Build Türkiye pages from Eurostat extracts.
# Run from deployment repo root:
#   Rscript R/build_turkey.R
#
# Inputs:  data/turkey/{tr_trend.csv, tr_citizens.csv, tr_age_2025.csv,
#          tr_headline.csv, tr_permits.csv}
# Outputs: docs/pages/tr-population.html    + tr-population.fa.html
#          docs/pages/tr-immigration.html   + tr-immigration.fa.html
#
# Bilingual (en + fa), following the build_switzerland.R / build_nl.R pattern.
# All user-facing strings come from R/i18n/strings_turkey.R via tr(); numbers go
# through fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while the
# Persian edition renders RTL with Persian digits and the Vazirmatn face.
# Türkiye is a standard-cluster builder (shared page_template), so the fa
# edition is produced by page_template_i18n(..., lang = "fa").
#
# Extract first via: Rscript R/tr_export/extract_eurostat_tr.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/turkey"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), page_template_i18n().
source("R/_helpers_i18n.R")
# Türkiye string table (defines the global STR consumed by tr()).
source("R/i18n/strings_turkey.R")

# --- i18n formatting helpers (build_nl.R pattern) -----------------------------
# lnk():  in fa, isolate a Latin agency link/URL in <bdi> so bidi ordering is
#         correct; in en, pass through unchanged (keeps English byte-identical).
lnk <- function(x) if (is_fa()) bdi(x) else x

# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — so it reproduces format()'s common-width
#         PADDING that the committed hover text relies on. In fa it Persian-
#         digits that same string (ASCII "," thousands kept).
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

# --- Source links (language-independent) -------------------------------------
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"
TUIK_LINK <- "<a href='https://data.tuik.gov.tr/' target='_blank' style='color:#2774AE;'>TÜİK</a>"
GOC_LINK  <- "<a href='https://www.goc.gov.tr/ikamet-izinleri' target='_blank' style='color:#2774AE;'>Presidency of Migration Management (Göç İdaresi)</a>"

# --- Load data (ONCE — language-independent) ---------------------------------
cat("Loading Türkiye extracts...\n")
trend   <- read.csv(file.path(DATA_DIR, "tr_trend.csv"), stringsAsFactors = FALSE)
cit     <- read.csv(file.path(DATA_DIR, "tr_citizens.csv"), stringsAsFactors = FALSE)
age_df  <- read.csv(file.path(DATA_DIR, "tr_age_2025.csv"), stringsAsFactors = FALSE)
hl      <- read.csv(file.path(DATA_DIR, "tr_headline.csv"), stringsAsFactors = FALSE)
trend   <- trend[order(trend$year), ]
cit     <- cit[order(cit$year), ]

tr_total  <- hl$count[hl$category == "total"]
tr_male   <- hl$count[hl$category == "male"]
tr_female <- hl$count[hl$category == "female"]
data_yr   <- hl$year[hl$category == "total"]
tr_min_yr <- min(trend$year)

# --- Residence permits by type (Göç İdaresi snapshot, May 2026) --------------
# Language-independent: keep a stable permit_type key for ordering/colours; the
# display labels are built per language inside the loop. Row order (bottom→top
# on the horizontal bar) is Other/long-term, Family, Student, Short-term.
permit <- read.csv(file.path(DATA_DIR, "tr_permits.csv"), stringsAsFactors = FALSE)
permit_total <- permit$count[permit$permit_type == "total"]
pb <- permit %>%
  filter(permit_type != "total") %>%
  mutate(
    permit_type = factor(permit_type,
      levels = c("other_long_term", "family", "student", "short_term")),
    pct = round(count / permit_total * 100)
  ) %>%
  arrange(permit_type)
permit_short_pct   <- pb$pct[pb$permit_type == "short_term"]
permit_student_pct <- pb$pct[pb$permit_type == "student"]

# --- Iranian-citizen growth (language-independent) ---------------------------
cit_peak    <- cit$iranian_citizens[which.max(cit$iranian_citizens)]
cit_peak_yr <- cit$year[which.max(cit$iranian_citizens)]
cit_2014    <- cit$iranian_citizens[cit$year == 2014]
cit_growth  <- round(cit_peak / cit_2014, 1)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Türkiye [%s] ===\n", LANG))

  # --- Source citation strings (per language) --------------------------------
  HEADLINE_SOURCE <- sprintf(tr("tr_src_headline"), lnk(EURO_LINK))
  TREND_SOURCE    <- sprintf(tr("tr_src_trend"), lnk(EURO_LINK), lnk(TUIK_LINK))
  AGE_SOURCE      <- sprintf(tr("tr_src_age"), lnk(EURO_LINK))
  CIT_SOURCE      <- sprintf(tr("tr_src_citizens"), lnk(EURO_LINK), lnk(TUIK_LINK))
  PERMIT_SOURCE   <- sprintf(tr("tr_src_permits"), lnk(GOC_LINK))

  # =========================================================================
  # TR-POPULATION
  # =========================================================================
  cat("Building tr-population...\n")

  # --- Figure 1: Annual Iran-born stock (line + markers) -------------------
  p_hist <- plot_ly(trend, x = ~year, y = ~total, type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 6),
      text = htxt(sprintf(tr("tr_hist_hover"),
        fa_num(trend$year, 0, big = FALSE), fmtv(trend$total))),
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(
        text = htxt(sprintf(tr("tr_hist_title"),
          fa_num(tr_min_yr, 0, big = FALSE), fa_num(data_yr, 0, big = FALSE))),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", tickmode = "array", tickvals = trend$year),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Figure 2: Age distribution 2025, M vs F (horizontal bars) -----------
  # Age axis: oldest (85+) at top, youngest (Under 5) at bottom — standard
  # population-chart convention. Plotly renders the first factor level at the
  # bottom of the y-axis, so levels go youngest → oldest here.
  age_disp <- age_df$age_label
  if (is_fa()) age_disp[age_df$age_label == "Under 5"] <- tr("tr_age_under5")
  age_fac <- factor(age_disp, levels = age_disp)

  # Trace order: female added first (so within each pair the female bar sits
  # BELOW the male bar), male second (on top). The `traceorder = "reversed"`
  # legend flag keeps "Male" listed first in the legend.
  p_age <- plot_ly() %>%
    add_trace(
      x = age_df$female, y = age_fac,
      type = "bar", orientation = "h",
      name = tr("tr_label_female"),
      marker = list(color = "#c4793a"),
      text = htxt(sprintf(tr("tr_age_hover_female"),
        as.character(age_fac), fmtv(age_df$female))),
      hoverinfo = "text", textposition = "none"
    ) %>%
    add_trace(
      x = age_df$male, y = age_fac,
      type = "bar", orientation = "h",
      name = tr("tr_label_male"),
      marker = list(color = "#1a4e72"),
      text = htxt(sprintf(tr("tr_age_hover_male"),
        as.character(age_fac), fmtv(age_df$male))),
      hoverinfo = "text", textposition = "none"
    ) %>%
    layout(
      title = list(
        text = htxt(tr("tr_age_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = "", tickfont = list(size = 10),
                   ticks = "outside", ticklen = 8,
                   tickcolor = "rgba(0,0,0,0)"),
      margin = list(t = 50, b = 30, l = 70, r = 20),
      barmode = "group", bargap = 0.2,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08,
                    traceorder = "reversed"),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)
  # fa/RTL: mirror the grouped horizontal bar (BBC Persian). Grouped (not a
  # back-to-back butterfly), so both sexes grow from one right-hand baseline.
  # fa-only. Native centered legend identifies colors, not positions — unchanged.
  if (is_fa()) p_age <- p_age %>% layout(
    xaxis = list(autorange = "reversed"),
    yaxis = list(side = "right"),
    margin = list(t = 50, b = 30, l = 20, r = 70))

  # --- Sex breakdown boxes (from headline counts) --------------------------
  sex_boxes <- paste0(
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">',
    tr("tr_sex_box_title"), '</div>',
    '<div style="display:flex; gap:12px; margin-top:12px;">',
    make_gen_box(tr_male,
      sprintf(tr("tr_sex_pct_of_total"), round(tr_male / tr_total * 100)),
      tr("tr_label_male"), tr("tr_sublabel_men"), "#1a4e72"),
    make_gen_box(tr_female,
      sprintf(tr("tr_sex_pct_of_total"), round(tr_female / tr_total * 100)),
      tr("tr_label_female"), tr("tr_sublabel_women"), "#5a9bd5"),
    '</div>')

  # --- Assemble tr-population page (Italy format; no map available) --------
  pop_body <- paste0(
    # Top row: headline (left) + sex boxes card (right)
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("tr_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(tr_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("tr_pop_headline_caption"),
      lnk(TUIK_LINK), fa_num(data_yr, 0, big = FALSE)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("tr_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("tr_pop_idbox_bullet1"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("tr_pop_thirdgen_note"), '</p>',
    '<p style="margin-top:8px; font-size:11px; color:#999; line-height:1.5;">', tr("tr_pop_un_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
    sex_boxes,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>',
            HEADLINE_SOURCE),
    '</div>',
    '</div>',

    # Bottom row: Iran-born trend (left) + age chart (right)
    '<div class="chart-row">',
    '<div class="chart-card">',
    plotly_div("tr-hist", pj(p_hist), "540px", source = TREND_SOURCE),
    '</div>',
    '<div class="chart-card">',
    plotly_div("tr-age", pj(p_age), "540px", source = AGE_SOURCE),
    '</div>',
    '</div>'
  )

  fname_pop <- if (is_fa()) "docs/pages/tr-population.fa.html" else "docs/pages/tr-population.html"
  writeLines(page_template_i18n(tr("tr_pop_title"), pop_body, lang = LANG),
             fname_pop)
  cat("  Done (tr-population)\n")

  # =========================================================================
  # TR-IMMIGRATION
  # =========================================================================
  cat("Building tr-immigration...\n")

  # --- Figure 3: Iranian citizens 2014-2025 (bar) --------------------------
  p_cit <- plot_ly(cit, x = ~year, y = ~iranian_citizens, type = "bar",
      marker = list(color = "#2774AE",
                    line = list(color = "#1a4e72", width = 0.4)),
      text = htxt(sprintf(tr("tr_cit_hover"),
        fa_num(cit$year, 0, big = FALSE), fmtv(cit$iranian_citizens))),
      hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
    layout(
      title = list(
        text = htxt(tr("tr_cit_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", tickmode = "array", tickvals = cit$year),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Figure 4: Residence permits by type (Göç İdaresi snapshot) ----------
  pb$label <- factor(
    c(other_long_term = tr("tr_permit_other"), family = tr("tr_permit_family"),
      student = tr("tr_permit_student"), short_term = tr("tr_permit_short"))[
        as.character(pb$permit_type)],
    levels = c(other_long_term = tr("tr_permit_other"), family = tr("tr_permit_family"),
      student = tr("tr_permit_student"), short_term = tr("tr_permit_short"))[
        as.character(pb$permit_type)])
  pb$hover <- htxt(sprintf(tr("tr_permits_hover"),
    as.character(pb$label), fmtv(pb$count), fa_num(pb$pct, 0)))

  p_permits <- plot_ly(pb, y = ~label, x = ~count, type = "bar",
      orientation = "h",
      marker = list(color = c("#b0b0b0", "#8a5a3a", "#d4a943", "#2774AE")),
      text = ~hover,
      hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
    layout(
      title = list(
        text = htxt(tr("tr_permits_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = "", tickfont = list(size = 12),
                   ticks = "outside", ticklen = 8,
                   tickcolor = "rgba(0,0,0,0)"),
      margin = list(t = 55, b = 40, l = 140),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)
  # fa/RTL: mirror the ranked horizontal bar (BBC Persian). fa-only.
  if (is_fa()) p_permits <- p_permits %>% layout(
    xaxis = list(autorange = "reversed"),
    yaxis = list(side = "right"),
    margin = list(t = 55, b = 40, l = 20, r = 140))

  # --- Assemble tr-immigration page ----------------------------------------
  imm_body <- paste0(
    '<div class="page-content">',
    # pt1: factoid above pc1 (citizens trend). Iranian-citizen growth 2014→peak.
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s×</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
  </div>',
      htxt(format(cit_growth)),
      sprintf(tr("tr_imm_cit_sentence"),
        format(cit_growth), fmtv(cit_2014),
        fa_num(cit_peak_yr, 0, big = FALSE), fmtv(cit_peak))),
    # pt2: factoid above pc2 (permits by type).
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
      fmtv(permit_total),
      tr("tr_imm_permits_sentence"),
      sprintf(tr("tr_imm_permits_bullet1"), fa_num(permit_short_pct, 0)),
      sprintf(tr("tr_imm_permits_bullet2"), fa_num(permit_student_pct, 0)),
      tr("tr_imm_permits_bullet3")),
    # pc1: citizens trend (untabbed now that Iran-born trend lives on the pop page)
    '<div class="chart-card pc1">',
    plotly_div("tr-citizens", pj(p_cit), "430px", source = CIT_SOURCE),
    '</div>',
    # pc2: permits by type (replaces the Iran-born vs citizens overlay, which
    # was the same data as the citizens chart; Iran-born moved to tr-population)
    '<div class="chart-card pc2">',
    plotly_div("tr-permits", pj(p_permits), "430px", source = PERMIT_SOURCE),
    '</div>',
    '</div>'
  )

  fname_imm <- if (is_fa()) "docs/pages/tr-immigration.fa.html" else "docs/pages/tr-immigration.html"
  writeLines(page_template_i18n(tr("tr_imm_title"), imm_body, lang = LANG),
             fname_imm)
  cat("  Done (tr-immigration)\n")
}

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nTürkiye: %s Iran-born (%d)\n",
            format(tr_total, big.mark = ","), data_yr))
cat(sprintf("Citizens: %s → %s (%d–%d)\n",
            format(cit_2014, big.mark = ","),
            format(cit$iranian_citizens[nrow(cit)], big.mark = ","),
            min(cit$year), max(cit$year)))

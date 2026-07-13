# Build Israel page from CBS (Central Bureau of Statistics) extracts.
# Run from deployment repo root:
#   Rscript R/build_israel.R
#
# Input:  data/israel/*.csv
# Output: docs/pages/il-population.html      + il-population.fa.html
#
# Bilingual (en + fa), following the build_nl.R pattern (Israel is in the
# standard page_template cluster, so it uses page_template_i18n() rather than a
# local shell like build_armenia.R). All user-facing strings come from
# R/i18n/strings_israel.R via tr(); numbers go through fa_num()/fmtv() so the
# English edition stays BYTE-IDENTICAL while the Persian edition renders RTL
# with Persian digits and the Vazirmatn face.
#
# Extract first via: Rscript R/il_export/extract_cbs_israel.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/israel"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, make_gen_box(),
# make_html_legend(), page_template().
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), page_template_i18n().
source("R/_helpers_i18n.R")
# Israel string table (defines the global STR consumed by tr()).
source("R/i18n/strings_israel.R")

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

# --- i18n formatting helpers --------------------------------------------------
# lnk():  in fa, isolate a Latin agency link/URL in <bdi> so bidi ordering is
#         correct; in en, pass through unchanged (keeps English byte-identical).
lnk <- function(x) if (is_fa()) bdi(x) else x

# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — so it reproduces format()'s common-width
#         PADDING that the committed hover text relies on; in fa it Persian-
#         digits that same padded string (ASCII "," thousands kept).
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

# --- Latin source links (language-independent) --------------------------------
CBS_LINK <- "<a href='https://www.cbs.gov.il/en' target='_blank' style='color:#2774AE;'>Israel Central Bureau of Statistics (CBS)</a>"
UN_LINK  <- "<a href='https://www.un.org/development/desa/pd/content/international-migrant-stock' target='_blank' style='color:#2774AE;'>UN DESA</a>"

# --- Load data (ONCE — language-independent) ----------------------------------
cat("Loading Israel CBS extracts...\n")
hl       <- read.csv(file.path(DATA_DIR, "il_headline.csv"), stringsAsFactors = FALSE)
age      <- read.csv(file.path(DATA_DIR, "il_age.csv"), stringsAsFactors = FALSE)
age_det  <- read.csv(file.path(DATA_DIR, "il_age_detail.csv"), stringsAsFactors = FALSE)
comp     <- read.csv(file.path(DATA_DIR, "il_comparison.csv"), stringsAsFactors = FALSE)

il_total <- hl$count[hl$category == "total"]
il_gen1  <- hl$count[hl$category == "gen1"]
il_gen2  <- hl$count[hl$category == "gen2"]

# --- Age distribution ordering (language-independent) -------------------------
# Order age groups from youngest at bottom to oldest at top.
age_order <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55+")
age$age_group <- factor(age$age_group, levels = age_order)
age <- age[order(age$age_group), ]

# --- Iran-born detail chart ordering (full 8 age bins) ------------------------
gen1_det <- age_det[age_det$generation == "Iran-born", ]
gen1_order <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
gen1_det$age_group <- factor(gen1_det$age_group, levels = gen1_order)
gen1_det <- gen1_det[order(gen1_det$age_group), ]
gen1_det$pct <- round(gen1_det$count / il_gen1 * 100, 1)

# Blue gradient from light (youngest) to dark (oldest).
gen1_colors <- c("#d4e6f1", "#c6dbef", "#8bbdde", "#5a9bd5", "#2774AE",
                 "#1a5c8a", "#1a4e72", "#08306b")

# --- Iran-born population over time (UN Migrant Stock 1990–2024) --------------
un_csv <- read.csv("data/global/stocks_countries.csv", stringsAsFactors = FALSE,
                   check.names = FALSE)
un_row <- un_csv[un_csv$destination == "Israel" &
                 grepl("^Iran", un_csv$origin), ]
un_years <- as.integer(sub("^X", "", names(un_row)[grepl("^X", names(un_row))]))
un_vals  <- as.integer(unlist(un_row[, grepl("^X", names(un_row))]))
un_trend <- data.frame(year = un_years, iran_born = un_vals)

# --- Comparison with other Asian-origin groups (colors — once) ----------------
# Sort by total descending (already sorted in the CSV). Categorical colors per
# country/group, keyed by the English name so the lookup is language-neutral;
# the display labels are translated per language inside the build loop.
comp_colors <- c("Iraq" = "#7b5ea7", "Iran" = "#1a4e72", "Yemen" = "#d4a943",
                 "Türkiye" = "#2ca089", "India and Pakistan" = "#e07b54",
                 "Syria and Lebanon" = "#8bbdde")
comp$color <- comp_colors[comp$country]

# String-table keys for each comparison group, keyed by the English name
# (same lookup shape as comp_colors, so it survives the "Türkiye" encoding).
comp_keys <- c("Iraq" = "country_iraq", "Iran" = "country_iran",
               "Yemen" = "country_yemen", "Türkiye" = "country_turkiye",
               "India and Pakistan" = "country_india_pakistan",
               "Syria and Lebanon" = "country_syria_lebanon")

# --- Age stat for the console summary (language-independent) ------------------
pct_55plus <- round(sum(age$gen1[age$age_group == "55+"]) / il_gen1 * 100)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Israel [%s] ===\n", LANG))

  # --- Source citation strings (per language) ---------------------------------
  cbs_src <- sprintf(tr("il_src_cbs"), lnk(CBS_LINK))
  un_src  <- sprintf(tr("il_src_un"), lnk(UN_LINK))

  # --- Age distribution (grouped horizontal bars) -----------------------------
  # Add Israeli-born first so Iran-born (dark blue) sits on top of each group.
  p_age <- plot_ly() %>%
    add_trace(y = age$age_group, x = age$gen2, type = "bar", orientation = "h",
      name = "Israeli-born (2nd gen)",
      marker = list(color = "#5a9bd5"),
      text = htxt(sprintf(tr("il_age_hover_gen2"), age$age_group, fmtv(age$gen2))),
      hoverinfo = "text", textposition = "none") %>%
    add_trace(y = age$age_group, x = age$gen1, type = "bar", orientation = "h",
      name = "Iran-born (1st gen)",
      marker = list(color = "#1a4e72"),
      text = htxt(sprintf(tr("il_age_hover_gen1"), age$age_group, fmtv(age$gen1))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("il_age_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = "", autorange = TRUE, categoryorder = "array",
        categoryarray = age_order,
        ticks = "outside", ticklen = 8,
        tickcolor = "rgba(0,0,0,0)"),
      barmode = "group",
      showlegend = FALSE,
      margin = list(t = 50, b = 30, l = 60),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  age_legend <- make_html_legend(setNames(c("#1a4e72", "#5a9bd5"),
    c(tr("il_leg_gen1"), tr("il_leg_gen2"))))

  # --- Iran-born detail chart (full 8 age bins) -------------------------------
  p_gen1_detail <- plot_ly() %>%
    add_trace(y = gen1_det$age_group, x = gen1_det$count, type = "bar", orientation = "h",
      marker = list(color = gen1_colors),
      text = htxt(sprintf(tr("il_gen1_hover"),
        gen1_det$age_group, fmtv(gen1_det$count), fa_num(gen1_det$pct, 1))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("il_gen1_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = "", categoryorder = "array", categoryarray = gen1_order,
                   ticks = "outside", ticklen = 8,
                   tickcolor = "rgba(0,0,0,0)"),
      showlegend = FALSE,
      margin = list(t = 50, b = 30, l = 60),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Iran-born population over time (UN Migrant Stock 1990–2024) ------------
  p_un_trend <- plot_ly(un_trend, x = ~year, y = ~iran_born,
      type = "scatter", mode = "lines+markers",
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 8),
      text = htxt(sprintf(tr("il_trend_hover"),
        fa_num(un_trend$year, 0, big = FALSE), fmtv(un_trend$iran_born))),
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(
        text = htxt(tr("il_trend_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 5),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 55, b = 40, l = 60, r = 15),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Comparison with other Asian-origin groups ------------------------------
  comp_disp <- unname(vapply(comp_keys[comp$country], tr, character(1)))
  comp_disp_factor <- factor(comp_disp, levels = rev(comp_disp))

  p_comp <- plot_ly() %>%
    add_trace(y = comp_disp_factor, x = comp$total, type = "bar", orientation = "h",
      marker = list(color = comp$color),
      text = htxt(sprintf(tr("il_comp_hover"),
        comp_disp, fmtv(comp$total), fmtv(comp$gen1), fmtv(comp$gen2))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("il_comp_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = "",
                   ticks = "outside", ticklen = 8,
                   tickcolor = "rgba(0,0,0,0)"),
      showlegend = FALSE,
      margin = list(t = 40, b = 30, l = 130),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Generation boxes -------------------------------------------------------
  gen_boxes <- paste0(
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">', tr("il_gen_box_title"), '</div>',
    '<div style="display:flex; gap:12px; margin-top:12px;">',
    make_gen_box(fmtv(il_gen1),
      sprintf(tr("il_gen_pct_of_total"), fa_num(round(il_gen1 / il_total * 100), 0)),
      tr("il_gen1_label"), tr("il_gen1_sub"), "#1a4e72"),
    make_gen_box(fmtv(il_gen2),
      sprintf(tr("il_gen_pct_of_total"), fa_num(round(il_gen2 / il_total * 100), 0)),
      tr("il_gen2_label"), tr("il_gen2_sub"), "#5a9bd5"),
    '</div>')

  # --- Assemble il-population page ---------------------------------------------
  pop_body <- paste0(
    # Top row: headline + generation boxes
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("il_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(il_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("il_pop_headline_caption"), lnk(CBS_LINK)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("il_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("il_pop_idbox_bullet1"), '</li>',
    '<li>', tr("il_pop_idbox_bullet2"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("il_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
    gen_boxes,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', cbs_src),
    '</div>',
    '</div>',

    # Bottom row: tabbed age charts + comparison chart
    '<div class="chart-row">',
    '<div class="chart-card">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'il-tab-both\',this,\'age-tabs\')">', tr("il_tab_both"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'il-tab-detail\',this,\'age-tabs\')">', tr("il_tab_detail"), '</button>',
    '</div>',
    '<div id="il-tab-both" class="tab-panel active" data-group="age-tabs">',
    plotly_div("il-age", pj(p_age), "430px", source = cbs_src,
      legend_html = age_legend),
    '</div>',
    '<div id="il-tab-detail" class="tab-panel" data-group="age-tabs">',
    plotly_div("il-gen1-detail", pj(p_gen1_detail), "430px", source = cbs_src),
    '</div>',
    '</div>',
    # Bottom-right: tabbed chart — population over time (default) +
    # Asian-origin comparison (second tab). Per framework rules we don't
    # add new rows; the comparative-context chart becomes a second tab.
    '<div class="chart-card">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'il-tab-trend\',this,\'il-right-tabs\')">', tr("il_tab_trend"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'il-tab-comp\',this,\'il-right-tabs\')">', tr("il_tab_comp"), '</button>',
    '</div>',
    '<div id="il-tab-trend" class="tab-panel active" data-group="il-right-tabs">',
    plotly_div("il-un-trend", pj(p_un_trend), "430px", source = un_src),
    '</div>',
    '<div id="il-tab-comp" class="tab-panel" data-group="il-right-tabs">',
    plotly_div("il-comp", pj(p_comp), "430px", source = cbs_src),
    '</div>',
    '</div>',
    '</div>'
  )

  fname_pop <- if (is_fa()) "docs/pages/il-population.fa.html" else "docs/pages/il-population.html"
  writeLines(page_template_i18n(tr("il_pop_title"), pop_body, has_tabs = TRUE, lang = LANG),
             fname_pop)
  cat("  Done\n")
}

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nIsrael: %s Iranian-origin (%s Iran-born + %s Israeli-born)\n",
  format(il_total, big.mark = ","),
  format(il_gen1, big.mark = ","),
  format(il_gen2, big.mark = ",")))
cat(sprintf("Iran-born aged 55+: %d%%\n", pct_55plus))
cat(sprintf("Iran is #%d largest Asian-origin group (after Iraq)\n",
  which(comp$country[order(-comp$total)] == "Iran")))

# Build Norway pages from SSB (Statistics Norway) extracts.
# Run from deployment repo root:
#   Rscript R/build_norway.R
#
# Input:  data/norway/*.csv
# Output: docs/pages/no-population.html      + no-population.fa.html
#
# Bilingual (en + fa), following the build_nl.R pattern. All user-facing
# strings come from R/i18n/strings_norway.R via tr(); numbers go through
# fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while the Persian
# edition renders RTL with Persian digits and the Vazirmatn face. Norway is a
# standard-cluster builder (shared page_template), so the fa edition is produced
# by page_template_i18n(..., lang = "fa"). See .claude/rules for context.
#
# Extract first via: Rscript R/no_export/extract_ssb.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/norway"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, make_gen_box(), make_html_legend().
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), page_template_i18n().
source("R/_helpers_i18n.R")
# Norway string table (defines the global STR consumed by tr()).
source("R/i18n/strings_norway.R")

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
SSB_LINK <- "<a href='https://www.ssb.no/en/' target='_blank' style='color:#2774AE;'>Statistics Norway (SSB)</a>"

# --- Load data (ONCE — language-independent) ---------------------------------
cat("Loading Norway SSB extracts...\n")
hl       <- read.csv(file.path(DATA_DIR, "no_headline.csv"), stringsAsFactors = FALSE)
trend    <- read.csv(file.path(DATA_DIR, "no_trend.csv"), stringsAsFactors = FALSE)
hist_sex <- read.csv(file.path(DATA_DIR, "no_hist_sex.csv"), stringsAsFactors = FALSE)
county   <- read.csv(file.path(DATA_DIR, "no_county.csv"), stringsAsFactors = FALSE)
employ   <- read.csv(file.path(DATA_DIR, "no_employment.csv"), stringsAsFactors = FALSE)

no_total <- hl$count[hl$category == "total"]
no_gen1  <- hl$count[hl$category == "gen1"]
no_gen2  <- hl$count[hl$category == "gen2"]
data_yr  <- hl$year[1]

# Clean county names: strip Sami variants after " - "
county$county_name <- sub(" - .*", "", county$county_name)

# --- Derived (language-independent) ------------------------------------------
no_hist_max <- max(hist_sex$year)

# Norway's register splits 1st gen (Iran-born) from 2nd gen (Norwegian-born to
# Iran-born parents) only from 2010 onward. Stacked area shows each generation's
# relative size; the total-line tab carries the long pre-2010 history.
gen_trend <- trend[trend$year <= data_yr, ]
no_gen_min <- min(gen_trend$year); no_gen_max <- max(gen_trend$year)

# County shares (map depicts the national total vintage).
county$join_name <- gsub(" - .*$", "", county$county_name)
county$pct <- round(county$count / no_total * 100, 1)

# County geojson (loaded once).
no_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "no_counties.geojson"),
                                 simplifyVector = FALSE)

latest_emp <- employ[nrow(employ), ]
oslo_pct <- round(county$count[county$county_name == "Oslo"] / no_total * 100)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Norway [%s] ===\n", LANG))

  # --- Source citation strings (per language) --------------------------------
  SSB_SOURCE     <- sprintf(tr("no_src_ssb"),     lnk(SSB_LINK))
  SSB_EMP_SOURCE <- sprintf(tr("no_src_ssb_emp"), lnk(SSB_LINK))

  pct_suffix <- tr("no_axis_pct_suffix")

  # =========================================================================
  # NO-POPULATION
  # =========================================================================
  cat("Building no-population...\n")

  # --- Historical total Iran-born 1970-2026 (single line) ------------------
  # Single total line: the value of this series is its length (1970-2026),
  # which shows when the Iran-born population took off. The sex split was
  # dropped (no other country breaks population by sex); the generation
  # breakdown lives in its own tab (gen data only goes back to 2010).
  p_hist <- plot_ly(hist_sex, x = ~year, y = ~total, type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 5),
      text = htxt(sprintf(tr("no_hist_hover"),
        fa_num(hist_sex$year, 0, big = FALSE), fmtv(hist_sex$total))),
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(sprintf(tr("no_hist_title"),
          fa_num(no_hist_max, 0, big = FALSE))),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", dtick = 10),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      showlegend = FALSE,
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Generation split (stacked area) -------------------------------------
  # Both traces share one hover so the mouseover is consistent.
  no_gen_hover <- htxt(sprintf(tr("no_gen_hover"),
    fa_num(gen_trend$year, 0, big = FALSE), fmtv(gen_trend$gen1),
    fmtv(gen_trend$gen2), fmtv(gen_trend$total)))
  p_gen <- plot_ly(gen_trend) %>%
    add_trace(x = ~year, y = ~gen1, type = "scatter", mode = "lines",
      stackgroup = "one", name = tr("no_leg_immigrants"),
      fillcolor = "rgba(26,78,114,0.75)", line = list(color = "#1a4e72", width = 1),
      text = no_gen_hover, hoverinfo = "text") %>%
    add_trace(x = ~year, y = ~gen2, type = "scatter", mode = "lines",
      stackgroup = "one", name = tr("no_leg_norwegian_born"),
      fillcolor = "rgba(90,155,213,0.75)", line = list(color = "#5a9bd5", width = 1),
      text = no_gen_hover, hoverinfo = "text") %>%
    layout(
      title = list(text = htxt(sprintf(tr("no_gen_title"),
          fa_num(no_gen_min, 0, big = FALSE), fa_num(no_gen_max, 0, big = FALSE))),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", dtick = 5),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      showlegend = FALSE,
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  no_gen_leg <- make_html_legend(setNames(c("#1a4e72", "#5a9bd5"),
                                 c(tr("no_leg_immigrants"), tr("no_leg_norwegian_born"))))

  # --- Employment rate trend (2001-2025) -----------------------------------
  p_employ <- plot_ly() %>%
    add_trace(data = employ, x = ~year, y = ~pct_total, type = "scatter",
      mode = "lines+markers", name = tr("no_name_total"),
      line = list(color = "#1a4e72", width = 3),
      marker = list(color = "#1a4e72", size = 7),
      text = htxt(sprintf(tr("no_emp_hover_total"),
        fa_num(employ$year, 0, big = FALSE), fa_num(employ$pct_total, 1),
        fmtv(employ$employed_total))),
      hoverinfo = "text") %>%
    add_trace(data = employ, x = ~year, y = ~pct_male, type = "scatter",
      mode = "lines", name = tr("no_name_male"),
      line = list(color = "#2171b5", width = 1.5, dash = "dash"),
      text = htxt(sprintf(tr("no_emp_hover_male"),
        fa_num(employ$year, 0, big = FALSE), fa_num(employ$pct_male, 1))),
      hoverinfo = "text") %>%
    add_trace(data = employ, x = ~year, y = ~pct_female, type = "scatter",
      mode = "lines", name = tr("no_name_female"),
      line = list(color = "#c4793a", width = 1.5, dash = "dash"),
      text = htxt(sprintf(tr("no_emp_hover_female"),
        fa_num(employ$year, 0, big = FALSE), fa_num(employ$pct_female, 1))),
      hoverinfo = "text") %>%
    layout(
      title = list(text = htxt(tr("no_emp_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", dtick = 2),
      yaxis = list(title = "", ticksuffix = pct_suffix, range = c(40, 75)),
      showlegend = FALSE,
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  no_emp_leg <- make_html_legend(setNames(c("#1a4e72", "#2171b5", "#c4793a"),
                                 c(tr("no_leg_total"), tr("no_leg_male_dashed"),
                                   tr("no_leg_female_dashed"))))

  # --- County choropleth map -----------------------------------------------
  p_county <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = no_geojson,
      locations = county$join_name, z = county$count,
      featureidkey = "properties.name",
      text = htxt(sprintf(tr("no_county_hover"),
        county$county_name, fmtv(county$count), fa_num(county$pct, 1))),
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                        c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron",
        center = list(lon = 15, lat = 64), zoom = 3.2),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Generation boxes ----------------------------------------------------
  gen_boxes <- paste0(
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">', tr("no_gen_box_title"), '</div>',
    '<div style="display:flex; gap:12px; margin-top:12px;">',
    make_gen_box(fmtv(no_gen1),
      sprintf(tr("no_gen_pct_of_total"), fa_num(round(no_gen1 / no_total * 100), 0)),
      tr("no_leg_immigrants"), tr("no_gen1_sub"), "#1a4e72"),
    make_gen_box(fmtv(no_gen2),
      sprintf(tr("no_gen_pct_of_total"), fa_num(round(no_gen2 / no_total * 100), 0)),
      tr("no_leg_norwegian_born"), tr("no_gen2_sub"), "#5a9bd5"),
    '</div>')

  # --- Assemble no-population page ------------------------------------------
  pop_body <- paste0(
    # Top row: headline + generation grid
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("no_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(no_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("no_pop_headline_caption"), lnk(SSB_LINK),
      fa_num(data_yr, 0, big = FALSE)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("no_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("no_pop_idbox_bullet1"), '</li>',
    '<li>', tr("no_pop_idbox_bullet2"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("no_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
    gen_boxes,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', SSB_SOURCE),
    '</div>',
    '</div>',

    # Bottom row: tabbed (history | generation | employment) + county map
    '<div class="chart-row">',
    '<div class="chart-card">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'no-tab-hist\',this,\'pop-tabs\')">', tr("no_tab_hist"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'no-tab-gen\',this,\'pop-tabs\')">', tr("no_tab_gen"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'no-tab-emp\',this,\'pop-tabs\')">', tr("no_tab_emp"), '</button>',
    '</div>',
    '<div id="no-tab-hist" class="tab-panel active" data-group="pop-tabs">',
    plotly_div("no-hist", pj(p_hist), "400px", source = SSB_SOURCE),
    '</div>',
    '<div id="no-tab-gen" class="tab-panel" data-group="pop-tabs">',
    plotly_div("no-gen", pj(p_gen), "400px", source = SSB_SOURCE,
      legend_html = no_gen_leg),
    '</div>',
    '<div id="no-tab-emp" class="tab-panel" data-group="pop-tabs">',
    plotly_div("no-employ", pj(p_employ), "400px", source = SSB_EMP_SOURCE,
      legend_html = no_emp_leg),
    '</div>',
    '</div>',
    '<div class="chart-card">',
    '<div class="section-title" style="margin-top:0;">', tr("no_geo_section_title"), '</div>',
    plotly_div("no-county", pj(p_county), "500px", source = SSB_SOURCE),
    '</div>',
    '</div>'
  )

  fname_pop <- if (is_fa()) "docs/pages/no-population.fa.html" else "docs/pages/no-population.html"
  writeLines(page_template_i18n(tr("no_pop_title"), pop_body, has_tabs = TRUE, lang = LANG),
             fname_pop)
  cat("  Done\n")
}

# --- Summary ------------------------------------------------------------------
cat(sprintf("\nNorway: %s Iranian-origin (%s immigrants + %s Norwegian-born)\n",
  format(no_total, big.mark = ","),
  format(no_gen1, big.mark = ","),
  format(no_gen2, big.mark = ",")))
cat(sprintf("Counties: %d, Oslo share: %d%%\n", nrow(county), oslo_pct))
cat(sprintf("Employment rate (20-66): %.1f%% (%d)\n",
  latest_emp$pct_total, latest_emp$year))

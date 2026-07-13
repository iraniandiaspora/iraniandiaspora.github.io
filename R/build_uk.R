# UK pages — ONS Census 2021 (E+W) + Scotland Census 2022 + NI 2021.
#
# Consumes:
#   data/uk/iran_uk_summary_2021.csv  -- 4 UK nations + totals
#   data/uk/uk_region_iran.csv        -- 11 GB regions (aggregated from LADs)
#   data/uk/uk_regions.geojson        -- 11-region GeoJSON
#   data/uk/uk_age_sex.csv            -- Age (6 bands) x Sex
#   data/uk/uk_year_of_arrival.csv    -- Year of arrival (11 periods)
#   data/uk/uk_economic_activity.csv  -- Economic activity (6 categories)
#   data/uk/uk_qualification.csv      -- Highest qualification (6 categories)
#   data/uk/uk_religion.csv           -- Religion (9 categories)
#
# Produces:
#   docs/pages/uk-population.html      + uk-population.fa.html
#   docs/pages/uk-workedu.html         + uk-workedu.fa.html
#
# Bilingual (en + fa), following the build_nl.R / build_denmark.R pattern. All
# user-facing strings come from R/i18n/strings_uk.R via tr(); numbers go through
# fa_num()/fmtv() so the English editions stay BYTE-IDENTICAL while the Persian
# editions render RTL with Persian digits and the Vazirmatn face. UK keeps its
# LOCAL page_template() for English; the fa edition is that same shell run
# through fa_shell() (UK's body font token already matches the canonical anchor,
# so no alignment is needed — unlike Armenia).
#
# Run from the deployment repo root:
#   Rscript R/build_uk.R
#
# (Regenerate clean data first via:
#    Rscript R/uk_export/clean_nomis_data.R
#    Rscript R/uk_export/clean_ons_custom.R)

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, make_html_legend(),
# hbar_over_labels(), pct_lab().
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# UK string table (defines the global STR consumed by tr() + the UK_*_FA maps).
source("R/i18n/strings_uk.R")

# Page-local tab-switching JS. UK used to concatenate this into
# iframe_resize_script; now injected as its own <script> block below.
tab_switch_script <- '
<script>
function switchTab(tabId,btn,groupId){
  var panels = document.querySelectorAll(".tab-panel[data-group=\'" + groupId + "\']");
  panels.forEach(function(p) { p.classList.remove("active"); });
  document.getElementById(tabId).classList.add("active");
  var btns = btn.parentElement.querySelectorAll(".tab-btn");
  btns.forEach(function(b) { b.classList.remove("active"); });
  btn.classList.add("active");
  var active = document.getElementById(tabId);
  if (active) {
    active.querySelectorAll(".js-plotly-plot").forEach(function(p) {
      if (window.Plotly) Plotly.Plots.resize(p);
    });
  }
  reportHeight();
}
</script>'

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
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:hidden; min-width:0; }
.text-card { background:white; border-radius:8px; padding:20px; border:1px solid #e0e0e0; font-size:14px; color:#444; line-height:1.7; }
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
.tab-bar { display:flex; justify-content:center; gap:0; margin:12px 0 0; }
.tab-btn { padding:6px 16px; border:1px solid #ddd; background:#f0f0f0; cursor:pointer;
  font-family:"Montserrat",sans-serif; font-size:13px; color:#333; border-radius:4px; margin:0 2px; transition:background 0.15s; white-space:nowrap; }
.tab-btn.active { background:#2774AE; color:white; font-weight:600; border-color:#2774AE; }
.tab-btn:hover:not(.active) { background:#e0e0e0; }
.tab-panel { display:none; }
.tab-panel.active { display:block; }
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row { grid-template-columns:1fr !important; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .chart-card { padding:10px; }
  .text-card { font-size:13px; padding:14px; }
  .tab-btn { font-size:11px; padding:4px 8px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
</head>
<body>
', body_html, '
', iframe_resize_script, '
', tab_switch_script, '
</body>
</html>')
}

# --- i18n formatting helpers (build_nl.R / build_denmark.R pattern) -----------
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
#         (hover text, chart titles) that lands inside Plotly's <script> JSON,
#         which FA_NUM_SCRIPT can't reach. Idempotent on already-Persian digits.
#         NEVER apply to HTML that carries CSS — only to plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source links (language-independent) --------------------------------
ONS_LINK  <- "<a href='https://www.nomisweb.co.uk/sources/census_2021' target='_blank' style='color:#2774AE;'>ONS Census 2021</a>"
SCOT_LINK <- "<a href='https://www.scotlandscensus.gov.uk/' target='_blank' style='color:#2774AE;'>Scotland&rsquo;s Census 2022</a>"
NISRA_LINK <- "<a href='https://www.nisra.gov.uk/statistics/census/2021-census' target='_blank' style='color:#2774AE;'>NISRA Census 2021</a>"

# ============================================================================
# DATA (loaded ONCE — language-independent)
# ============================================================================
cat("Building UK pages...\n")

summary_df <- read.csv("data/uk/iran_uk_summary_2021.csv", stringsAsFactors = FALSE)
region_df  <- read.csv("data/uk/uk_region_iran.csv", stringsAsFactors = FALSE)
age_sex    <- read.csv("data/uk/uk_age_sex.csv", stringsAsFactors = FALSE)
arrival    <- read.csv("data/uk/uk_year_of_arrival.csv", stringsAsFactors = FALSE)
arrival$label <- sub("^before ", "Before ", arrival$label)  # title-case the open-tail cohort chip to match "1951 to 1960" etc.
econ       <- read.csv("data/uk/uk_economic_activity.csv", stringsAsFactors = FALSE)
qual       <- read.csv("data/uk/uk_qualification.csv", stringsAsFactors = FALSE)
relig      <- read.csv("data/uk/uk_religion.csv", stringsAsFactors = FALSE)

uk_total <- summary_df$iran_born[summary_df$country == "UK total"]
nireland <- summary_df$iran_born[summary_df$country == "Northern Ireland"]

# --- Choropleth geojson (loaded once) ----------------------------------------
uk_geojson <- jsonlite::fromJSON("data/uk/uk_regions.geojson", simplifyVector = FALSE)

# --- Age by sex ordering (language-independent) ------------------------------
age_order <- c("15 years and under", "16 to 24 years", "25 to 34 years",
               "35 to 49 years", "50 to 64 years", "65 years and over")
age_sex$age_band <- factor(age_sex$age_band, levels = age_order)
males   <- age_sex %>% filter(sex == "Male") %>% arrange(age_band)
females <- age_sex %>% filter(sex == "Female") %>% arrange(age_band)

# --- Year of arrival binning (language-independent) --------------------------
# Numeric calendar x-axis with per-year-average heights and proportional bar
# widths (the Australia method), so the wide census decades do not visually
# swamp the narrow 2-3 year bins at the recent end. The cumulative-% line is
# computed from the true (un-averaged) counts. Census bins span unequal numbers
# of years: parse each into [low, high] arrival years to get span and center.
arrival <- arrival %>% arrange(mid_year)
arr_bounds <- function(period, mid) {
  if (grepl("before", period, ignore.case = TRUE)) {
    c(low = mid - 5, high = mid + 4)        # open tail: nominal 10-year span
  } else {
    yy <- as.integer(regmatches(period, gregexpr("[0-9]{4}", period))[[1]])
    c(low = yy[1], high = yy[length(yy)])
  }
}
bd <- t(mapply(arr_bounds, arrival$period, arrival$mid_year))
arrival$arr_low  <- bd[, "low"]
arrival$arr_high <- bd[, "high"]
arrival$span     <- arrival$arr_high - arrival$arr_low + 1
arrival$center   <- (arrival$arr_low + arrival$arr_high) / 2
arrival$annual_avg <- round(arrival$count / arrival$span)
arrival$bar_width  <- arrival$span * 0.9

# --- Religion sort/colors (language-independent) -----------------------------
uk_rel_colors <- c(
  "No religion"    = "#d4a943",
  "Muslim"         = "#1a4e72",
  "Christian"      = "#4a8c6f",
  "Other religion" = "#7b5ea7",
  "Jewish"         = "#e07b54",
  "Buddhist"       = "#c4793a",
  "Sikh"           = "#2ca089",
  "Hindu"          = "#8bbdde",
  "Not answered"   = "#b0b0b0"
)
relig_sorted <- relig %>% arrange(count)
relig_sorted$religion <- factor(relig_sorted$religion, levels = relig_sorted$religion)
relig_total <- sum(relig_sorted$count)
relig_bar_colors <- uk_rel_colors[as.character(relig_sorted$religion)]
relig_bar_colors[is.na(relig_bar_colors)] <- "#b0b0b0"

# --- Economic activity aggregation (language-independent) --------------------
# Combine three student categories into one; match Germany's 4-category scheme
econ_agg <- econ %>%
  mutate(group = case_when(
    category == "Employed" ~ "Employed",
    category == "Unemployed" ~ "Unemployed",
    category == "Inactive" ~ "Inactive",
    grepl("Student", category) ~ "Student"
  )) %>%
  group_by(group) %>%
  summarize(count = sum(count), .groups = "drop")

# Order: Student at bottom, then Inactive, Unemployed, Employed at top
econ_order <- c("Student", "Inactive", "Unemployed", "Employed")
econ_agg$group <- factor(econ_agg$group, levels = econ_order)
econ_agg <- econ_agg %>% arrange(group)
econ_total_16plus <- sum(econ_agg$count)

# Colors matching Germany pattern: employed=coral, unemployed=red, inactive=grey, student=light grey
econ_colors <- c("Student" = "#c8c8c8", "Inactive" = "#b0b0b0",
                 "Unemployed" = "#b05050", "Employed" = "#c4793a")

# --- Qualification ordering (language-independent) ---------------------------
qual_order <- c("Other qualifications", "No qualifications", "Level 1 (GCSEs)",
                "Level 2 (5+ GCSEs / O levels)", "Level 3 (A levels)",
                "Level 4+ (Degree or higher)")
qual$category <- factor(qual$category, levels = qual_order)
qual <- qual %>% arrange(category)
qual_total <- sum(qual$count)

# Blue gradient from light (low) to dark (high); grey for "Other"
qual_colors <- c("#b0b0b0", "#d4e6f1", "#c6dbef", "#8bbdde", "#5a9bd5", "#1a4e72")
qual_ord <- match(levels(qual$category), as.character(qual$category))
qual_xmax <- max(qual$count) * 1.15

# --- Key stats for text cards (language-independent) -------------------------
employed <- econ_agg$count[econ_agg$group == "Employed"]
unemployed <- econ_agg$count[econ_agg$group == "Unemployed"]
inactive <- econ_agg$count[econ_agg$group == "Inactive"]
students <- econ_agg$count[econ_agg$group == "Student"]
active_total <- employed + unemployed
employment_rate <- round(employed / active_total * 100, 1)

degree_plus <- qual$count[qual$category == "Level 4+ (Degree or higher)"]
degree_pct <- round(degree_plus / qual_total * 100, 1)

# Additional shares for the factoid notes
inactive_pct <- round(inactive / (active_total + inactive) * 100)
student_pct  <- round(students / (active_total + inactive) * 100)
econ_total   <- active_total + inactive
unemp_pct    <- round(unemployed / active_total * 100)

no_qual <- qual$count[qual$category == "No qualifications"]
no_qual_pct <- round(no_qual / qual_total * 100)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building UK [%s] ===\n", LANG))

  # --- Source citation strings (per language) ---------------------------------
  ONS_CUSTOM <- sprintf(tr("uk_src_ew"), lnk(ONS_LINK))
  MAP_SRC    <- sprintf(tr("uk_src_map"), lnk(ONS_LINK), lnk(SCOT_LINK))
  pct_suffix <- tr("uk_axis_pct_suffix")

  # ===========================================================================
  # UK-POPULATION
  # ===========================================================================
  cat("Building uk-population...\n")

  # --- Choropleth map (region names stay Latin; only surrounding text fa) -----
  p_uk_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = uk_geojson,
      locations = region_df$region_code,
      z = region_df$iran_born,
      featureidkey = "properties.AREACD",
      text = htxt(sprintf(tr("uk_map_hover"),
        region_df$region_name,
        fmtv(region_df$iran_born),
        fa_num(region_df$iran_born / uk_total * 100, 1))),
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"), c(0.08, "#6baed6"),
        c(0.35, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.92)
    ) %>% layout(
      mapbox = list(style = "carto-positron",
        center = list(lon = -4.0, lat = 54.5), zoom = 4.2),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Age by sex grouped bar -------------------------------------------------
  males_x   <- if (is_fa()) fa_digits(unname(UK_AGE_FA[as.character(males$age_band)])) else males$age_band
  females_x <- if (is_fa()) fa_digits(unname(UK_AGE_FA[as.character(females$age_band)])) else females$age_band

  age_xaxis <- list(title = "", tickangle = -30, tickfont = list(size = 10))
  if (is_fa()) {
    age_xaxis$categoryorder <- "array"
    age_xaxis$categoryarray <- fa_digits(unname(UK_AGE_FA[age_order]))
  }

  p_age <- plot_ly() %>%
    add_bars(x = males_x, y = males$count, name = tr("uk_leg_male"),
      marker = list(color = "#2171b5"),
      text = htxt(sprintf(tr("uk_age_hover_male"), males_x,
        fmtv(males$count))),
      hoverinfo = "text", textposition = "none") %>%
    add_bars(x = females_x, y = females$count, name = tr("uk_leg_female"),
      marker = list(color = "#c4793a"),
      text = htxt(sprintf(tr("uk_age_hover_female"), females_x,
        fmtv(females$count))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("uk_age_title")),
        font = list(size = 14, family = "Montserrat")),
      barmode = "group",
      xaxis = age_xaxis,
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 55, b = 80), showlegend = FALSE,
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  age_leg <- make_html_legend(setNames(c("#2171b5", "#c4793a"),
    c(tr("uk_leg_male"), tr("uk_leg_female"))))

  # --- Year of arrival bars + cumulative line ---------------------------------
  disp_label <- arrival$label
  if (is_fa()) {
    disp_label <- sub("^Before ", "پیش از ", disp_label)
    disp_label <- gsub(" to ", " تا ", disp_label)
    disp_label <- fa_digits(disp_label)
  }
  arr_bar_hover <- htxt(sprintf(tr("uk_arrival_hover_bar"),
    disp_label, fmtv(arrival$count), fmtv(arrival$annual_avg),
    fa_num(arrival$cum_pct, 1)))
  arr_cum_hover <- htxt(sprintf(tr("uk_arrival_hover_cum"),
    disp_label, fa_num(arrival$cum_pct, 1)))

  p_arrival <- plot_ly() %>%
    add_bars(data = arrival, x = ~center, y = ~annual_avg, width = ~bar_width,
      marker = list(color = "#2774AE"),
      text = arr_bar_hover, hoverinfo = "text", textposition = "none",
      showlegend = FALSE) %>%
    add_trace(data = arrival, x = ~center, y = ~cum_pct, type = "scatter",
      mode = "lines", yaxis = "y2",
      line = list(color = "lightblue", width = 2),
      text = arr_cum_hover,
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(tr("uk_arrival_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", dtick = 10, range = c(1939, 2024),
        tickfont = list(size = 11)),
      yaxis = list(title = "", tickformat = ","),
      yaxis2 = list(title = "", overlaying = "y", side = "right",
        ticksuffix = pct_suffix, range = c(0, 105), showgrid = FALSE,
        tickfont = list(size = 10, color = "#6b6b6b")),
      margin = list(t = 55, b = 55, r = 40),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(text = htxt(tr("uk_arrival_footnote")),
          x = 0.5, y = -0.15, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 9, color = "#6b6b6b"), xanchor = "center"))
    ) %>% config(displayModeBar = FALSE)

  # --- Religion horizontal bar ------------------------------------------------
  relig_disp <- if (is_fa()) fa_digits(unname(UK_REL_FA[as.character(relig_sorted$religion)])) else as.character(relig_sorted$religion)
  rs <- relig_sorted
  rs$lab <- factor(relig_disp, levels = relig_disp)
  p_relig <- plot_ly(rs, y = ~lab, x = ~count, type = "bar",
    orientation = "h", marker = list(color = relig_bar_colors),
    text = htxt(sprintf(tr("uk_bar_hover"),
      relig_disp,
      fmtv(relig_sorted$count),
      fa_num(relig_sorted$count / relig_total * 100, 1))),
    hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("uk_relig_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = "", tickfont = list(size = 11),
                   ticks = "outside", ticklen = 8,
                   tickcolor = "rgba(0,0,0,0)"),
      margin = list(l = 120, r = 20, t = 55, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Assemble uk-population -------------------------------------------------
  pop_body <- paste0(
    # Top row: headline (left) + tabbed age/arrival (right)
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("uk_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(uk_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("uk_pop_headline_caption"), lnk(ONS_LINK), lnk(SCOT_LINK), lnk(NISRA_LINK)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("uk_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("uk_pop_idbox_bullet1"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("uk_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'uk-tab-age\',this,\'pop-tabs\')">', tr("uk_tab_age"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'uk-tab-arrival\',this,\'pop-tabs\')">', tr("uk_tab_arrival"), '</button>',
    '</div>',
    '<div id="uk-tab-age" class="tab-panel active" data-group="pop-tabs">',
    plotly_div("uk-age", pj(p_age), "430px", source = ONS_CUSTOM, legend_html = age_leg),
    '</div>',
    '<div id="uk-tab-arrival" class="tab-panel" data-group="pop-tabs">',
    plotly_div("uk-arrival", pj(p_arrival), "430px", source = ONS_CUSTOM),
    '</div>',
    '</div>',
    '</div>',

    # Bottom row: religion (left) + map (right)
    '<div class="chart-row">',
    '<div class="chart-card">',
    plotly_div("uk-relig", pj(p_relig), "430px", source = ONS_CUSTOM),
    '</div>',
    '<div class="chart-card">',
    '<div class="section-title" style="margin-top:0;">', tr("uk_geo_section_title"), '</div>',
    plotly_div("uk-region-map", pj(p_uk_map), "430px", source = MAP_SRC),
    '</div>',
    '</div>'
  )

  html <- page_template(tr("uk_pop_title"), pop_body)
  if (is_fa()) html <- fa_shell(html)
  writeLines(html, if (is_fa()) "docs/pages/uk-population.fa.html" else "docs/pages/uk-population.html")
  cat("  Done\n")

  # ===========================================================================
  # UK-WORKEDU
  # ===========================================================================
  cat("Building uk-workedu...\n")

  # --- Economic activity horizontal bar ---------------------------------------
  econ_disp <- if (is_fa()) fa_digits(unname(UK_ECON_FA[as.character(econ_agg$group)])) else as.character(econ_agg$group)
  ea <- econ_agg
  ea$lab <- factor(econ_disp, levels = econ_disp)
  p_econ <- plot_ly(ea, y = ~lab, x = ~count, type = "bar",
    orientation = "h", marker = list(color = econ_colors[as.character(econ_agg$group)]),
    text = htxt(sprintf(tr("uk_bar_hover"),
      econ_disp,
      fmtv(econ_agg$count),
      fa_num(econ_agg$count / econ_total_16plus * 100, 1))),
    hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("uk_econ_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", tickformat = ","),
      yaxis = list(title = "", tickfont = list(size = 11)),
      margin = list(l = 100, r = 20, t = 55, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Qualification horizontal bar (label-above; RTL-mirrored on fa) ----------
  qual_labels_disp <- if (is_fa()) fa_digits(unname(UK_QUAL_FA[levels(qual$category)])) else levels(qual$category)
  qual_end_text <- pct_lab(qual$count[qual_ord] / qual_total * 100)
  if (is_fa()) qual_end_text <- gsub("%", "٪", fa_digits(qual_end_text))  # N% -> ۲۴٪ (axis-style suffix)
  ov_qual <- hbar_over_labels(qual_labels_disp,
    ends = qual$count[qual_ord],
    end_text = qual_end_text)
  qual_xrange <- if (isTRUE(ov_qual$xreversed)) c(qual_xmax, 0) else c(0, qual_xmax)
  qual_row_disp <- if (is_fa()) fa_digits(unname(UK_QUAL_FA[as.character(qual$category)])) else as.character(qual$category)
  p_qual <- plot_ly(qual, y = ~category, x = ~count, type = "bar",
    orientation = "h", marker = list(color = qual_colors),
    text = htxt(sprintf(tr("uk_bar_hover"),
      qual_row_disp,
      fmtv(qual$count),
      fa_num(qual$count / qual_total * 100, 1))),
    hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("uk_qual_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE, range = qual_xrange),
      yaxis = ov_qual$yaxis,
      annotations = ov_qual$annotations, bargap = ov_qual$bargap,
      margin = list(l = ov_qual$margin_l, r = 20, t = ov_qual$margin_t, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Text-card pieces (htxt guards stray literal digits in fa prose) --------
  w1_big  <- sprintf(tr("uk_card_bignum"), fa_num(employment_rate, 0))
  w1_prim <- htxt(tr("uk_card1_primary"))
  w1_sec  <- htxt(sprintf(tr("uk_card1_secondary"),
    fa_num(inactive_pct, 0), fa_num(student_pct, 0)))
  w2_big  <- sprintf(tr("uk_card_bignum"), fa_num(degree_pct, 0))
  w2_prim <- htxt(tr("uk_card2_primary"))
  w2_sec  <- htxt(sprintf(tr("uk_card2_secondary"), fa_num(no_qual_pct, 0)))

  workedu_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', w1_big, w1_prim, w1_sec),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', w2_big, w2_prim, w2_sec),
    '<div class="chart-card pc1">',
    plotly_div("uk-econ", pj(p_econ), "380px", source = ONS_CUSTOM),
    '</div>',
    '<div class="chart-card pc2">',
    plotly_div("uk-qual", pj(p_qual), ov_qual$height, source = ONS_CUSTOM),
    '</div>',
    '</div>'
  )

  html_wi <- page_template(tr("uk_workedu_title"), workedu_body)
  if (is_fa()) html_wi <- fa_shell(html_wi)
  writeLines(html_wi, if (is_fa()) "docs/pages/uk-workedu.fa.html" else "docs/pages/uk-workedu.html")
  cat("  Done\n")
}

cat(sprintf("\nUK: %s Iran-born, %s%% employed, %s%% degree+\n",
  format(uk_total, big.mark = ","), employment_rate, degree_pct))

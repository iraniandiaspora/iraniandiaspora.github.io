# Build Netherlands pages from CBS StatLine Iran extracts.
# Run from deployment repo root:
#   Rscript R/build_nl.R
#
# Input:  data/netherlands/*.csv, data/netherlands/nl_provinces.geojson
# Output: docs/pages/nl-population.html      + nl-population.fa.html
#         docs/pages/nl-workinc.html         + nl-workinc.fa.html
#
# Bilingual (en + fa) pattern-setting builder. All user-facing strings come
# from R/i18n/strings_nl.R via tr(); numbers go through fa_num()/fmtv() so the
# English editions stay BYTE-IDENTICAL while the Persian editions render RTL
# with Persian digits and the Vazirmatn face. See .claude/rules for context.
#
# Extract first via: Rscript R/nl_export/extract_cbs.R

suppressPackageStartupMessages({
  library(plotly)
  library(jsonlite)
  library(dplyr)
})

DATA_DIR <- "data/netherlands"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, make_gen_box(), make_html_legend().
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), page_template_i18n().
source("R/_helpers_i18n.R")
# Netherlands string table (defines the global STR consumed by tr()).
source("R/i18n/strings_nl.R")

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
#         PADDING (leading spaces) that the committed hover text relies on;
#         fa_num()/formatC() do NOT pad and would break byte-identity. In fa it
#         Persian-digits that same padded string (separators -> U+066C).
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles, axis ticktext). Idempotent on already-
#         Persian digits, so safe to wrap fa_num() output. NEVER apply to HTML
#         that carries CSS (e.g. "36px") — only to plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source links (language-independent) --------------------------------
CBS_LINK <- "<a href='https://opendata.cbs.nl/statline/' target='_blank' style='color:#2774AE;'>CBS StatLine</a>"
EURO_LINK <- "<a href='https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/' target='_blank' style='color:#2774AE;'>Eurostat</a>"

# --- Load data (ONCE — language-independent) ---------------------------------
cat("Loading Netherlands CBS extracts...\n")
hl    <- read.csv(file.path(DATA_DIR, "nl_headline.csv"), stringsAsFactors = FALSE)
trend <- read.csv(file.path(DATA_DIR, "nl_trend.csv"), stringsAsFactors = FALSE)
prov  <- read.csv(file.path(DATA_DIR, "nl_province.csv"), stringsAsFactors = FALSE)
lf    <- read.csv(file.path(DATA_DIR, "nl_labourforce.csv"), stringsAsFactors = FALSE)

nl_total <- hl$count[hl$category == "total"]
nl_gen1  <- hl$count[hl$category == "gen1"]
nl_gen2  <- hl$count[hl$category == "gen2"]

# Province/map shares divide by the 2025 provincial total (the vintage the map
# depicts), not the 2026 headline total, so hover percentages stay internally
# consistent (the 12 provinces sum to the 2025 national figure).
prov_total <- sum(prov$count)

# --- Historical Iran-born trend 1990-2025 (UN + Eurostat) --------------------
# Eurostat migr_pop3ctb gives Iran-born stock annually 1999-2025.
# Extend back to 1990 with UN Migrant Stock.
eurostat_all <- read.csv("data/europe/iran_born_combined.csv",
                         stringsAsFactors = FALSE)
euro_nl <- eurostat_all %>%
  filter(geo == "NL") %>%
  mutate(year = as.integer(year), iran_born = as.integer(value)) %>%
  select(year, iran_born) %>%
  arrange(year)

# Add UN 1990, 1995 data points
un_global <- read.csv("data/global/stocks_countries.csv", stringsAsFactors = FALSE)
un_nl <- un_global[grepl("Netherlands", un_global$destination), ]
if (nrow(un_nl) > 0) {
  un_pts <- data.frame(year = c(1990L, 1995L),
    iran_born = c(un_nl$X1990, un_nl$X1995))
  un_pts <- un_pts[!un_pts$year %in% euro_nl$year, ]
  euro_nl <- bind_rows(un_pts, euro_nl) %>% arrange(year)
}

# --- Arrival year aggregation (from residence duration) ----------------------
arrival <- read.csv(file.path(DATA_DIR, "nl_arrival_year.csv"),
                    stringsAsFactors = FALSE)
arrival$label <- ifelse(arrival$arrival_year <= 1965,
                        "≤1965", as.character(arrival$arrival_year))
arrival_agg <- aggregate(count ~ label, data = arrival, FUN = sum)
arrival_agg$year_num <- ifelse(arrival_agg$label == "≤1965", 1965L,
                               as.integer(arrival_agg$label))
arrival_agg <- arrival_agg[order(arrival_agg$year_num), ]
total_arr <- sum(arrival_agg$count)
arrival_agg$cumulative <- cumsum(arrival_agg$count)
arrival_agg$cum_pct <- round(arrival_agg$cumulative / total_arr * 100, 1)
arrival_agg$x_num <- seq_len(nrow(arrival_agg)) - 1L
max_bar <- max(arrival_agg$count)

# --- Province geojson (loaded once) ------------------------------------------
geojson_path <- file.path(DATA_DIR, "nl_provinces.geojson")
has_geojson <- file.exists(geojson_path)
if (has_geojson) nl_geojson <- fromJSON(geojson_path, simplifyVector = FALSE)

# --- Work/income derived scalars (language-independent) ----------------------
latest <- lf[lf$year == max(lf$year), ]
income <- read.csv(file.path(DATA_DIR, "nl_income.csv"), stringsAsFactors = FALSE)
inc_valid <- income[!is.na(income$iran_low_income_pct) &
                    !is.na(income$nl_low_income_pct), ]
latest_inc <- income[income$year == max(income$year[!is.na(income$iran_avg_income_k)]), ]
inc_ratio  <- round(latest_inc$iran_avg_income_k / latest_inc$nl_avg_income_k * 100)
# Low-income trend (share of Iranian-origin households under the
# national low-income threshold). Has fallen substantially since the
# early 2010s peak.
li_latest_yr <- max(income$year[!is.na(income$iran_low_income_pct)])
li_latest    <- round(income$iran_low_income_pct[income$year == li_latest_yr])
li_peak_row  <- income[which.max(income$iran_low_income_pct), ]
li_peak_yr   <- li_peak_row$year
li_peak      <- round(li_peak_row$iran_low_income_pct)

# Randstad share (used only in the console summary below).
randstad     <- sum(prov$count[prov$province_code %in% c("PV27", "PV28", "PV26")])
randstad_pct <- round(randstad / prov_total * 100)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# LANG is the loop variable; at top-level Rscript scope it lives in the global
# env, which is where the i18n helpers read it.
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Netherlands [%s] ===\n", LANG))

  # --- Source citation strings (per language) --------------------------------
  cbs_src     <- sprintf(tr("nl_src_cbs_pop2025"),  lnk(CBS_LINK))
  cbs_pop_src <- sprintf(tr("nl_src_cbs_pop2026"),  lnk(CBS_LINK))
  cbs_lf_src  <- sprintf(tr("nl_src_cbs_lf"),       lnk(CBS_LINK))
  hist_src    <- sprintf(tr("nl_src_hist"),         lnk(EURO_LINK))
  cbs_dur_src <- sprintf(tr("nl_src_cbs_duration"), lnk(CBS_LINK))
  cbs_inc_src <- sprintf(tr("nl_src_cbs_income"),   lnk(CBS_LINK))

  # Axis affixes.
  pct_suffix  <- tr("nl_axis_pct_suffix")
  euro_prefix <- if (is_fa()) "" else "€"   # tr() fails loud on empty fa
  k_suffix    <- if (is_fa()) "" else "k"
  y2_ticktext <- htxt(c(tr("nl_axis_pct_0"), tr("nl_axis_pct_25"),
                        tr("nl_axis_pct_50"), tr("nl_axis_pct_75"),
                        tr("nl_axis_pct_100")))

  # Arrival cohort display labels (pre-1965 relabel + Persian digits in fa).
  disp_label <- ifelse(arrival_agg$label == "≤1965",
                       tr("nl_arrival_pre1965_label"), arrival_agg$label)
  if (is_fa()) disp_label <- fa_digits(disp_label)

  # ===========================================================================
  # NL-POPULATION
  # ===========================================================================
  cat("Building nl-population...\n")

  # --- Province choropleth map -----------------------------------------------
  if (has_geojson) {
    p_nl_map <- plot_ly() %>%
      add_trace(type = "choroplethmapbox",
        geojson = nl_geojson,
        locations = prov$province_code,
        z = prov$count,
        featureidkey = "properties.statcode",
        text = htxt(sprintf(tr("nl_prov_hover"),
          prov$province_name,
          fmtv(prov$count),
          fa_num(prov$count / prov_total * 100, 1))),
        hoverinfo = "text",
        colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"),
                          c(0.08, "#6baed6"), c(0.35, "#2171b5"), c(1, "#08306b")),
        showscale = TRUE,
        colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
        marker = list(line = list(color = "white", width = 1), opacity = 0.92)
      ) %>% layout(
        mapbox = list(
          style = "carto-positron",
          center = list(lon = 5.3, lat = 52.15),
          zoom = 5.8
        ),
        margin = list(t = 10, b = 10, l = 0, r = 0),
        paper_bgcolor = "white"
      ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

    map_html <- plotly_div("nl-prov-map", pj(p_nl_map), "500px",
      source = cbs_src)
  } else {
    # Fallback: horizontal bar chart if GeoJSON is missing
    prov$province_name <- factor(prov$province_name, levels = rev(prov$province_name))
    p_nl_bar <- plot_ly(prov, y = ~province_name, x = ~count, type = "bar",
      orientation = "h", marker = list(color = "#2774AE"),
      text = htxt(sprintf(tr("nl_prov_hover"), prov$province_name,
        fmtv(prov$count), fa_num(prov$count / prov_total * 100, 1))),
      hoverinfo = "text", textposition = "none") %>%
      layout(
        xaxis = list(title = "", tickformat = ","),
        yaxis = list(title = "",
                     ticks = "outside", ticklen = 8,
                     tickcolor = "rgba(0,0,0,0)"),
        margin = list(l = 130, r = 20, t = 10, b = 20),
        plot_bgcolor = "white", paper_bgcolor = "white"
      ) %>% config(displayModeBar = FALSE)
    map_html <- plotly_div("nl-prov-bar", pj(p_nl_bar), "500px",
      source = cbs_src)
  }

  # --- Historical Iran-born trend 1990-2025 ----------------------------------
  p_hist <- plot_ly(euro_nl, x = ~year, y = ~iran_born, type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1a4e72", width = 2.5),
      marker = list(color = "#1a4e72", size = 5),
      text = htxt(sprintf(tr("nl_hist_hover"),
        fa_num(euro_nl$year, 0, big = FALSE), fmtv(euro_nl$iran_born))),
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(tr("nl_hist_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 5),
      yaxis = list(title = "", tickformat = ",", rangemode = "tozero"),
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Arrival year chart (from residence duration) --------------------------
  p_arrival <- plot_ly() %>%
    add_bars(data = arrival_agg, x = ~x_num, y = ~count,
      marker = list(color = "#2774AE",
        line = list(color = "#1a4e72", width = 0.3)),
      text = htxt(sprintf(tr("nl_arrival_hover_bar"),
        disp_label, fmtv(arrival_agg$count),
        fa_num(arrival_agg$cum_pct, 1))),
      hoverinfo = "text", textposition = "none", showlegend = FALSE, name = "Arrivals") %>%
    add_trace(data = arrival_agg, x = ~x_num,
      y = ~cum_pct / 100 * max_bar,
      type = "scatter", mode = "lines",
      yaxis = "y2",
      line = list(color = "lightblue", width = 2),
      text = htxt(sprintf(tr("nl_arrival_hover_cum"),
        disp_label, fa_num(arrival_agg$cum_pct, 1))),
      hoverinfo = "text", showlegend = FALSE, name = "Cumulative",
      inherit = FALSE) %>%
    layout(
      title = list(
        text = htxt(tr("nl_arrival_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", tickangle = -45, tickfont = list(size = 9),
        tickvals = arrival_agg$x_num[seq(1, nrow(arrival_agg), by = 5)],
        ticktext = disp_label[seq(1, nrow(arrival_agg), by = 5)]),
      yaxis = list(title = "", tickformat = ",", tickfont = list(size = 10)),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
        range = c(0, max_bar * 1.05),
        tickvals = seq(0, max_bar, length.out = 5),
        ticktext = y2_ticktext,
        tickfont = list(size = 10)),
      margin = list(t = 55, b = 70, r = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Generation boxes ------------------------------------------------------
  gen_boxes <- paste0(
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">', tr("nl_gen_box_title"), '</div>',
    '<div style="display:flex; gap:12px; margin-top:12px;">',
    make_gen_box(fmtv(nl_gen1),
      sprintf(tr("nl_gen_pct_of_total"), fa_num(round(nl_gen1 / nl_total * 100), 0)),
      tr("nl_gen1_label"), tr("nl_gen1_sub"), "#1a4e72"),
    make_gen_box(fmtv(nl_gen2),
      sprintf(tr("nl_gen_pct_of_total"), fa_num(round(nl_gen2 / nl_total * 100), 0)),
      tr("nl_gen2_label"), tr("nl_gen2_sub"), "#5a9bd5"),
    '</div>')

  # --- Assemble nl-population page (Germany layout) --------------------------
  # Top row: headline (left) + generation grid (right)
  # Bottom row: tabbed trend/arrival (left) + province map (right)
  pop_body <- paste0(
    # Top row: headline + generation grid
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("nl_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(nl_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("nl_pop_headline_caption"), lnk(CBS_LINK)), '</div>',
    '<div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("nl_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:2;">',
    '<li>', tr("nl_pop_idbox_bullet1"), '</li>',
    '<li>', tr("nl_pop_idbox_bullet2"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("nl_pop_thirdgen_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; flex-direction:column; justify-content:center;">',
    gen_boxes,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0 0; padding-right:2px;">%s</p>', cbs_pop_src),
    '</div>',
    '</div>',

    # Bottom row: tabbed (historical trend | arrival year) + province map
    '<div class="chart-row">',
    '<div class="chart-card">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'nl-tab-hist\',this,\'pop-tabs\')">', tr("nl_tab_hist"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'nl-tab-arrival\',this,\'pop-tabs\')">', tr("nl_tab_arrival"), '</button>',
    '</div>',
    '<div id="nl-tab-hist" class="tab-panel active" data-group="pop-tabs">',
    plotly_div("nl-hist", pj(p_hist), "430px", source = hist_src),
    '</div>',
    '<div id="nl-tab-arrival" class="tab-panel" data-group="pop-tabs">',
    plotly_div("nl-arrival", pj(p_arrival), "430px", source = cbs_dur_src),
    '</div>',
    '</div>',
    '<div class="chart-card">',
    '<div class="section-title" style="margin-top:0;">', tr("nl_geo_section_title"), '</div>',
    map_html,
    '</div>',
    '</div>'
  )

  fname_pop <- if (is_fa()) "docs/pages/nl-population.fa.html" else "docs/pages/nl-population.html"
  writeLines(page_template_i18n(tr("nl_pop_title"), pop_body, has_tabs = TRUE, lang = LANG),
             fname_pop)
  cat("  Done\n")


  # ===========================================================================
  # NL-WORKINC (page-content: 2 text + 2 tabbed chart cells)
  # ===========================================================================
  cat("Building nl-workinc...\n")

  # --- Labour participation trend --------------------------------------------
  p_participation <- plot_ly(lf, x = ~year, y = ~participation_rate,
    type = "scatter", mode = "lines+markers",
    line = list(color = "#1a4e72", width = 3),
    marker = list(color = "#1a4e72", size = 8),
    text = htxt(sprintf(tr("nl_part_hover"),
      fa_num(lf$year, 0, big = FALSE), fa_num(lf$participation_rate, 0),
      fmtv(lf$employed), fmtv(lf$total_15_74))),
    hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("nl_part_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", tickmode = "array", tickvals = lf$year, dtick = 1),
      yaxis = list(title = "", range = c(50, 70), ticksuffix = pct_suffix),
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Employment type stacked bar -------------------------------------------
  p_emp <- plot_ly() %>%
    add_trace(x = lf$year, y = lf$permanent_pct, type = "bar",
      name = "Permanent", marker = list(color = "#1a4e72"),
      text = htxt(sprintf(tr("nl_emp_hover_perm"), fa_num(lf$year, 0, big = FALSE), fa_num(lf$permanent_pct, 0))),
      hoverinfo = "text", textposition = "none") %>%
    add_trace(x = lf$year, y = lf$flexible_pct, type = "bar",
      name = "Flexible", marker = list(color = "#5a9bd5"),
      text = htxt(sprintf(tr("nl_emp_hover_flex"), fa_num(lf$year, 0, big = FALSE), fa_num(lf$flexible_pct, 0))),
      hoverinfo = "text", textposition = "none") %>%
    add_trace(x = lf$year, y = lf$selfemployed_pct, type = "bar",
      name = "Self-employed", marker = list(color = "#e6a756"),
      text = htxt(sprintf(tr("nl_emp_hover_self"), fa_num(lf$year, 0, big = FALSE), fa_num(lf$selfemployed_pct, 0))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      barmode = "stack",
      title = list(text = htxt(tr("nl_emp_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", tickmode = "array", tickvals = lf$year, dtick = 1),
      yaxis = list(title = "", range = c(0, 105), ticksuffix = pct_suffix),
      showlegend = FALSE,
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  emp_leg <- make_html_legend(setNames(c("#1a4e72", "#5a9bd5", "#e6a756"),
                              c(tr("nl_leg_permanent"), tr("nl_leg_flexible"),
                                tr("nl_leg_selfemp"))))

  # --- Income comparison -----------------------------------------------------
  p_income <- plot_ly() %>%
    add_trace(x = income$year, y = income$nl_avg_income_k, type = "scatter",
      mode = "lines+markers", name = "NL average",
      line = list(color = "#999999", width = 2, dash = "dash"),
      marker = list(color = "#999999", size = 5),
      text = htxt(sprintf(tr("nl_inc_hover_nl"),
        fa_num(income$year, 0, big = FALSE), fmtv(income$nl_avg_income_k * 1000))),
      hoverinfo = "text", textposition = "none") %>%
    add_trace(x = income$year, y = income$iran_avg_income_k, type = "scatter",
      mode = "lines+markers", name = "Iranian",
      line = list(color = "#c4793a", width = 3),
      marker = list(color = "#c4793a", size = 7),
      text = htxt(sprintf(tr("nl_inc_hover_iran"),
        fa_num(income$year, 0, big = FALSE),
        fmtv(income$iran_avg_income_k * 1000),
        fa_num(round(income$iran_avg_income_k / income$nl_avg_income_k * 100), 0))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(
        text = htxt(tr("nl_inc_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 2),
      yaxis = list(title = "", tickprefix = euro_prefix, ticksuffix = k_suffix),
      showlegend = FALSE,
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  inc_leg <- make_html_legend(setNames(c("#c4793a", "#999999"),
                              c(tr("nl_leg_iranian_origin"), tr("nl_leg_nl_avg"))))

  # --- Low-income share ------------------------------------------------------
  p_lowinc <- plot_ly() %>%
    add_trace(x = inc_valid$year, y = inc_valid$nl_low_income_pct,
      type = "scatter", mode = "lines+markers", name = "NL average",
      line = list(color = "#999999", width = 2, dash = "dash"),
      marker = list(color = "#999999", size = 5),
      text = htxt(sprintf(tr("nl_lowinc_hover_nl"),
        fa_num(inc_valid$year, 0, big = FALSE), fa_num(inc_valid$nl_low_income_pct, 1))),
      hoverinfo = "text", textposition = "none") %>%
    add_trace(x = inc_valid$year, y = inc_valid$iran_low_income_pct,
      type = "scatter", mode = "lines+markers", name = "Iranian",
      line = list(color = "#c4793a", width = 3),
      marker = list(color = "#c4793a", size = 7),
      text = htxt(sprintf(tr("nl_lowinc_hover_iran"),
        fa_num(inc_valid$year, 0, big = FALSE), fa_num(inc_valid$iran_low_income_pct, 1))),
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("nl_lowinc_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", dtick = 2),
      yaxis = list(title = "", ticksuffix = pct_suffix),
      showlegend = FALSE,
      margin = list(t = 40, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  lowinc_leg <- make_html_legend(setNames(c("#c4793a", "#999999"),
                                 c(tr("nl_leg_iranian_origin"), tr("nl_leg_nl_avg"))))

  # --- Text-card pieces (htxt guards stray literal digits in fa prose) --------
  p1_big  <- htxt(sprintf(tr("nl_card_bignum"), fa_num(latest$participation_rate, 0)))
  p1_prim <- htxt(sprintf(tr("nl_card1_primary"), fa_num(latest$year, 0, big = FALSE)))
  p1_perm <- htxt(sprintf(tr("nl_card1_bullet_perm"), fa_num(latest$permanent_pct, 0)))
  p1_flex <- htxt(sprintf(tr("nl_card1_bullet_flex"), fa_num(latest$flexible_pct, 0)))
  p1_self <- htxt(sprintf(tr("nl_card1_bullet_self"), fa_num(latest$selfemployed_pct, 0)))

  p2_big  <- htxt(sprintf(tr("nl_card_bignum"), fa_num(inc_ratio, 0)))
  p2_prim <- htxt(sprintf(tr("nl_card2_primary"), fa_num(latest_inc$year, 0, big = FALSE)))
  p2_sec  <- htxt(sprintf(tr("nl_card2_secondary"),
    fa_num(li_peak, 0), fa_num(li_peak_yr, 0, big = FALSE),
    fa_num(li_latest, 0), fa_num(li_latest_yr, 0, big = FALSE)))

  workinc_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', p1_big, p1_prim, p1_perm, p1_flex, p1_self),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', p2_big, p2_prim, p2_sec),

    # Chart cell 1: tabbed (Participation | Employment Type)
    '<div class="chart-card pc1">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'nl-tab-part\',this,\'work-tabs\')">', tr("nl_tab_participation"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'nl-tab-emp\',this,\'work-tabs\')">', tr("nl_tab_emptype"), '</button>',
    '</div>',
    '<div id="nl-tab-part" class="tab-panel active" data-group="work-tabs">',
    plotly_div("nl-participation", pj(p_participation), "380px",
      source = cbs_lf_src),
    '</div>',
    '<div id="nl-tab-emp" class="tab-panel" data-group="work-tabs">',
    plotly_div("nl-emptype", pj(p_emp), "340px",
      source = cbs_lf_src, legend_html = emp_leg),
    '</div>',
    '</div>',

    # Chart cell 2: tabbed (Income | Low Income)
    '<div class="chart-card pc2">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'nl-tab-inc\',this,\'inc-tabs\')">', tr("nl_tab_income"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'nl-tab-low\',this,\'inc-tabs\')">', tr("nl_tab_lowinc"), '</button>',
    '</div>',
    '<div id="nl-tab-inc" class="tab-panel active" data-group="inc-tabs">',
    plotly_div("nl-income", pj(p_income), "340px",
      source = cbs_inc_src, legend_html = inc_leg),
    '</div>',
    '<div id="nl-tab-low" class="tab-panel" data-group="inc-tabs">',
    plotly_div("nl-lowinc", pj(p_lowinc), "340px",
      source = cbs_inc_src, legend_html = lowinc_leg),
    '</div>',
    '</div>',
    '</div>'
  )

  fname_wi <- if (is_fa()) "docs/pages/nl-workinc.fa.html" else "docs/pages/nl-workinc.html"
  writeLines(page_template_i18n(tr("nl_workinc_title"), workinc_body, has_tabs = TRUE, lang = LANG),
             fname_wi)
  cat("  Done\n")
}


# --- Summary ------------------------------------------------------------------
cat(sprintf("\nNetherlands: %s Iranian-origin (%s 1st gen + %s 2nd gen)\n",
  format(nl_total, big.mark = ","),
  format(nl_gen1, big.mark = ","),
  format(nl_gen2, big.mark = ",")))
cat(sprintf("Province map: %d provinces, Randstad share: %d%%\n",
  nrow(prov), randstad_pct))
cat(sprintf("Labour force: %d%% participation rate (%d)\n",
  latest$participation_rate, latest$year))
cat(sprintf("Income: Iranian avg €%sk vs NL avg €%sk (%d%%)\n",
  latest_inc$iran_avg_income_k, latest_inc$nl_avg_income_k, inc_ratio))

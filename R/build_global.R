# Build Global overview page — stacked area chart + world choropleth
# Input: data/global/stocks_countries.csv
# Output: docs/pages/global.html + docs/pages/global.fa.html
#
# Run from the iraniandiaspora.github.io/ directory:
#   Rscript R/build_global.R
#
# Data sourcing:
#   stocks_countries.csv is primarily UN International Migrant Stock (2024).
#   Countries with incomplete UN coverage are supplemented from Eurostat
#   (migr_pop3ctb). As of 2026-04-12, France is the only Eurostat-sourced
#   country (UN has no France row; Eurostat values from 2000-2018, with
#   2020/2024 carried forward from 2018). The headline total is computed
#   dynamically from the CSV sum, not from the UN's published total.
#
# Bilingual (en + fa), following the R/build_nl.R pattern: all user-facing
# strings come from R/i18n/strings_global.R via tr(); numbers go through
# fa_num()/fmtv() so the English edition stays BYTE-IDENTICAL while the
# Persian edition renders RTL with Persian digits and the Vazirmatn face.
# This builder assembles a bespoke inline shell (not page_template()), so the
# fa edition is produced by running the finished English shell through
# fa_shell() — the shell already carries the exact anchors fa_shell() expects.

library(plotly)
library(dplyr)
library(readr)
library(jsonlite)

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# Global string table (defines the global STR consumed by tr(), plus the
# GLOBAL_GROUP_KEYS / COUNTRY_FA display-name lookups).
source("R/i18n/strings_global.R")

# --- i18n formatting helpers (same shapes as build_nl.R) ----------------------
# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — so it reproduces format()'s common-width
#         PADDING (leading spaces) that the committed hover text relies on;
#         fa_num()/formatC() do NOT pad and would break byte-identity. In fa it
#         Persian-digits that same padded string (separators -> U+066C).
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)   # keep ASCII "," thousands (Iranian economic-press convention)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles, axis ticktext). Identity in en.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# grp_label(): display name for a streamgraph group. en = the group key itself
# (byte-identical); fa = Persian name. The trace `name` and the legend
# `data-lg` attribute keep the ENGLISH key in BOTH editions so the
# highlight/lock JS (which matches trace.name against data-lg) keeps working;
# the trace name is never displayed (hoverinfo = "text", showlegend = FALSE).
grp_label <- function(g) tr(GLOBAL_GROUP_KEYS[[g]])

# dest_label(): choropleth country display name (vectorized). en = passthrough;
# fa = COUNTRY_FA lookup with English fallback (never NA in hover text).
dest_label <- function(d) {
  if (!is_fa()) return(d)
  out <- unname(COUNTRY_FA[d])
  ifelse(is.na(out), d, out)
}

DATA_DIR <- "data/global"

cat("Building global...\n")

# --- Load UN Migrant Stock data ---
stocks <- read_csv(file.path(DATA_DIR, "stocks_countries.csv"), show_col_types = FALSE)
stocks$destination <- trimws(gsub("[*]", "", stocks$destination))

# Headline total computed from the CSV (UN + Eurostat supplement)
total_2024 <- sum(stocks$X2024, na.rm = TRUE)

# Group into 12 categories matching original dashboard
stocks <- stocks %>% mutate(group = case_when(
  destination == "United States of America" ~ "United States of America",
  destination == "Canada" ~ "Canada",
  destination == "Germany" ~ "Germany",
  destination == "United Kingdom" ~ "United Kingdom",
  destination == "Sweden" ~ "Sweden",
  destination == "Netherlands" ~ "Netherlands",
  destination %in% c("Turkey", "Türkiye", "Türkiye") ~ "Türkiye",
  destination == "Israel" ~ "Israel",
  destination == "Iraq" ~ "Iraq",
  destination == "Australia" ~ "Australia",
  # European countries not listed above
  destination %in% c("France", "Austria", "Italy", "Norway", "Denmark", "Spain",
    "Belgium", "Switzerland", "Finland", "Greece", "Ireland", "Portugal",
    "Luxembourg", "Iceland", "Czechia", "Hungary", "Poland", "Romania",
    "Bulgaria", "Cyprus", "Ukraine", "Estonia", "Slovakia", "Slovenia",
    "Malta", "Lithuania", "Belarus", "Latvia", "Croatia") ~ "Other (Europe)",
  TRUE ~ "Other (Non-Europe)"
))

years <- c("X1990", "X1995", "X2000", "X2005", "X2010", "X2015", "X2020", "X2024")

grouped <- stocks %>%
  group_by(group) %>%
  summarize(across(all_of(years), ~sum(.x, na.rm = TRUE)), .groups = "drop")

long <- grouped %>%
  tidyr::pivot_longer(cols = all_of(years), names_to = "year", values_to = "pop") %>%
  mutate(year = as.numeric(gsub("X", "", year)))

# Order: largest countries from center outward (for streamgraph centering)
# Match original legend order exactly
legend_order <- c("United States of America", "Canada", "Germany",
  "United Kingdom", "Sweden", "Netherlands", "Other (Europe)",
  "Türkiye", "Israel", "Iraq", "Australia", "Other (Non-Europe)")
long$group <- factor(long$group, levels = legend_order)

# Color palette matching original (blues/teals for Western, purples for MENA)
colors <- c(
  "United States of America" = "#08306b",
  "Canada" = "#2171b5",
  "Germany" = "#084c61",
  "United Kingdom" = "#0a6c5c",
  "Sweden" = "#2ca089",
  "Netherlands" = "#5ec4b6",
  "Other (Europe)" = "#b2e0d9",
  "Türkiye" = "#3b2066",
  "Israel" = "#5e3a8a",
  "Iraq" = "#8b6daf",
  "Australia" = "#1a7c8a",
  "Other (Non-Europe)" = "#b0b0b0"
)

# --- Streamgraph data prep (language-independent) ---
wide <- grouped
wide_mat <- as.matrix(wide[, years])
rownames(wide_mat) <- wide$group
wide_mat <- wide_mat[legend_order, ]

# Use only the 8 actual UN data points (no interpolation)
yr_nums_orig <- as.numeric(gsub("X", "", years))
yr_nums <- yr_nums_orig
wide_interp <- wide_mat

# Top: rev so US is outermost (top), Other(Europe) closest to center
# Bottom: no rev so legend order matches visual order (Türkiye near center, Other(Non-Europe) outermost bottom)
top_groups <- rev(legend_order[1:7])
bot_groups <- legend_order[8:12]

n_yr <- length(yr_nums)

# Top groups: stack upward from y=0
top_cumul <- matrix(0, nrow = length(top_groups) + 1, ncol = n_yr)
for (i in seq_along(top_groups)) {
  top_cumul[i + 1, ] <- top_cumul[i, ] + wide_interp[top_groups[i], ]
}

# Bottom groups: stack downward from y=0
bot_cumul <- matrix(0, nrow = length(bot_groups) + 1, ncol = n_yr)
for (i in seq_along(bot_groups)) {
  bot_cumul[i + 1, ] <- bot_cumul[i, ] - wide_interp[bot_groups[i], ]
}

# --- World choropleth data prep (language-independent) ---
stocks_2024 <- stocks %>%
  group_by(destination) %>%
  summarize(pop_2024 = sum(X2024, na.rm = TRUE), .groups = "drop") %>%
  filter(pop_2024 > 0) %>%
  arrange(desc(pop_2024))

iso_map <- c(
  "United States of America" = "USA", "Canada" = "CAN", "Germany" = "DEU",
  "United Kingdom" = "GBR", "Sweden" = "SWE", "Netherlands" = "NLD",
  "Türkiye" = "TUR", "Israel" = "ISR", "Iraq" = "IRQ", "Australia" = "AUS",
  "France" = "FRA", "Austria" = "AUT", "Italy" = "ITA", "Norway" = "NOR",
  "Denmark" = "DNK", "Spain" = "ESP", "Belgium" = "BEL", "Switzerland" = "CHE",
  "Finland" = "FIN", "Greece" = "GRC", "Japan" = "JPN", "India" = "IND",
  "Pakistan" = "PAK", "Afghanistan" = "AFG", "Kuwait" = "KWT",
  "United Arab Emirates" = "ARE", "Qatar" = "QAT", "Bahrain" = "BHR",
  "Oman" = "OMN", "Saudi Arabia" = "SAU", "Jordan" = "JOR", "Lebanon" = "LBN",
  "Syrian Arab Republic" = "SYR", "Egypt" = "EGY", "Libya" = "LBY",
  "South Africa" = "ZAF", "Brazil" = "BRA", "Argentina" = "ARG",
  "New Zealand" = "NZL", "Russian Federation" = "RUS", "Ukraine" = "UKR",
  "Georgia" = "GEO", "Armenia" = "ARM", "Azerbaijan" = "AZE",
  "Tajikistan" = "TJK", "Turkmenistan" = "TKM", "Uzbekistan" = "UZB",
  "Kazakhstan" = "KAZ", "Kyrgyzstan" = "KGZ", "China" = "CHN",
  "Republic of Korea" = "KOR", "Malaysia" = "MYS", "Thailand" = "THA",
  "Namibia" = "NAM", "Czechia" = "CZE", "Hungary" = "HUN", "Poland" = "POL",
  "Romania" = "ROU", "Bulgaria" = "BGR", "Ireland" = "IRL", "Portugal" = "PRT",
  "Luxembourg" = "LUX", "Iceland" = "ISL", "Cyprus" = "CYP",
  "State of Palestine" = "PSE", "Iran (Islamic Republic of)" = "IRN",
  # Additional small-population destinations (previously silently dropped)
  "Mexico" = "MEX", "Ecuador" = "ECU", "Estonia" = "EST", "Indonesia" = "IDN",
  "Slovakia" = "SVK", "Slovenia" = "SVN", "Malta" = "MLT", "Panama" = "PAN",
  "Costa Rica" = "CRI", "Sri Lanka" = "LKA",
  "Venezuela (Bolivarian Republic of)" = "VEN",
  "Bolivia (Plurinational State of)" = "BOL",
  "Lithuania" = "LTU", "Belarus" = "BLR", "Croatia" = "HRV", "Latvia" = "LVA",
  "Dominican Republic" = "DOM", "Guatemala" = "GTM", "El Salvador" = "SLV",
  "Liechtenstein" = "LIE", "Yemen" = "YEM", "Nauru" = "NRU"
)

# Some UN rows append a footnote asterisk to the country name (e.g. "United
# States of America*", "United Kingdom*"). Strip the trailing asterisk
# before matching so those rows still find their ISO3 code instead of being
# silently dropped from the world choropleth.
stocks_2024 <- stocks_2024 %>%
  mutate(destination_clean = sub("\\*$", "", destination),
         iso3 = iso_map[destination_clean])

# Log any residual mismatches so maintainers can extend the map next time.
missing_dests <- stocks_2024 %>% filter(is.na(iso3))
if (nrow(missing_dests) > 0) {
  message(sprintf(
    "build_global.R: %d destinations dropped from world map (%s Iran-born 2024):",
    nrow(missing_dests), format(sum(missing_dests$pop_2024, na.rm = TRUE), big.mark = ",")))
  for (i in seq_len(nrow(missing_dests))) {
    message(sprintf("  - %s (%s)",
                    missing_dests$destination[i],
                    format(missing_dests$pop_2024[i], big.mark = ",")))
  }
}
stocks_2024 <- stocks_2024 %>% filter(!is.na(iso3))
# Dedupe: some asterisked rows are the same country as the non-asterisked
# row with a UN footnote (rare in this dataset but keep the check).
stocks_2024 <- stocks_2024 %>%
  group_by(iso3, destination_clean) %>%
  summarize(destination = first(destination_clean),
            pop_2024 = sum(pop_2024, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(pop_2024))

stocks_2024 <- stocks_2024 %>% mutate(
  bin = case_when(
    pop_2024 <= 1000 ~ 1, pop_2024 <= 10000 ~ 2,
    pop_2024 <= 100000 ~ 3, TRUE ~ 4),
  bin_label = case_when(
    pop_2024 <= 1000 ~ "1 to 1,000", pop_2024 <= 10000 ~ "1,001 to 10,000",
    pop_2024 <= 100000 ~ "10,001 to 100,000", TRUE ~ "100,001 to 450,000")
)

iran_row <- data.frame(destination = "Iran", pop_2024 = 0, iso3 = "IRN",
  bin = 5, bin_label = "Origin country", stringsAsFactors = FALSE)
stocks_2024 <- bind_rows(stocks_2024, iran_row)

bin_colors <- list(c(0, "#e8e8e8"), c(0.2, "#c6dbef"), c(0.4, "#6baed6"),
  c(0.6, "#2171b5"), c(0.8, "#08306b"), c(1, "#f4c430"))

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# LANG is the loop variable; at top-level Rscript scope it lives in the global
# env, which is where the i18n helpers read it.
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building global [%s] ===\n", LANG))

  total_label <- sprintf(tr("gl_total_label"), fa_num(total_2024 / 1e6, 2))

  # Streamgraph fill-hover fix. Each filled trace keeps its ENGLISH `name` (the
  # legend/highlight JS matches trace.name against data-lg). With hoveron="fills"
  # and a per-point `text` ARRAY, Plotly falls back to that English name for the
  # fill label — so the fa fill hover showed "United States of America" instead
  # of the Persian country name already sitting in `text`. A `hovertemplate`
  # overrides the name-box and renders the per-point Persian `text` (with
  # <extra></extra> suppressing the trace-name secondary box). fa-only (NULL in
  # en, which plotly_build drops), so the English page stays byte-identical.
  stream_hovertmpl <- if (is_fa()) "%{text}<extra></extra>" else NULL

  # Source-line link anchors are per-language: the fa anchors carry the Persian
  # agency gloss with the Latin acronym isolated in <bdi> (glossary rule); the
  # en anchors reproduce the committed text exactly.
  un_link <- sprintf('<a href="https://www.un.org/development/desa/pd/content/international-migrant-stock" target="_blank" style="color:#2774AE;">%s</a>',
    tr("gl_un_link_text"))
  euro_link <- sprintf('<a href="https://ec.europa.eu/eurostat" target="_blank" style="color:#2774AE;">%s</a>',
    tr("gl_euro_link_text"))
  src_area <- sprintf(tr("gl_src_area"), un_link, euro_link)
  src_map  <- sprintf(tr("gl_src_map"),  un_link, euro_link)

  # --- Build centered streamgraph (manual plotly) ---
  p_stock <- plot_ly()

  for (i in seq_along(top_groups)) {
    g <- top_groups[i]
    vals <- as.numeric(wide_interp[g, ])
    y0 <- as.numeric(top_cumul[i, ])
    y1 <- as.numeric(top_cumul[i + 1, ])
    if (i == 1) {
      p_stock <- p_stock %>%
        add_trace(x = yr_nums, y = y0, type = "scatter", mode = "lines",
          showlegend = FALSE, hoverinfo = "skip",
          line = list(width = 0, color = "transparent", shape = "spline"))
    }
    p_stock <- p_stock %>%
      add_trace(x = yr_nums, y = y1, type = "scatter", mode = "lines+markers",
        fill = "tonexty", fillcolor = colors[g], name = g,
        line = list(width = 0.5, color = colors[g], shape = "spline"),
        marker = list(size = 8, color = "rgba(0,0,0,0)"),
        text = sprintf(tr("gl_stream_hover"),
          grp_label(g), fa_num(yr_nums, 0, big = FALSE), fmtv(vals)),
        hoverinfo = "text", hovertemplate = stream_hovertmpl)
  }

  for (i in seq_along(bot_groups)) {
    g <- bot_groups[i]
    vals <- as.numeric(wide_interp[g, ])
    y0 <- as.numeric(bot_cumul[i, ])
    y1 <- as.numeric(bot_cumul[i + 1, ])
    if (i == 1) {
      p_stock <- p_stock %>%
        add_trace(x = yr_nums, y = y0, type = "scatter", mode = "lines",
          showlegend = FALSE, hoverinfo = "skip",
          line = list(width = 0, color = "transparent", shape = "spline"))
    }
    p_stock <- p_stock %>%
      add_trace(x = yr_nums, y = y1, type = "scatter", mode = "lines+markers",
        fill = "tonexty", fillcolor = colors[g], name = g,
        line = list(width = 0.5, color = colors[g], shape = "spline"),
        marker = list(size = 8, color = "rgba(0,0,0,0)"),
        text = sprintf(tr("gl_stream_hover"),
          grp_label(g), fa_num(yr_nums, 0, big = FALSE), fmtv(vals)),
        hoverinfo = "text", hovertemplate = stream_hovertmpl)
  }

  p_stock <- p_stock %>% layout(
    title = list(text = tr("gl_stream_title"),
      font = list(size = 16, family = "Montserrat", color = "#333"), x = 0.5, xanchor = "center"),
    xaxis = list(title = "", dtick = 5, tickvals = yr_nums_orig,
      ticktext = htxt(as.character(yr_nums_orig)), tickfont = list(size = 12),
      showgrid = TRUE, gridcolor = "#e8e8e8"),
    yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
    showlegend = FALSE,
    margin = list(t = 55, b = 60, l = 20, r = 20),
    plot_bgcolor = "white", paper_bgcolor = "white",
    font = list(family = "Montserrat, sans-serif"),
    hoverlabel = list(bgcolor = "white", bordercolor = "#ccc",
      font = list(family = "Montserrat, sans-serif", size = 13, color = "#333")),
    hovermode = "closest",
    annotations = list()
  ) %>% config(displayModeBar = FALSE)

  # --- World choropleth ---
  p_world <- plot_ly(type = "choropleth",
    locations = stocks_2024$iso3, z = stocks_2024$bin,
    text = ifelse(stocks_2024$destination == "Iran",
      tr("gl_map_hover_iran"),
      htxt(sprintf(tr("gl_map_hover"),
        dest_label(stocks_2024$destination), fmtv(stocks_2024$pop_2024)))),
    hoverinfo = "text",
    colorscale = bin_colors, zmin = 0, zmax = 5, showscale = FALSE,
    marker = list(line = list(color = "white", width = 0.5))
  ) %>% layout(
    title = list(text = ""),
    geo = list(showframe = FALSE, showcoastlines = TRUE, coastlinecolor = "#ccc",
      projection = list(type = "natural earth"), bgcolor = "white",
      landcolor = "#f0f0f0", showland = TRUE),
    margin = list(t = 10, b = 10, l = 0, r = 0),
    paper_bgcolor = "white",
    hoverlabel = list(align = "left")
  ) %>% config(displayModeBar = FALSE)

  # --- Assemble page (side-by-side layout) ---
  global_page <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>', tr("gl_page_title"), '</title>
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap" rel="stylesheet">
<script src="lib/plotly-3.4.0.min.js"></script>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family:"Montserrat",sans-serif; background:#fafafa; color:#333; padding:15px 40px; max-width:100%; overflow-x:hidden; }
.global-grid { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.text-card { background:white; border-radius:8px; padding:20px; text-align:center;
  font-size:15px; line-height:1.6; border:1px solid #e0e0e0; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; overflow:hidden; min-width:0; }
@media (max-width:900px) {
  body { padding:10px 15px; }
  .global-grid { grid-template-columns:1fr; }
  .chart-with-legend { flex-direction:column !important; }
  .chart-legend-sidebar { width:auto !important; display:flex; flex-wrap:wrap; gap:4px 16px; padding:10px 0 0 0 !important; justify-content:center; line-height:1.8 !important; }
  /* Mobile reorder: area chart, text1, world map, text2 */
  .global-area  { order:1; }
  .global-text1 { order:2; }
  .global-map   { order:3; }
  .global-text2 { order:4; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .chart-card { padding:10px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
</head>
<body>

<div class="global-grid">
<div class="text-card global-text1" style="text-align:center;">
  <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">', total_label, '</div>
  <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">', tr("gl_card1_primary"), '</div>
</div>
<div class="text-card global-text2" style="text-align:center;">
  <div style="font-size:15px; font-weight:700; color:#1a4e72; line-height:1.45;">', tr("gl_card2_heading"), '</div>
  <ul style="margin:10px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
    <li>', tr("gl_card2_bullet1"), '</li>
    <li>', tr("gl_card2_bullet2"), '</li>
    <li>', tr("gl_card2_bullet3"), '</li>
    <li>', tr("gl_card2_bullet4"), '</li>
  </ul>
  <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">', tr("gl_card2_note"), '</div>
</div>
<div class="chart-card global-area" style="overflow:visible;">
  <div class="chart-with-legend" style="display:flex; align-items:stretch;">
    <div style="flex:1; min-width:0;">',
      plotly_div("stock-area", pj(p_stock, inject_hoveron = !is_fa()), "500px"),
    '</div>
    <div class="chart-legend-sidebar" style="width:200px; flex-shrink:0; padding:40px 10px 0 5px; font-size:13px; line-height:2.2;">',
      paste(sapply(legend_order, function(g) {
        html_g <- gsub("&", "&amp;", grp_label(g))
        sprintf("<div data-lg=\"%s\" style=\"display:flex; align-items:center; gap:8px; cursor:pointer; transition:opacity 0.2s;\" onmouseenter=\"var el=document.getElementById('stock-area');if(el&&el.__hlOn&&!el.__locked)el.__hlOn(this.getAttribute('data-lg'));\" onmouseleave=\"var el=document.getElementById('stock-area');if(el&&el.__hlOff&&!el.__locked)el.__hlOff();\" onclick=\"var el=document.getElementById('stock-area');if(el&&el.__toggle)el.__toggle(this.getAttribute('data-lg'));\"><div style=\"width:14px; height:14px; border-radius:50%%; background:%s; flex-shrink:0;\"></div> %s</div>", g, colors[g], html_g)
      }), collapse = "\n"),
    '</div>
  </div>
  <p style="font-size:10px; color:#6b6b6b; text-align:right; margin:4px 4px 0 0;">', src_area, '</p>
  <script>(function(){
    var el=document.getElementById("stock-area");
    if(!el)return;
    var card=el.closest(".chart-card");
    var origColors=null;
    var locked=null;
    el.__hlOn=function(name){
      if(!origColors){origColors=el.data.map(function(t){return t.fillcolor||null;});}
      el.data.forEach(function(t,i){
        var match=t.name===name;
        var updates={opacity:match?1:0.12};
        if(t.fillcolor){updates.fillcolor=match?origColors[i]:"rgba(200,200,200,0.15)";}
        if(t.line){updates["line.color"]=match?origColors[i]:"rgba(200,200,200,0.15)";}
        Plotly.restyle(el,updates,[i]);
      });
      if(card){card.querySelectorAll("[data-lg]").forEach(function(s){
        s.style.opacity=s.getAttribute("data-lg")===name?1:0.3;
      });}
    };
    el.__hlOff=function(){
      if(!origColors)return;
      locked=null;
      el.data.forEach(function(t,i){
        var updates={opacity:1};
        if(origColors[i]){updates.fillcolor=origColors[i];updates["line.color"]=origColors[i];}
        Plotly.restyle(el,updates,[i]);
      });
      if(card){card.querySelectorAll("[data-lg]").forEach(function(s){s.style.opacity=1;});}
    };
    el.__toggle=function(name){
      if(locked===name){el.__hlOff();}
      else{locked=name;el.__hlOn(name);}
    };
    el.on("plotly_hover",function(d){
      if(locked)return;
      if(d.points&&d.points[0]&&d.points[0].data.name){el.__hlOn(d.points[0].data.name);}
    });
    el.on("plotly_unhover",function(){if(!locked)el.__hlOff();});
    el.on("plotly_click",function(d){
      if(d.points&&d.points[0]&&d.points[0].data.name){el.__toggle(d.points[0].data.name);}
    });
  })();</script>
</div>
<div class="chart-card global-map">',
  '<div style="text-align:center; font-size:16px; font-weight:600; margin:4px 0 12px;">', tr("gl_map_section_title"), '</div>',
  '<div style="display:flex; justify-content:center; flex-wrap:wrap; gap:12px; font-size:13px; color:#444; margin:0 0 12px; line-height:1;">',
  '<span style="display:inline-flex;align-items:center;gap:4px;"><span style="display:inline-block;width:16px;height:16px;background:#f4c430;border-radius:2px;"></span> ', tr("gl_leg_iran"), '</span>',
  '<span style="display:inline-flex;align-items:center;gap:4px;"><span style="display:inline-block;width:16px;height:16px;background:#c6dbef;border-radius:2px;"></span> ', tr("gl_leg_bin1"), '</span>',
  '<span style="display:inline-flex;align-items:center;gap:4px;"><span style="display:inline-block;width:16px;height:16px;background:#6baed6;border-radius:2px;"></span> ', tr("gl_leg_bin2"), '</span>',
  '<span style="display:inline-flex;align-items:center;gap:4px;"><span style="display:inline-block;width:16px;height:16px;background:#2171b5;border-radius:2px;"></span> ', tr("gl_leg_bin3"), '</span>',
  '<span style="display:inline-flex;align-items:center;gap:4px;"><span style="display:inline-block;width:16px;height:16px;background:#08306b;border-radius:2px;"></span> ', tr("gl_leg_bin4"), '</span>',
  '<span style="display:inline-flex;align-items:center;gap:4px;"><span style="display:inline-block;width:16px;height:16px;background:#e8e8e8;border:1px solid #ccc;border-radius:2px;"></span> ', tr("gl_leg_nodata"), '</span>',
  '</div>',
  plotly_div("world-map", pj(p_world), "460px"),
  '<p style="font-size:10px; color:#6b6b6b; text-align:right; margin:4px 4px 0 0;">', src_map, '</p>
</div>
</div>

', iframe_resize_script, '
</body>
</html>')

  # fa edition: transform the finished English shell (RTL, Vazirmatn, Persian
  # locale/digit scripts). fa path only — English bytes never change.
  if (is_fa()) global_page <- fa_shell(global_page)

  fname <- if (is_fa()) "docs/pages/global.fa.html" else "docs/pages/global.html"
  writeLines(global_page, fname)
  cat("  Done\n")
}

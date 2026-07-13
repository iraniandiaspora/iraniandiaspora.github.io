# Build all Germany pages from Mikrozensus 2025 Iran extracts + BAMF figures.
# Run from deployment repo root:
#   Rscript R/build_germany.R
#
# Input:  data/germany/*.csv (see R/germany_export/extract_mikrozensus.R)
#         data/germany/bundeslaender.geojson
# Output: docs/pages/de-population.html   + de-population.fa.html
#         docs/pages/de-immigration.html  + de-immigration.fa.html
#         docs/pages/de-langedu.html      + de-langedu.fa.html
#         docs/pages/de-workinc.html      + de-workinc.fa.html
#
# Bilingual (en + fa) following the build_nl.R / build_denmark.R pattern.
# Germany keeps its LOCAL page_template() (has_tabs / extra_head) and its LOCAL
# tab_switch_script (which re-attaches the language-chart highlight handlers on
# tab switch). All user-facing strings come from R/i18n/strings_germany.R via
# tr(); numbers go through fa_num()/fmtv() so the four English editions stay
# BYTE-IDENTICAL while the Persian editions render RTL with Persian digits and
# the Vazirmatn face. The en path calls the local page_template() unchanged; the
# fa path runs that same shell through fa_shell() (its font token already matches
# the canonical anchor, so no realignment is needed).

library(plotly)
library(dplyr)
library(jsonlite)

DATA_DIR <- "data/germany"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, hbar_over_labels(), pct_lab(),
# cat_colors(), make_html_legend_hover(), OKABE_ITO, CAT_OTHER.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# Germany string table (defines the global STR consumed by tr(), plus the
# fa-only category display vectors DE_MOTIVE_FA / DE_SCHOOL_FA / ...).
source("R/i18n/strings_germany.R")

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
    if (window.Plotly) {
      Plotly.Plots.resize(p);
      if (p.__hlOn) {
        p.removeAllListeners("plotly_hover");
        p.removeAllListeners("plotly_unhover");
        p.on("plotly_hover", function(d) { p.__hlOn(d.points[0].data.legendgroup); });
        p.on("plotly_unhover", p.__hlOff);
      }
    }
  });
  reportHeight();
}
</script>'

page_template <- function(title, body_html, has_tabs = FALSE, extra_head = "") {
  tab_css <- if (has_tabs) '
.tab-bar { display:flex; justify-content:center; gap:0; margin:12px 0 0; }
.tab-btn { padding:6px 16px; border:1px solid #ddd; background:#f0f0f0; cursor:pointer;
  font-family:"Montserrat",sans-serif; font-size:13px; color:#333; border-radius:4px; margin:0 2px; transition:background 0.15s; white-space:nowrap; }
.tab-btn.active { background:#2774AE; color:white; font-weight:600; border-color:#2774AE; }
.tab-btn:hover:not(.active) { background:#e0e0e0; }
.tab-panel { display:none; }
.tab-panel.active { display:block; }' else ''

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
.text-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.text-row-4 { display:grid; grid-template-columns:repeat(4,1fr); gap:16px; margin-bottom:20px; }
.text-card { background:white; border-radius:8px; padding:20px; text-align:center;
  font-size:15px; line-height:1.6; border:1px solid #e0e0e0; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:hidden; min-width:0; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.headline { background:white; border-radius:8px; padding:30px; text-align:center; border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em; }
a { transition: color 0.15s; }
a:hover { color: #1a4e72 !important; text-decoration: underline; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
.source { font-size:12px; color:#666; text-align:right; padding:4px 0; margin-top:10px; }
.source a { color:#2774AE; }
.footnote { font-size:12px; color:#6b6b6b; text-align:center; margin:8px 0; font-style:italic; }
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }', tab_css, '
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .text-row-4 { grid-template-columns:1fr 1fr; }
  .text-row, .text-row-4 { order:1; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
  .measure-num { font-size:28px !important; }
}
@media (max-width:600px) {
  .text-row-4 { grid-template-columns:1fr !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .chart-card { padding:10px; }
  .measure-num { font-size:24px !important; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
', extra_head, '
</head>
<body>
', body_html, '
', if (has_tabs) tab_switch_script else '', '
', iframe_resize_script, '
</body>
</html>')
}

# render(): en delegates to the local page_template() (byte-identical English);
# fa is that same shell run through fa_shell() (the font token already matches
# the canonical anchor, so no realignment is required).
render <- function(title, body_html, has_tabs = FALSE) {
  html <- page_template(title, body_html, has_tabs)
  if (is_fa()) html <- fa_shell(html)
  html
}

# --- i18n formatting helpers (build_nl.R pattern) -----------------------------
# lnk():  in fa, isolate a Latin agency link/URL in <bdi> so bidi ordering is
#         correct; in en, pass through unchanged (keeps English byte-identical).
lnk <- function(x) if (is_fa()) bdi(x) else x

# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — reproducing format()'s common-width PADDING
#         (leading spaces) that the committed hover text relies on. In fa it
#         Persian-digits that padded string (ASCII "," thousands kept, per press).
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)
}

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles, card prose). Idempotent on already-Persian
#         digits. NEVER apply to HTML that carries CSS — only to plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source links / anchors (language-independent) ----------------------
MZ_LINK <- "<a href='https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Migration-Integration/Publikationen/Downloads-Migration/statistischer-bericht-migrationshintergrund-erst-2010220257005.html' target='_blank' style='color:#2774AE;'>Destatis</a>"
MZ_HEADLINE_LINK <- '<a href="https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Migration-Integration/Methoden/mikrozensus.html" style="color:#2774AE;" target="_blank">2025 Mikrozensus</a>'

blues <- c("#1a4e72", "#2774AE", "#5a9bd5", "#8bbdde", "#d4e6f1")

# --- Load all extracts (ONCE — language-independent) --------------------------
cat("Loading Mikrozensus extracts from ", DATA_DIR, "...\n", sep = "")
hl <- read.csv(file.path(DATA_DIR, "de_headline.csv"))
bund <- read.csv(file.path(DATA_DIR, "de_bundesland.csv"))
school <- read.csv(file.path(DATA_DIR, "de_school.csv"))
prof <- read.csv(file.path(DATA_DIR, "de_profqual.csv"))
emp <- read.csv(file.path(DATA_DIR, "de_employment.csv"))
lang <- read.csv(file.path(DATA_DIR, "de_language.csv"))
motive <- read.csv(file.path(DATA_DIR, "de_motive.csv"))
nat <- read.csv(file.path(DATA_DIR, "de_naturalization.csv"))
duration <- read.csv(file.path(DATA_DIR, "de_duration.csv"))
annual_arrivals <- read.csv(file.path(DATA_DIR, "de_annual_arrivals.csv"))

# Headline lookups (values are in thousands; convert to persons)
hl_total <- hl$value_thousands[hl$category == "total"] * 1000
hl_fg    <- hl$value_thousands[hl$category == "first_gen"] * 1000
hl_sg    <- hl$value_thousands[hl$category == "second_gen"] * 1000
hl_deu   <- hl$value_thousands[hl$category == "german_citizens"] * 1000
hl_for   <- hl$value_thousands[hl$category == "foreign_citizens"] * 1000

# Generation breakdown box (Germany LOCAL variant — 38px number, min-height 170).
# Takes a PRE-FORMATTED value string so the fa edition can pass Persian digits;
# en passes fmtv() = format(val, big.mark=","), keeping the output byte-identical.
make_mig_box <- function(val_str, pct, label, sublabel, color) {
  sprintf('<div style="background:%s; border-radius:6px; padding:28px 18px; text-align:center; color:white; flex:1; min-width:0; display:flex; flex-direction:column; justify-content:center; min-height:170px;">
    <div class="measure-num" style="font-size:38px; font-weight:700; line-height:1.1;">%s</div>
    <div class="measure-label" style="font-size:14px; margin-top:6px; font-weight:600;">%s</div>
    <div style="font-size:12px; opacity:0.9; margin-top:3px;">%s</div>
    <div style="font-size:11px; opacity:0.85; margin-top:4px;">%s</div>
  </div>',
    color, val_str, label, pct, sublabel)
}

# =============================================================================
# LANGUAGE-INDEPENDENT DATA PREP (numeric / factor / colour; computed ONCE)
# =============================================================================

# --- DE-POPULATION: Bundesland bar + choropleth ------------------------------
bund_all <- bund %>% filter(gen == "all_gens") %>%
  mutate(is_suppressed = is.na(value_k),
         value = ifelse(is_suppressed, 0, value_k * 1000)) %>%
  arrange(desc(value), is_suppressed)
bar_df <- bund_all %>% select(land, value, is_suppressed)
bar_df$pct <- round(bar_df$value / hl_total * 100, 1)
bar_df$fill_color <- ifelse(bar_df$is_suppressed, "#c8c8c8", "#2774AE")
bar_df$display_value <- ifelse(bar_df$is_suppressed, 2500, bar_df$value)
bar_df$land <- factor(bar_df$land, levels = bar_df$land)

bund_map_data <- bund %>% filter(gen == "all_gens") %>%
  mutate(is_suppressed = is.na(value_k),
         value = ifelse(is_suppressed, NA_real_, value_k * 1000))
bund_map_visible <- bund_map_data %>% filter(!is_suppressed)
bund_map_suppressed <- bund_map_data %>% filter(is_suppressed)

de_geojson <- jsonlite::fromJSON(file.path(DATA_DIR, "bundeslaender.geojson"),
  simplifyVector = FALSE)

# --- DE-IMMIGRATION: motive / duration / citizenship -------------------------
motive_clean <- motive %>%
  mutate(short = case_when(
    motive == "Flucht, Asyl, internationaler Schutz" ~ "Flight / asylum",
    motive == "Familienzusammenführung" ~ "Family reunification",
    motive == "Familiengründung" ~ "Family formation",
    motive == "Studium, Ausbildung, Weiterbildung" ~ "Study / training",
    motive == "Arbeit/Beschäftigung zusammen" ~ "Work",
    motive == "sonstige Gründe" ~ "Other",
    motive == "Hauptmotiv für die Zuwanderung Insgesamt" ~ "TOTAL",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(short), short != "TOTAL") %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)
motive_total <- hl_fg
motive_clean$pct <- round(motive_clean$value / motive_total * 100, 1)
motive_clean <- motive_clean %>% arrange(desc(value))
motive_clean$short <- factor(motive_clean$short, levels = motive_clean$short)
# Shared Okabe-Ito categorical palette (named map so a category keeps its color
# regardless of sort order); the "Other" catch-all stays grey (CAT_OTHER).
motive_colors <- c(
  "Flight / asylum"      = "#0072B2",
  "Study / training"     = "#E69F00",
  "Family reunification" = "#009E73",
  "Work"                 = "#CC79A7",
  "Family formation"     = "#56B4E9",
  "Other"                = "#b0b0b0"
)

dur_labels <- c(
  "under_5"  = "< 5",
  "5_to_10"  = "5–10",
  "10_to_15" = "10–15",
  "15_to_20" = "15–20",
  "20_to_25" = "20–25",
  "25_to_30" = "25–30",
  "30_to_40" = "30–40",
  "40_plus"  = "40+"
)
dur_df <- duration %>%
  mutate(
    value = ifelse(is.na(value_k), 0, value_k) * 1000,
    label = dur_labels[bucket]
  )
# Order DESCENDING (40+ -> <5) so the earliest-arrived cohort sits on the LEFT.
dur_df$label <- factor(dur_df$label, levels = rev(unname(dur_labels)))
dur_df$pct <- round(dur_df$value / hl_fg * 100, 1)

# Dynamic shares for the immigration factoid cards
de_cit_pct <- round(hl_deu / hl_total * 100)
dur_10plus_k <- sum(duration$value_k[duration$bucket %in% c(
  "10_to_15","15_to_20","20_to_25","25_to_30","30_to_40","40_plus")], na.rm = TRUE)
dur_30plus_k <- sum(duration$value_k[duration$bucket %in% c(
  "30_to_40","40_plus")], na.rm = TRUE)
fg_k <- hl_fg / 1000
dur_10plus_pct <- round(dur_10plus_k / fg_k * 100)
dur_30plus_pct <- round(dur_30plus_k / fg_k * 100)

# --- DE-EDUCATION: school + vocational/academic ------------------------------
school_cats <- c(
  "Ohne Schulabschluss" = "No school<br>certificate",
  "Darunter: Hauptschule" = "Basic<br>secondary",
  "Realschule o. ä." = "Intermediate<br>secondary",
  "Fachhochschulreife" = "Applied-university<br>entrance",
  "Abitur" = "University<br>entrance"
)
school_clean <- school %>%
  mutate(cat = school_cats[school_level]) %>%
  filter(!is.na(cat)) %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)
school_all <- school_clean %>% filter(gen == "all_gens") %>%
  mutate(pct = round(value / hl_total * 100, 1))
school_all$cat <- factor(school_all$cat, levels = unname(school_cats))
school_colors <- c(
  "No school<br>certificate" = "#c6dbef",
  "Basic<br>secondary" = "#8bbdde",
  "Intermediate<br>secondary" = "#5a9bd5",
  "Applied-university<br>entrance" = "#2774AE",
  "University<br>entrance" = "#1a4e72"
)

prof_clean <- prof %>%
  mutate(cat = case_when(
    prof_level == "Ohne berufsqualifizierenden Abschluss" ~ "No vocational qualification",
    prof_level == "darunter: nicht-akademischer Abschluss" ~ "Vocational (non-academic)",
    prof_level == "akademischer Abschluss" ~ "Academic degree",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(cat)) %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)
prof_all <- prof_clean %>% filter(gen == "all_gens") %>%
  mutate(pct = round(value / hl_total * 100, 1))
prof_cat_order <- c("No vocational qualification", "Vocational (non-academic)", "Academic degree")
prof_all$cat <- factor(prof_all$cat, levels = prof_cat_order)
prof_all <- prof_all %>% arrange(cat)
prof_colors <- c(
  "No vocational qualification" = "#c6dbef",
  "Vocational (non-academic)" = "#5a9bd5",
  "Academic degree" = "#1a4e72"
)

# --- DE-WORK: labour-force status + industry ---------------------------------
emp_status <- emp %>% filter(section == "status", gen == "all_gens") %>%
  mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)
employed    <- emp_status$value[emp_status$category == "Erwerbstätige"]
unemployed  <- emp_status$value[emp_status$category == "Erwerbslose"]
inactive    <- emp_status$value[emp_status$category == "Nichterwerbspersonen"]
emp_total   <- employed + unemployed + inactive
emp_bar_df <- data.frame(
  status = c("Employed", "Unemployed", "Not in labor force"),
  count  = c(employed, unemployed, inactive),
  stringsAsFactors = FALSE
)
emp_bar_df$pct <- round(emp_bar_df$count / emp_total * 100, 1)
emp_bar_df$status <- factor(emp_bar_df$status, levels = emp_bar_df$status)

# True employed total ("Erwerbstätige") — the denominator for industry %.
hl_employed <- emp$value_k[emp$section == "status" &
                           emp$category == "Erwerbstätige" &
                           emp$gen == "all_gens"] * 1000

ind <- emp %>% filter(section == "industry", gen == "all_gens") %>%
  mutate(is_suppressed = is.na(value_k),
         value = ifelse(is_suppressed, 0, value_k * 1000),
         label = case_when(
           category == "Land- und Forstwirtschaft, Fischerei" ~ "Agriculture & fishing",
           category == "Produzierendes Gewerbe, Baugewerbe" ~ "Industry & construction",
           category == "Handel, Gastgewerbe und Verkehr" ~ "Trade, hospitality & transport",
           category == "Öffentliche Verwaltung" ~ "Public administration",
           category == "Sonstige Dienstleistungen" ~ "Other services",
           TRUE ~ category
         )) %>%
  arrange(desc(value), is_suppressed)
ind$pct <- round(ind$value / hl_employed * 100, 1)
ind$display_value <- ifelse(ind$is_suppressed, 3000, ind$value)
# Horizontal bars: suppressed rows at the bottom, then non-suppressed ascending.
ind <- ind %>% arrange(desc(is_suppressed), value)
ind$label <- factor(ind$label, levels = ind$label)
# Okabe-Ito palette for the visible sectors; suppressed stay grey (a separate
# signal). Colors applied by suppression mask, not row index.
visible_idx <- which(!ind$is_suppressed)
ind$fill_color <- rep("#c8c8c8", nrow(ind))
ind$fill_color[visible_idx] <- cat_colors(length(visible_idx))
# Label-above-bar geometry (language-independent parts): plot order, bar ends,
# and the base % end-text (blank for suppressed) — the fa edition re-styles the
# end-text (Persian digits + ٪) inside the loop.
de_ind_levels_en <- levels(ind$label)
de_ind_ord   <- match(de_ind_levels_en, as.character(ind$label))
de_ind_ends  <- ind$display_value[de_ind_ord]
de_ind_base_text <- ifelse(ind$is_suppressed[de_ind_ord], "", pct_lab(ind$pct[de_ind_ord]))
de_ind_xmax  <- max(ind$display_value) * 1.15

# --- DE-INCOME: monthly net income brackets ----------------------------------
inc <- emp %>% filter(section == "income", gen == "all_gens") %>%
  mutate(is_suppressed = is.na(value_k),
         value = ifelse(is_suppressed, 0, value_k) * 1000,
         bracket = case_when(
           grepl("unter 500", category) ~ "Under €500",
           category == "500 - 1 000" ~ "€500–€1,000",
           category == "1 000 - 1 500" ~ "€1,000–€1,500",
           category == "1 500 - 2 000" ~ "€1,500–€2,000",
           category == "2 000 - 2 500" ~ "€2,000–€2,500",
           category == "2 500 - 3 000" ~ "€2,500–€3,000",
           category == "3 000 - 3 500" ~ "€3,000–€3,500",
           category == "3 500 und mehr" ~ "€3,500+",
           category == "Kein Einkommen" ~ "No income",
           TRUE ~ category
         )) %>%
  filter(bracket != "No income")
inc_denom <- sum(inc$value, na.rm = TRUE)
inc$pct <- round(inc$value / inc_denom * 100, 1)
inc_order <- c("Under €500", "€500–€1,000", "€1,000–€1,500",
               "€1,500–€2,000", "€2,000–€2,500",
               "€2,500–€3,000", "€3,000–€3,500", "€3,500+")
inc$bracket <- factor(inc$bracket, levels = inc_order)
inc <- inc %>% arrange(bracket)
inc_colors <- c("#d4e6f1", "#bcdcec", "#a3d2e6", "#8bbdde", "#5a9bd5", "#2774AE", "#1a4e72", "#0d2f4a")
inc$bar_color <- inc_colors[as.integer(inc$bracket)]
inc$bar_color[inc$is_suppressed] <- "#c8c8c8"
inc$display_pct <- ifelse(inc$is_suppressed, 1.2, inc$pct)
middle_share <- sum(inc$pct[inc$bracket %in% c("€1,000–€1,500", "€1,500–€2,000", "€2,000–€2,500")])
top_share    <- sum(inc$pct[inc$bracket %in% c("€3,000–€3,500", "€3,500+")])
low_share    <- sum(inc$pct[inc$bracket %in% c("Under €500", "€500–€1,000")])

# --- DE-WORK & INCOME: factoid-card shares -----------------------------------
emp_all <- emp %>% filter(gen == "all_gens")
employed_k   <- emp_all$value_k[grepl("^Erwerbstätige$", emp_all$category)][1]
unemployed_k <- emp_all$value_k[grepl("Erwerbslose|^Arbeitslose", emp_all$category)][1]
nilf_k       <- emp_all$value_k[grepl("Nichterwerbspersonen", emp_all$category)][1]
services_k   <- sum(emp_all$value_k[emp_all$category %in% c(
  "Handel, Gastgewerbe und Verkehr", "Sonstige Dienstleistungen")], na.rm = TRUE)
services_pct <- round(services_k / employed_k * 100)
total_15plus_k <- hl_total / 1000
emp_pct      <- round(employed_k / total_15plus_k * 100)
unemp_pct    <- round(unemployed_k / total_15plus_k * 100)
nilf_pct     <- round(nilf_k / total_15plus_k * 100)

# --- DE-LANGUAGE rollup ------------------------------------------------------
lang_rollup <- function(g) {
  sub <- lang %>% filter(gen == g) %>%
    mutate(value = ifelse(is.na(value_k), 0, value_k) * 1000)
  total <- sub$value[sub$language == "Zu Hause vorwiegend gesprochene Sprache Insgesamt"]
  only_de   <- sub$value[sub$language == "nur Deutsch"]
  pred_de   <- sub$value[sub$language == "vorwiegend deutsch"]
  persian   <- sub$value[sub$language == "Persisch"]
  pred_nde  <- sub$value[sub$language == "vorwiegend nicht-deutsch"]
  other_nde <- max(pred_nde - persian, 0)
  data.frame(
    gen = g,
    category = c("German only", "Mostly German", "Mostly Persian",
                 "Mostly other non-German"),
    value = c(only_de, pred_de, persian, other_nde),
    stringsAsFactors = FALSE
  ) %>% mutate(pct = round(value / total * 100, 1))
}
lang_all <- bind_rows(lang_rollup("all_gens"), lang_rollup("first_gen"))
lang_all$gen_label <- factor(ifelse(lang_all$gen == "all_gens",
  "All Iranian-origin", "First generation only"),
  levels = c("All Iranian-origin", "First generation only"))
lang_cat_order <- c("German only", "Mostly German", "Mostly Persian", "Mostly other non-German")
lang_all$category <- factor(lang_all$category, levels = lang_cat_order)
lang_colors <- c(
  "German only" = "#1a4e72",
  "Mostly German" = "#5a9bd5",
  "Mostly Persian" = "#4a8c6f",
  "Mostly other non-German" = "#b0b0b0"
)

# --- DE-LANGUAGE & EDUCATION factoid-card shares -----------------------------
lang_total <- lang$value_k[lang$language == "Zu Hause vorwiegend gesprochene Sprache Insgesamt" & lang$gen == "all_gens"]
persian_k <- lang$value_k[lang$language == "Persisch" & lang$gen == "all_gens"]
only_de_k <- lang$value_k[lang$language == "nur Deutsch" & lang$gen == "all_gens"]
mainly_de_k <- lang$value_k[lang$language == "vorwiegend deutsch" & lang$gen == "all_gens"]
nonde_k <- lang$value_k[lang$language == "vorwiegend nicht-deutsch" & lang$gen == "all_gens"]
kurdish_k <- lang$value_k[lang$language == "Kurdisch" & lang$gen == "all_gens"]
persian_pct <- round(persian_k / lang_total * 100)
only_de_pct <- round(only_de_k / lang_total * 100)
mainly_de_pct <- round(mainly_de_k / lang_total * 100)
other_lang_pct <- round((nonde_k - persian_k) / lang_total * 100)
kurdish_pct <- round(kurdish_k / lang_total * 100)
abitur_k <- school$value_k[school$school_level == "Abitur" & school$gen == "all_gens"]
abitur_pct <- round(abitur_k / hl_total * 1000 * 100)
academic_k <- prof$value_k[prof$prof_level == "akademischer Abschluss" & prof$gen == "all_gens"]
academic_pct <- round(academic_k / hl_total * 1000 * 100)

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Germany [%s] ===\n", LANG))

  # --- Per-language source citations + axis affix ----------------------------
  MZ_SOURCE    <- sprintf(tr("de_src_mz"), lnk(MZ_LINK))
  MZ_SIMPLE    <- sprintf(tr("de_src_mz_simple"), lnk(MZ_LINK))
  src_annual   <- tr("de_src_annual")
  src_motive   <- sprintf(tr("de_src_motive"), lnk(MZ_LINK))
  src_duration <- sprintf(tr("de_src_duration"), lnk(MZ_LINK))
  src_industry <- sprintf(tr("de_src_industry"), lnk(MZ_LINK))
  src_income   <- sprintf(tr("de_src_income"), lnk(MZ_LINK))
  pct_suffix   <- if (is_fa()) "٪" else "%"

  # ===========================================================================
  # DE-POPULATION
  # ===========================================================================
  cat("Building de-population...\n")

  bar_df$hover <- ifelse(bar_df$is_suppressed,
    htxt(sprintf(tr("de_hover_suppressed"), as.character(bar_df$land))),
    htxt(sprintf(tr("de_hover_cat_pct"), as.character(bar_df$land),
      fmtv(bar_df$value), fa_num(bar_df$pct, 1))))

  p_bund_bar <- plot_ly(bar_df, x = ~land, y = ~display_value, type = "bar",
      marker = list(color = ~fill_color),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_pop_bar_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickangle = -30, tickfont = list(size = 10)),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 60, b = 100),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE) %>%
    config(displayModeBar = FALSE)

  supp_hover_map <- htxt(sprintf(tr("de_hover_suppressed"), bund_map_suppressed$land))
  vis_hover_map  <- htxt(sprintf(tr("de_hover_map"), bund_map_visible$land,
    fmtv(bund_map_visible$value), fa_num(bund_map_visible$value / hl_total * 100, 1)))

  p_de_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = de_geojson,
      locations = bund_map_suppressed$land,
      z = rep(1, nrow(bund_map_suppressed)),
      featureidkey = "properties.name",
      text = supp_hover_map,
      hoverinfo = "text",
      colorscale = list(c(0, "#d0d0d0"), c(1, "#d0d0d0")),
      zmin = 0, zmax = 1,
      showscale = FALSE,
      marker = list(line = list(color = "white", width = 1), opacity = 0.7)
    ) %>%
    add_trace(type = "choroplethmapbox",
      geojson = de_geojson,
      locations = bund_map_visible$land,
      z = bund_map_visible$value,
      featureidkey = "properties.name",
      text = vis_hover_map,
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.001, "#c6dbef"), c(0.1, "#6baed6"),
        c(0.4, "#2171b5"), c(1, "#08306b")),
      showscale = TRUE,
      colorbar = list(title = "", tickformat = ",", len = 0.3, thickness = 10),
      marker = list(line = list(color = "white", width = 1), opacity = 0.9)
    ) %>% layout(
      mapbox = list(
        style = "carto-positron",
        center = list(lon = 10.5, lat = 51.2),
        zoom = 4.3
      ),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  mig_grid <- paste0(
    '<div style="display:flex; flex-direction:column; gap:12px; width:100%;">',
    '<div style="font-size:14px; font-weight:600; color:#333; text-align:center;">', tr("de_gen_box_title"), '</div>',
    '<div style="display:flex; gap:12px;">',
    make_mig_box(fmtv(hl_fg),
      sprintf(tr("de_gen_pct_of_total"), fa_num(round(hl_fg / hl_total * 100), 0)),
      tr("de_gen1_label"), tr("de_gen1_sub"), "#1a4e72"),
    make_mig_box(fmtv(hl_sg),
      sprintf(tr("de_gen_pct_of_total"), fa_num(round(hl_sg / hl_total * 100), 0)),
      tr("de_gen2_label"), tr("de_gen2_sub"), "#5a9bd5"),
    '</div>',
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0;">%s</p>', MZ_SOURCE),
    '</div>'
  )

  pop_body <- paste0(
    # Top row: headline block + migration grid
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("de_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(hl_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("de_pop_headline_caption"), lnk(MZ_HEADLINE_LINK)), '</div>',
    '<div style="margin:14px auto 0; max-width:460px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("de_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("de_pop_idbox_bullet1"), '</li>',
    '<li>', tr("de_pop_idbox_bullet2"), '</li>',
    '</ul>',
    '</div>',
    '</div>',
    '<div class="chart-card" style="display:flex; align-items:center;">', mig_grid, '</div>',
    '</div>',

    # Bottom row: bar chart + choropleth map
    '<div class="chart-row">',
    '<div class="chart-card">', plotly_div("de-bund-bar", pj(p_bund_bar), "450px",
      source = paste0(MZ_SOURCE, "<br>", tr("de_src_bundbar_note"))), '</div>',
    '<div class="chart-card">',
    '<div class="section-title">', tr("de_pop_map_section"), '</div>',
    plotly_div("de-bund-map", pj(p_de_map), "450px",
      source = paste0(MZ_SOURCE, "<br>", tr("de_src_map_note"))),
    '</div>',
    '</div>'
  )

  fname_pop <- if (is_fa()) "docs/pages/de-population.fa.html" else "docs/pages/de-population.html"
  writeLines(render(tr("de_pop_title"), pop_body), fname_pop)
  cat("  Done\n")


  # ===========================================================================
  # DE-IMMIGRATION & CITIZENSHIP
  # ===========================================================================
  cat("Building de-immigration...\n")

  # --- Motive chart ----------------------------------------------------------
  motive_disp_levels <- if (is_fa()) unname(DE_MOTIVE_FA[levels(motive_clean$short)]) else levels(motive_clean$short)
  motive_clean$short_disp <- factor(
    if (is_fa()) unname(DE_MOTIVE_FA[as.character(motive_clean$short)]) else as.character(motive_clean$short),
    levels = motive_disp_levels)
  motive_clean$hover <- htxt(sprintf(tr("de_hover_cat_pct"),
    as.character(motive_clean$short_disp), fmtv(motive_clean$value), fa_num(motive_clean$pct, 1)))
  p_motive <- plot_ly(motive_clean, x = ~short_disp, y = ~value, type = "bar",
      marker = list(color = motive_colors[as.character(motive_clean$short)]),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_motive_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11)),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 65, b = 110),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE) %>%
    config(displayModeBar = FALSE)

  # --- Residence duration chart ----------------------------------------------
  dur_df$hover <- htxt(sprintf(tr("de_hover_duration"),
    as.character(dur_df$label), fmtv(dur_df$value), fa_num(dur_df$pct, 1)))
  p_duration <- plot_ly(dur_df, x = ~label, y = ~value, type = "bar",
      marker = list(color = "#2774AE"),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_duration_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = tr("de_duration_xaxis"), tickfont = list(size = 11)),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 65, b = 90),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE,
      annotations = list(
        list(text = htxt(tr("de_duration_annot")),
          x = 0.5, y = -0.26, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 9, color = "#6b6b6b"), xanchor = "center"))) %>%
    config(displayModeBar = FALSE)

  # --- Annual arrivals chart -------------------------------------------------
  annual_arrivals$hover <- htxt(sprintf(tr("de_hover_annual"),
    fa_num(annual_arrivals$year, 0, big = FALSE), fmtv(annual_arrivals$iran_arrivals)))
  p_annual <- plot_ly() %>%
    add_bars(
      data = annual_arrivals,
      x = ~year, y = ~iran_arrivals,
      marker = list(color = "#2774AE", line = list(color = "#1a4e72", width = 0.3)),
      text = ~hover,
      hoverinfo = "text", textposition = "none",
      showlegend = FALSE,
      name = "Annual arrivals"
    ) %>%
    layout(
      title = list(text = htxt(tr("de_annual_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", tickfont = list(size = 10), dtick = 4),
      yaxis = list(title = "", tickformat = ",", tickfont = list(size = 10)),
      margin = list(t = 55, b = 40, l = 55, r = 20),
      plot_bgcolor = "white", paper_bgcolor = "white",
      hovermode = "closest"
    ) %>%
    config(displayModeBar = FALSE)

  # --- Citizenship chart -----------------------------------------------------
  cit_df <- data.frame(
    status_en = c("German citizens", "Iranian nationals"),
    count     = c(hl_deu, hl_for),
    pct       = round(c(hl_deu, hl_for) / hl_total * 100, 1),
    stringsAsFactors = FALSE)
  cit_df$status <- if (is_fa()) unname(DE_CITIZEN_FA[cit_df$status_en]) else cit_df$status_en
  cit_df$hover <- htxt(sprintf(tr("de_hover_cat_pct"),
    cit_df$status, fmtv(cit_df$count), fa_num(cit_df$pct, 1)))
  p_citizen <- plot_ly(cit_df, x = ~status, y = ~count, type = "bar",
      marker = list(color = c("#2774AE", "#e07b54")),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_citizen_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickfont = list(size = 12)),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 60, b = 60),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE) %>%
    config(displayModeBar = FALSE)

  # --- Text-card pieces ------------------------------------------------------
  im1_big  <- sprintf(tr("de_bignum"), fa_num(de_cit_pct, 0))
  im1_prim <- htxt(sprintf(tr("de_immig_c1_primary"), fmtv(hl_deu)))
  im1_b1   <- htxt(sprintf(tr("de_immig_c1_b1"), fmtv(hl_for)))
  im1_b2   <- htxt(tr("de_immig_c1_b2"))
  im2_big  <- sprintf(tr("de_bignum"), fa_num(dur_10plus_pct, 0))
  im2_prim <- htxt(tr("de_immig_c2_primary"))
  im2_b1   <- htxt(sprintf(tr("de_immig_c2_b1"), fa_num(dur_30plus_pct, 0)))

  immig_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', im1_big, im1_prim, im1_b1, im1_b2),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
    </ul>
  </div>', im2_big, im2_prim, im2_b1),

    # Left: citizenship chart
    '<div class="chart-card pc1">',
    plotly_div("de-cit", pj(p_citizen), "480px",
      source = MZ_SOURCE),
    '</div>',

    # Right: tabbed annual arrivals + motive + duration
    '<div class="chart-card pc2">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'de-immig-annual\',this,\'de-immig-tabs\')">', tr("de_tab_annual"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'de-immig-motive\',this,\'de-immig-tabs\')">', tr("de_tab_motive"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'de-immig-duration\',this,\'de-immig-tabs\')">', tr("de_tab_duration"), '</button>',
    '</div>',
    '<div id="de-immig-annual" class="tab-panel active" data-group="de-immig-tabs">',
    plotly_div("de-annual", pj(p_annual), "430px",
      source = src_annual),
    '</div>',
    '<div id="de-immig-motive" class="tab-panel" data-group="de-immig-tabs">',
    plotly_div("de-motive", pj(p_motive), "440px",
      source = src_motive),
    '</div>',
    '<div id="de-immig-duration" class="tab-panel" data-group="de-immig-tabs">',
    plotly_div("de-duration", pj(p_duration), "430px",
      source = src_duration),
    '</div>',
    '</div>',
    '</div>'
  )

  fname_immig <- if (is_fa()) "docs/pages/de-immigration.fa.html" else "docs/pages/de-immigration.html"
  writeLines(render(tr("de_immig_title"), immig_body, has_tabs = TRUE), fname_immig)
  cat("  Done\n")


  # ===========================================================================
  # DE-EDUCATION charts (built here, rendered on the de-langedu page)
  # ===========================================================================
  school_disp_levels <- if (is_fa()) unname(DE_SCHOOL_FA[levels(school_all$cat)]) else levels(school_all$cat)
  school_all$cat_disp <- factor(
    if (is_fa()) unname(DE_SCHOOL_FA[as.character(school_all$cat)]) else as.character(school_all$cat),
    levels = school_disp_levels)
  school_all$hover <- htxt(sprintf(tr("de_hover_cat_pct"),
    as.character(school_all$cat_disp), fmtv(school_all$value), fa_num(school_all$pct, 1)))
  p_school <- plot_ly(school_all, x = ~cat_disp, y = ~pct, type = "bar",
      marker = list(color = school_colors[as.character(school_all$cat)]),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_school_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickangle = 0, tickfont = list(size = 10)),
      yaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 60)),
      margin = list(t = 60, b = 70),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE) %>%
    config(displayModeBar = FALSE)

  prof_disp_levels <- if (is_fa()) unname(DE_PROF_FA[levels(prof_all$cat)]) else levels(prof_all$cat)
  prof_all$cat_disp <- factor(
    if (is_fa()) unname(DE_PROF_FA[as.character(prof_all$cat)]) else as.character(prof_all$cat),
    levels = prof_disp_levels)
  prof_all$hover <- htxt(sprintf(tr("de_hover_cat_pct"),
    as.character(prof_all$cat_disp), fmtv(prof_all$value), fa_num(prof_all$pct, 1)))
  p_prof <- plot_ly(prof_all, x = ~cat_disp, y = ~pct, type = "bar",
      marker = list(color = prof_colors[as.character(prof_all$cat)]),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_prof_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickfont = list(size = 11)),
      yaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 40)),
      margin = list(t = 60, b = 70),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE) %>%
    config(displayModeBar = FALSE)


  # ===========================================================================
  # DE-WORK charts (built here, rendered on the de-workinc page)
  # ===========================================================================
  emp_disp_levels <- if (is_fa()) unname(DE_STATUS_FA[levels(emp_bar_df$status)]) else levels(emp_bar_df$status)
  emp_bar_df$status_disp <- factor(
    if (is_fa()) unname(DE_STATUS_FA[as.character(emp_bar_df$status)]) else as.character(emp_bar_df$status),
    levels = emp_disp_levels)
  emp_bar_df$hover <- htxt(sprintf(tr("de_hover_cat_pct"),
    as.character(emp_bar_df$status_disp), fmtv(emp_bar_df$count), fa_num(emp_bar_df$pct, 1)))
  p_emp_status <- plot_ly(emp_bar_df, x = ~status_disp, y = ~count, type = "bar",
      marker = list(color = c("#c4793a", "#b05050", "#b0b0b0")),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_empstatus_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickfont = list(size = 11)),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 55, b = 70),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE) %>%
    config(displayModeBar = FALSE)

  # Industry: label-above bars (RTL-mirrored on fa via hbar_over_labels()).
  de_ind_text <- de_ind_base_text
  if (is_fa()) de_ind_text <- ifelse(de_ind_text == "", "",
    gsub("%", "٪", fa_digits(de_ind_text)))   # N% -> ۲۴٪ (axis-style suffix)
  ind_cats_disp <- if (is_fa()) unname(DE_INDUSTRY_FA[de_ind_levels_en]) else de_ind_levels_en
  ov_de_ind <- hbar_over_labels(ind_cats_disp, ends = de_ind_ends, end_text = de_ind_text)
  de_ind_xrange <- if (isTRUE(ov_de_ind$xreversed)) c(de_ind_xmax, 0) else c(0, de_ind_xmax)
  row_ind_disp <- if (is_fa()) unname(DE_INDUSTRY_FA[as.character(ind$label)]) else as.character(ind$label)
  ind$hover <- ifelse(ind$is_suppressed,
    htxt(sprintf(tr("de_hover_suppressed"), row_ind_disp)),
    htxt(sprintf(tr("de_hover_industry"), row_ind_disp, fmtv(ind$value), fa_num(ind$pct, 1))))
  p_industry <- plot_ly(ind, y = ~label, x = ~display_value, type = "bar",
      orientation = "h",
      marker = list(color = ~fill_color),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_industry_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE, range = de_ind_xrange),
      yaxis = ov_de_ind$yaxis,
      annotations = ov_de_ind$annotations, bargap = ov_de_ind$bargap,
      margin = list(t = ov_de_ind$margin_t, b = 40, l = ov_de_ind$margin_l, r = 12),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE) %>%
    config(displayModeBar = FALSE)


  # ===========================================================================
  # DE-INCOME chart (built here, rendered on the de-workinc page)
  # ===========================================================================
  inc_disp_levels <- if (is_fa()) unname(DE_INCBRACKET_FA[inc_order]) else inc_order
  inc$bracket_disp <- factor(
    if (is_fa()) unname(DE_INCBRACKET_FA[as.character(inc$bracket)]) else as.character(inc$bracket),
    levels = inc_disp_levels)
  inc$hover <- ifelse(inc$is_suppressed,
    htxt(sprintf(tr("de_hover_income_suppressed"), as.character(inc$bracket_disp))),
    htxt(sprintf(tr("de_hover_income"), as.character(inc$bracket_disp),
      fmtv(inc$value), fa_num(inc$pct, 1))))
  p_income <- plot_ly(inc, x = ~bracket_disp, y = ~display_pct, type = "bar",
      marker = list(color = inc$bar_color),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("de_income_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickangle = -25, tickfont = list(size = 10)),
      yaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 25)),
      margin = list(t = 65, b = 110),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = FALSE) %>%
    config(displayModeBar = FALSE)


  # ===========================================================================
  # DE-WORK & INCOME (combined page)
  # ===========================================================================
  cat("Building de-workinc...\n")

  wi1_big  <- sprintf(tr("de_bignum"), fa_num(services_pct, 0))
  wi1_prim <- htxt(tr("de_wi_c1_primary"))
  wi1_b1   <- htxt(sprintf(tr("de_wi_c1_b1"), fa_num(emp_pct, 0)))
  wi1_b2   <- htxt(sprintf(tr("de_wi_c1_b2"), fa_num(unemp_pct, 0)))
  wi1_b3   <- htxt(sprintf(tr("de_wi_c1_b3"), fa_num(nilf_pct, 0)))
  wi2_big  <- sprintf(tr("de_bignum"), fa_num(middle_share, 0))
  wi2_prim <- htxt(tr("de_wi_c2_primary"))
  wi2_b1   <- htxt(sprintf(tr("de_wi_c2_b1"), fa_num(top_share, 0)))
  wi2_b2   <- htxt(sprintf(tr("de_wi_c2_b2"), fa_num(low_share, 0)))

  workinc_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>',
      wi1_big, wi1_prim),
    sprintf('    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
      wi1_b1, wi1_b2, wi1_b3),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', wi2_big, wi2_prim, wi2_b1, wi2_b2),
    # LEFT: work with 2 tabs (labour force status + industry)
    '<div class="chart-card pc1">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'de-wk-status\',this,\'de-wk-tabs\')">', tr("de_tab_status"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'de-wk-industry\',this,\'de-wk-tabs\')">', tr("de_tab_industry"), '</button>',
    '</div>',
    '<div id="de-wk-status" class="tab-panel active" data-group="de-wk-tabs">',
    plotly_div("de-empstatus", pj(p_emp_status), "460px",
      source = MZ_SIMPLE),
    '</div>',
    '<div id="de-wk-industry" class="tab-panel" data-group="de-wk-tabs">',
    plotly_div("de-industry", pj(p_industry), ov_de_ind$height,
      source = src_industry),
    '</div>',
    '</div>',
    # RIGHT: income chart
    '<div class="chart-card pc2">',
    plotly_div("de-income", pj(p_income), "510px",
      source = src_income),
    '</div>',
    '</div>'
  )

  fname_workinc <- if (is_fa()) "docs/pages/de-workinc.fa.html" else "docs/pages/de-workinc.html"
  writeLines(render(tr("de_workinc_title"), workinc_body, has_tabs = TRUE), fname_workinc)
  cat("  Done\n")


  # ===========================================================================
  # DE-LANGUAGE chart (built here, rendered on the de-langedu page)
  # ===========================================================================
  cat_disp <- function(c) if (is_fa()) unname(DE_LANGCAT_FA[c]) else c
  lang_all$gen_disp <- factor(
    if (is_fa()) unname(DE_GENLABEL_FA[as.character(lang_all$gen_label)]) else as.character(lang_all$gen_label),
    levels = if (is_fa()) unname(DE_GENLABEL_FA[levels(lang_all$gen_label)]) else levels(lang_all$gen_label))

  p_lang <- plot_ly()
  for (cat_name in lang_cat_order) {
    disp <- cat_disp(cat_name)
    sub <- lang_all %>% filter(category == cat_name)
    p_lang <- p_lang %>% add_bars(
      data = sub, y = ~gen_disp, x = ~pct, name = disp,
      marker = list(color = lang_colors[cat_name]), textposition = "none",
      hovertext = htxt(sprintf(tr("de_hover_lang"),
        disp, as.character(sub$gen_disp), fmtv(sub$value), fa_num(sub$pct, 1))),
      hoverinfo = "text",
      legendgroup = disp, showlegend = FALSE, orientation = "h")
  }
  p_lang <- p_lang %>% layout(
    barmode = "stack",
    title = list(text = htxt(tr("de_lang_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 105)),
    yaxis = list(title = "",
      categoryorder = "array", categoryarray = levels(lang_all$gen_disp),
      ticklabelstandoff = 6),
    margin = list(t = 55, b = 40, l = 140), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
    config(displayModeBar = FALSE)

  lang_colors_disp <- setNames(unname(lang_colors[lang_cat_order]),
                               vapply(lang_cat_order, cat_disp, character(1)))
  lang_leg <- make_html_legend_hover(lang_colors_disp, break_after = 2)


  # ===========================================================================
  # DE-LANGUAGE & EDUCATION (combined page)
  # ===========================================================================
  cat("Building de-langedu...\n")

  le1_big  <- sprintf(tr("de_bignum"), fa_num(persian_pct, 0))
  le1_prim <- htxt(sprintf(tr("de_le_c1_primary"), fmtv(persian_k * 1000)))
  le1_b1   <- htxt(sprintf(tr("de_le_c1_b1"), fa_num(only_de_pct, 0)))
  le1_b2   <- htxt(sprintf(tr("de_le_c1_b2"), fa_num(mainly_de_pct, 0)))
  le1_b3   <- htxt(sprintf(tr("de_le_c1_b3"), fa_num(other_lang_pct, 0)))
  le1_b4   <- htxt(sprintf(tr("de_le_c1_b4"), fa_num(kurdish_pct, 0)))
  le2_big  <- sprintf(tr("de_bignum"), fa_num(abitur_pct, 0))
  le2_prim <- htxt(tr("de_le_c2_primary"))
  le2_b1   <- htxt(tr("de_le_c2_b1"))
  le2_b2   <- htxt(sprintf(tr("de_le_c2_b2"), fa_num(academic_pct, 0), fmtv(academic_k * 1000)))

  langedu_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', le1_big, le1_prim, le1_b1, le1_b2, le1_b3, le1_b4),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', le2_big, le2_prim, le2_b1, le2_b2),
    # LEFT: language chart (standalone, no persian sidebar)
    '<div class="chart-card pc1" style="display:flex; flex-direction:column; justify-content:center;">',
    plotly_div("de-lang", pj(p_lang), "320px",
      source = MZ_SIMPLE,
      legend_html = lang_leg, highlight_hover = TRUE),
    '</div>',
    # RIGHT: education with 2 tabs (school qualification + vocational/academic)
    '<div class="chart-card pc2">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'de-ed-school\',this,\'de-ed-tabs\')">', tr("de_tab_school"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'de-ed-prof\',this,\'de-ed-tabs\')">', tr("de_tab_prof"), '</button>',
    '</div>',
    '<div id="de-ed-school" class="tab-panel active" data-group="de-ed-tabs">',
    plotly_div("de-school", pj(p_school), "450px",
      source = MZ_SIMPLE),
    '</div>',
    '<div id="de-ed-prof" class="tab-panel" data-group="de-ed-tabs">',
    plotly_div("de-prof", pj(p_prof), "450px",
      source = MZ_SIMPLE),
    '</div>',
    '</div>',
    '</div>'
  )

  fname_langedu <- if (is_fa()) "docs/pages/de-langedu.fa.html" else "docs/pages/de-langedu.html"
  writeLines(render(tr("de_langedu_title"), langedu_body, has_tabs = TRUE), fname_langedu)
  cat("  Done\n")
}

cat("\nAll Germany pages built (en + fa).\n")

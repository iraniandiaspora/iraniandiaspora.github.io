# Build all 6 Canada pages from pre-computed CSV outputs
# Run from the iraniandiaspora.github.io/ directory:
#   Rscript R/build_canada.R
#
# Output: docs/pages/ca-population.html   + ca-population.fa.html
#         docs/pages/ca-langrelig.html    + ca-langrelig.fa.html
#         docs/pages/ca-immigration.html  + ca-immigration.fa.html
#         docs/pages/ca-education.html     + ca-education.fa.html
#         docs/pages/ca-work.html          + ca-work.fa.html
#         docs/pages/ca-income.html        + ca-income.fa.html
#
# Bilingual (en + fa) following the build_germany.R pattern. Canada keeps its
# LOCAL page_template() (2x4 grid classes: page-content-4 / p4-*) and its LOCAL
# tab_switch_script. All user-facing strings come from R/i18n/strings_canada.R
# via tr(); numbers go through fa_num()/fmtv()/bnum_year() so the six English
# editions stay BYTE-IDENTICAL while the Persian editions render RTL with Persian
# digits and the Vazirmatn face. The en path calls the local page_template()
# unchanged; the fa path runs that same shell through fa_shell().

library(plotly)
library(dplyr)
library(jsonlite)

DATA_DIR <- "data/canada"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, criteria_table(),
# map_overlay_legend(), chk(), make_html_legend_hover().
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# Canada string table (defines the global STR consumed by tr(), plus the fa-only
# category display vectors CA_REGION_FA / CA_LANGCAT_FA / ...).
source("R/i18n/strings_canada.R")

# --- Latin source links / anchors (language-independent) ----------------------
PUMF_LINK <- "<a href='https://dc1.chass.utoronto.ca/census/index.html' target='_blank' style='color:#2774AE;'>Census PUMF</a>"
CA_CENSUS_LINK <- '<a href="https://www.statcan.gc.ca/census-recensement/2021/ref/questionnaire/index-eng.cfm" style="color:#2774AE;" target="_blank">2021 Canadian Census</a>'
ONT_LINK <- "<a href='https://www.statcan.gc.ca/census-recensement/2021/dp-pd/index-eng.cfm' target='_blank' style='color:#2774AE;'>Statistics Canada</a>"

# Tab-switching JS (injected once per page that uses tabs)
tab_switch_script <- '
<script>
function switchTab(tabId, btn, groupId) {
  var panels = document.querySelectorAll(".tab-panel[data-group=\'" + groupId + "\']");
  panels.forEach(function(p) { p.classList.remove("active"); });
  document.getElementById(tabId).classList.add("active");
  var btns = btn.parentElement.querySelectorAll(".tab-btn");
  btns.forEach(function(b) { b.classList.remove("active"); });
  btn.classList.add("active");
  // Resize any plotly charts in newly-visible tab and re-bindhover events
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
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }
.page-content-4 { display:grid; grid-template-columns:repeat(4,1fr); gap:16px; margin-bottom:20px; }
.page-content-4 .chart-card { margin-bottom:0; }
.p4-t1 { grid-area:1/1; } .p4-t2 { grid-area:1/2; } .p4-t3 { grid-area:1/3; } .p4-t4 { grid-area:1/4; }
.p4-c1 { grid-area:2/1/2/3; } .p4-c2 { grid-area:2/3/2/5; }', tab_css, '
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .text-row-4 { grid-template-columns:1fr 1fr; }
  .text-row, .text-row-4 { order:1; } /* push text below charts on mobile */
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .page-content-4 { grid-template-columns:1fr; }
  .p4-t1,.p4-t2,.p4-t3,.p4-t4,.p4-c1,.p4-c2 { grid-area:auto; }
  .p4-c1 { order:1; } .p4-t1 { order:2; } .p4-t2 { order:3; }
  .p4-c2 { order:4; } .p4-t3 { order:5; } .p4-t4 { order:6; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
}
@media (max-width:600px) {
  .text-row-4 { grid-template-columns:1fr !important; }
}
@media (max-width:600px) {
  .id-table { border-spacing:4px !important; }
  .id-cell { padding:8px 4px !important; min-height:60px !important; }
  .id-num { font-size:16px !important; }
  .id-label-col { width:55px !important; font-size:10px !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .chart-card { padding:10px; }
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
# fa is that same shell run through fa_shell().
render <- function(title, body_html, has_tabs = FALSE) {
  html <- page_template(title, body_html, has_tabs)
  if (is_fa()) html <- fa_shell(html)
  html
}

# --- i18n formatting helpers (build_germany.R pattern) ------------------------
# lnk():   in fa, isolate a Latin agency link in <bdi>; in en, pass through.
lnk <- function(x) if (is_fa()) bdi(x) else x
# fmtv():  vector-safe big-integer formatter. en = format(x, big.mark = ","),
#          reproducing the committed hover text (incl. common-width padding). fa
#          Persian-digits that string (ASCII "," thousands kept, per press).
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)
}
# htxt():  Persian-digit any stray Western digits in an assembled display string
#          (hover text, chart titles, card prose). No-op in en. Never apply to
#          HTML that carries CSS — only to plain human text.
htxt <- function(s) if (is_fa()) fa_digits(s) else s
# bnum_year(): a bare year with NO thousands separator (so 1998 stays "1998").
bnum_year <- function(x) fa_num(x, 0, big = FALSE)
# bpct(): integer percent for a card big-number. en "62%"; fa "62 درصد".
bpct <- function(x) if (is_fa()) paste0(fa_num(x), " درصد") else sprintf("%d%%", x)
# gshort(): translate the short generation label used in butterfly hovers.
gshort <- function(g) {
  if (!is_fa()) return(g)
  if (identical(g, "1st Gen")) tr("ca_gen1_short")
  else if (identical(g, "2nd Gen")) tr("ca_gen2_short")
  else g
}

# Colors
blues <- c("#1a4e72", "#2774AE", "#5a9bd5", "#8bbdde", "#d4e6f1")

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LANGUAGE-INDEPENDENT DATA PREP (read + wrangle ONCE; English-keyed factors,
# colors, numeric shares). Text / hovers / display factors are built per
# language inside the build loop.
# =============================================================================

# --- CA-POPULATION -----------------------------------------------------------
pop <- read.csv(file.path(DATA_DIR, "population/iranian_population_final_breakdown_complete.csv"))
pop_included <- pop %>% filter(Included_in_Total == "Yes")

wf <- pop_included
wf$cumulative <- cumsum(wf$Population)
wf$ymin <- wf$cumulative - wf$Population
wf$xnum <- seq_len(nrow(wf))
wf$is_born    <- grepl("^Born in Iran", wf$Category)
wf$is_ethnic  <- grepl("Iranian Ethnic", wf$Category) & !grepl("No Iranian Ethnic", wf$Category)
wf$is_persian <- grepl("Persian", wf$Category) & !grepl("No Persian", wf$Category)
wf_colors <- c("#1a4e72", "#2774AE", "#5a9bd5", "#4a8c6f", "#c4793a", "#d4a943", "#7b5ea7")
CA_LBL_PX <- 78

# Region bar — shares the denominator used by the province map below.
prov_for_regions <- read.csv(file.path(DATA_DIR, "population/iranians_by_province.csv"))
prov_totals <- prov_for_regions %>%
  mutate(region = case_when(
    province %in% c("Ontario", "Quebec") ~ "Central",
    province %in% c("British Columbia", "Alberta", "Manitoba", "Saskatchewan") ~ "Western",
    province %in% c("New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                    "Prince Edward Island") ~ "Atlantic",
    province %in% c("Yukon", "Northwest Territories", "Nunavut") ~ "Northern",
    TRUE ~ "Other"
  ))
region_totals <- prov_totals %>%
  group_by(region) %>%
  summarize(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pop))
region_grand <- sum(region_totals$pop, na.rm = TRUE)
region_totals$pct <- round(region_totals$pop / region_grand * 100, 1)
region_order <- c("Central", "Western", "Atlantic", "Northern")
if (!"Northern" %in% region_totals$region) {
  region_totals <- bind_rows(region_totals, data.frame(region = "Northern", pop = 0, pct = 0))
}
region_totals <- region_totals %>% filter(region %in% region_order)
region_totals$region <- factor(region_totals$region, levels = region_order)
region_colors <- c("#7b5ea7", "#d4a943", "#2ca089", "#e07b54")

# Provincial choropleth
prov_data <- read.csv(file.path(DATA_DIR, "population/iranians_by_province.csv"))
prov_iso <- c("Ontario" = "CA-ON", "British Columbia" = "CA-BC", "Quebec" = "CA-QC",
  "Alberta" = "CA-AB", "Manitoba" = "CA-MB", "Saskatchewan" = "CA-SK",
  "Nova Scotia" = "CA-NS", "New Brunswick" = "CA-NB",
  "Newfoundland and Labrador" = "CA-NL", "Prince Edward Island" = "CA-PE",
  "Yukon" = "CA-YT", "Northwest Territories" = "CA-NT", "Nunavut" = "CA-NU")
prov_data$iso <- prov_iso[prov_data$province]
prov_data <- prov_data %>% filter(!is.na(iso))
prov_total <- sum(prov_data$pop, na.rm = TRUE)
prov_geojson_url <- "https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/canada.geojson"

# Ontario municipality choropleth
library(sf)
sf_use_s2(FALSE)
ont_csds <- readRDS(file.path(DATA_DIR, "population/ontario_csds_iranian.rds"))
ont_csds <- ont_csds %>% filter(pop > 0) %>% arrange(desc(pop))
ont_geojson_path <- file.path(DATA_DIR, "population/ontario_csds.geojson")
if (!file.exists(ont_geojson_path)) {
  st_write(ont_csds %>% select(GeoUID, `Region Name`, pop, geometry),
    ont_geojson_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
}
ont_data <- ont_csds %>% st_drop_geometry()
ont_geojson <- jsonlite::fromJSON(ont_geojson_path, simplifyVector = FALSE)
ca_pop_headline_number <- format(round(sum(pop_included$Population)), big.mark = ",")

# --- CA-LANGUAGE & RELIGION --------------------------------------------------
lang <- read.csv(file.path(DATA_DIR, "language/persian_language_patterns_FINAL.csv"))
relig <- read.csv(file.path(DATA_DIR, "religion/religion_dashboard_ready.csv"))

lang <- lang %>% mutate(short_cat = case_when(
  persian_status == "Persian mother tongue, Persian home" ~ "Persian (mother tongue), Persian (at home)",
  persian_status == "Persian mother tongue, English/French home" ~ "Persian (mother tongue), Eng/Fr (at home)",
  persian_status == "Iranian minority mother tongue, same home" ~ "Minority (mother tongue), same (at home)",
  persian_status == "Iranian minority mother tongue, English/French home" ~ "Minority (mother tongue), Eng/Fr (at home)",
  persian_status == "Iranian minority mother tongue, various home" ~ "Minority (mother tongue), Eng/Fr (at home)",
  persian_status == "English mother tongue, English home" ~ "English (mother tongue), English (at home)",
  persian_status == "Other languages" ~ "Other languages",
  TRUE ~ persian_status
))
lang_cats <- c("Persian (mother tongue), Persian (at home)", "Persian (mother tongue), Eng/Fr (at home)",
               "Minority (mother tongue), same (at home)", "Minority (mother tongue), Eng/Fr (at home)",
               "English (mother tongue), English (at home)", "Other languages")
lang_colors <- c("Persian (mother tongue), Persian (at home)" = "#2d6a4f",
                 "Persian (mother tongue), Eng/Fr (at home)" = "#74a892",
                 "Minority (mother tongue), same (at home)" = "#8b6c42",
                 "Minority (mother tongue), Eng/Fr (at home)" = "#c4a96a",
                 "English (mother tongue), English (at home)" = "#6c757d",
                 "Other languages" = "#b0b0b0")
lang_gen_levels <- c("1st Generation", "2nd+ Generation")
lang1 <- lang %>% filter(generation == "First generation") %>%
  mutate(gen_label = "1st Generation")
lang2 <- lang %>% filter(generation == "Second+ generation") %>%
  mutate(gen_label = "2nd+ Generation")
lang2 <- lang2 %>% mutate(short_cat = case_when(
  grepl("^Iranian minority moth", persian_status) ~ "Minority (mother tongue), Eng/Fr (at home)",
  TRUE ~ short_cat
))
lang_all <- bind_rows(lang1, lang2) %>%
  filter(short_cat %in% lang_cats) %>%
  group_by(gen_label, short_cat) %>%
  summarize(percentage = sum(percentage, na.rm = TRUE), .groups = "drop")
lang_all <- lang_all %>%
  group_by(gen_label) %>%
  mutate(percentage = round(percentage / sum(percentage) * 100, 1)) %>%
  ungroup()
lang_all$gen_label <- factor(lang_all$gen_label, levels = lang_gen_levels)
lang_all$short_cat <- factor(lang_all$short_cat, levels = lang_cats)

relig_cats <- c("Muslim", "No religion/Secular", "Christian", "Other religions", "Not stated")
relig_colors <- c("Muslim" = "#1a4e72",
                  "No religion/Secular" = "#d4a943",
                  "Christian" = "#4a8c6f",
                  "Other religions" = "#7b5ea7",
                  "Not stated" = "#b0b0b0")
relig <- relig %>% mutate(short_cat = case_when(
  grepl("^Muslim", religion_category) ~ "Muslim",
  grepl("^No religion", religion_category) ~ "No religion/Secular",
  grepl("^Christian", religion_category) ~ "Christian",
  grepl("^Other", religion_category) ~ "Other religions",
  grepl("^All other", religion_category) ~ "Not stated",
  TRUE ~ religion_category
))
relig1 <- relig %>% filter(generation == "First generation") %>% mutate(gen_label = "1st Generation")
relig2 <- relig %>% filter(generation == "Second+ generation") %>% mutate(gen_label = "2nd+ Generation")
relig_all <- bind_rows(relig1, relig2) %>%
  filter(short_cat %in% relig_cats) %>%
  group_by(gen_label, short_cat) %>%
  summarize(percentage = sum(percentage, na.rm = TRUE), .groups = "drop")
relig_all <- relig_all %>%
  group_by(gen_label) %>%
  mutate(percentage = round(percentage / sum(percentage) * 100, 1)) %>%
  ungroup()
relig_all$gen_label <- factor(relig_all$gen_label, levels = lang_gen_levels)
relig_all$short_cat <- factor(relig_all$short_cat, levels = relig_cats)

# card4() helper — shared by ca-langrelig and ca-work.
card4 <- function(cls, big, sentence, note) {
  sprintf('<div class="text-card %s" style="text-align:center;">
    <div style="font-size:32px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:14px; font-weight:500; color:#333; margin-top:10px; line-height:1.45;">%s</div>
    <div style="font-size:12.5px; color:#555; margin-top:10px; line-height:1.5;">%s</div>
  </div>', cls, big, sentence, note)
}

lang_share <- function(gen, cat) {
  v <- lang_all$percentage[lang_all$gen_label == gen & lang_all$short_cat == cat]
  if (length(v) == 0) 0 else round(sum(v))
}
relig_share <- function(gen, cat) {
  v <- relig_all$percentage[relig_all$gen_label == gen & relig_all$short_cat == cat]
  if (length(v) == 0) 0 else round(sum(v))
}
g1_persian   <- lang_share("1st Generation",  "Persian (mother tongue), Persian (at home)")
g1_engfr     <- lang_share("1st Generation",  "Persian (mother tongue), Eng/Fr (at home)") +
                lang_share("1st Generation",  "English (mother tongue), English (at home)")
g2_persian   <- lang_share("2nd+ Generation", "Persian (mother tongue), Persian (at home)")
g2_engfr     <- lang_share("2nd+ Generation", "Persian (mother tongue), Eng/Fr (at home)") +
                lang_share("2nd+ Generation", "English (mother tongue), English (at home)")
g1_muslim     <- relig_share("1st Generation",  "Muslim")
g1_noreligion <- relig_share("1st Generation",  "No religion/Secular")
g2_muslim     <- relig_share("2nd+ Generation", "Muslim")
g2_noreligion <- relig_share("2nd+ Generation", "No religion/Secular")
g2_christian  <- relig_share("2nd+ Generation", "Christian")

# --- CA-IMMIGRATION & CITIZENSHIP --------------------------------------------
immig_annual <- read.csv(file.path(DATA_DIR, "immigration/immigration_annual.csv"))
cit <- read.csv(file.path(DATA_DIR, "citizenship/citizenship_status.csv"))
immig_cat <- read.csv(file.path(DATA_DIR, "immigration/iranian_immigration_category_trends.csv"),
  check.names = FALSE)

immig_annual <- immig_annual %>% arrange(year)
immig_annual$true_arrivals <- ifelse(immig_annual$is_period,
  immig_annual$period_total, immig_annual$count)
immig_annual$cumulative <- cumsum(immig_annual$true_arrivals)
immig_annual$cum_pct <- round(immig_annual$cumulative / max(immig_annual$cumulative) * 100, 1)
immig_annual$bar_width <- ifelse(immig_annual$is_period, 4.5, 0.8)
immig_annual$bar_color <- ifelse(immig_annual$is_period, "#5a9bd5", "#2774AE")

cit1 <- cit %>%
  group_by(status) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentage = round(count / sum(count) * 100, 1))
cit1$short_label <- case_when(
  grepl("by birth", cit1$status) ~ "Born in\nCanada",
  grepl("naturalization", cit1$status) ~ "Naturalized\ncitizen",
  grepl("Not a", cit1$status) ~ "Not a\ncitizen",
  TRUE ~ cit1$status
)
cit1_order <- c("Naturalized\ncitizen", "Born in\nCanada", "Not a\ncitizen")
cit1$short_label <- factor(cit1$short_label, levels = cit1_order)
cit_colors <- c("#2774AE", "#5a9bd5", "#e07b54")

immig_cat_long <- immig_cat %>%
  select(arrival_period, starts_with("percentage_")) %>%
  tidyr::pivot_longer(cols = -arrival_period, names_to = "category", values_to = "pct") %>%
  mutate(category = gsub("^percentage_", "", category))
immtype_cats <- c("Economic immigrants", "Family-sponsored immigrants", "Refugees")
immtype_colors <- c("Economic immigrants" = "#1a4e72",
                    "Family-sponsored immigrants" = "#4a8c6f",
                    "Refugees" = "#d4a943")
period_order_cat <- c("1990-1999", "2000-2009", "2010-2019", "2020-2021")
immig_cat_long$arrival_period <- factor(immig_cat_long$arrival_period, levels = period_order_cat)
immig_cat_long$category <- factor(immig_cat_long$category, levels = immtype_cats)

im_ordered <- immig_annual[order(immig_annual$year), ]
im_total <- sum(im_ordered$period_total, na.rm = TRUE)
im_ordered$cum <- cumsum(im_ordered$period_total) / im_total
im_median_year <- im_ordered$year[which(im_ordered$cum >= 0.5)[1]]
im_1980s_share <- round(
  sum(immig_annual$period_total[immig_annual$year >= 1980 & immig_annual$year < 1990], na.rm = TRUE) /
  im_total * 100)
cit_naturalized_pct <- round(cit$percentage[grepl("naturalization", cit$status)][1])
cit_not_citizen_pct <- round(cit$percentage[grepl("Not a", cit$status)][1])

# --- CA-EDUCATION ------------------------------------------------------------
ed1 <- read.csv(file.path(DATA_DIR, "education/education_first_gen_30plus_FINAL.csv"))
fos <- read.csv(file.path(DATA_DIR, "education/field_of_study_DASHBOARD_FINAL.csv"))
fos$field_category[fos$field_category == "STEM"] <- "Science, Technology, Engineering & Math"

# Education butterfly (bilingual): keeps English keys for filtering + colors,
# translates category / gen / gender labels for display.
make_ca_educ_butterfly <- function(df, gen_label, id_prefix, height = "450px", source = NULL) {
  educ_levels <- c("Less than BA degree", "BA degree", "Graduate degree")
  colors <- c("Less than BA degree" = "#1a4e72", "BA degree" = "#2774AE", "Graduate degree" = "#8bbdde")
  ed_disp <- function(e) if (is_fa()) unname(CA_EDUC_FA[e]) else e
  gl <- gshort(gen_label)

  d_m <- df %>% filter(gender_label == "Men") %>%
    mutate(education_level = factor(education_level, levels = educ_levels))
  d_f <- df %>% filter(gender_label == "Women") %>%
    mutate(education_level = factor(education_level, levels = educ_levels))
  age_levels <- sort(unique(df$age_cohort))

  p_men <- plot_ly()
  for (ed in educ_levels) {
    sm <- d_m %>% filter(education_level == ed)
    if (nrow(sm) > 0) {
      hover_texts <- htxt(sprintf(tr("ca_hover_bfly_age"), ed_disp(ed), gl, tr("ca_men"), sm$age_cohort, fa_num(sm$percentage, 1)))
      p_men <- p_men %>% add_bars(data = sm, y = ~age_cohort, x = ~percentage, name = ed_disp(ed),
        marker = list(color = colors[ed]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = ed_disp(ed), showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = !is_fa(), categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (ed in educ_levels) {
    sf <- d_f %>% filter(education_level == ed)
    if (nrow(sf) > 0) {
      hover_texts <- htxt(sprintf(tr("ca_hover_bfly_age"), ed_disp(ed), gl, tr("ca_women"), sf$age_cohort, fa_num(sf$percentage, 1)))
      p_women <- p_women %>% add_bars(data = sf, y = ~age_cohort, x = ~percentage, name = ed_disp(ed),
        marker = list(color = colors[ed]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = ed_disp(ed), showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = pct_suffix),
    yaxis = c(list(title = "", showticklabels = is_fa(), categoryorder = "array", categoryarray = age_levels), if (is_fa()) list(side = "right", automargin = TRUE)))

  p <- subplot(p_men, p_women, shareY = !is_fa(), titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = tr("ca_men"), x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = tr("ca_women"), x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = if (is_fa()) 20 else 60, r = if (is_fa()) 60 else 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  colors_disp <- setNames(unname(colors), vapply(names(colors), ed_disp, character(1)))
  leg <- make_html_legend_hover(colors_disp, break_after = 3)
  plotly_div(id_prefix, pj(p), height, source = source, legend_html = leg, highlight_hover = TRUE)
}

make_fos_butterfly <- function(fos_data, gen_label, id_prefix, height = "350px", source = NULL) {
  fos1 <- fos_data %>% filter(generation == "First-Generation")
  field_cats <- c("Science, Technology, Engineering & Math", "Business & Management", "Health", "Social Sciences & Humanities", "Other")
  colors <- c("Science, Technology, Engineering & Math" = "#1a4e72", "Business & Management" = "#d4a943",
              "Health" = "#4a8c6f", "Social Sciences & Humanities" = "#c4793a",
              "Other" = "#b0b0b0")
  fc_disp <- function(fc) if (is_fa()) unname(CA_FOS_FA[fc]) else fc
  gl <- gshort(gen_label)
  y_lab <- tr("ca_ages_25plus")

  d_m <- fos1 %>% filter(gender_label == "Men")
  d_f <- fos1 %>% filter(gender_label == "Women")
  y_levels <- c(y_lab)

  p_men <- plot_ly()
  for (fc in field_cats) {
    sm <- d_m %>% filter(field_category == fc)
    if (nrow(sm) > 0) {
      sm$y_label <- y_lab
      hover_texts <- htxt(sprintf(tr("ca_hover_bfly_noage"), fc_disp(fc), gl, tr("ca_men"), fa_num(sm$percentage, 1)))
      p_men <- p_men %>% add_bars(data = sm, y = ~y_label, x = ~percentage, name = fc_disp(fc),
        marker = list(color = colors[fc]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = fc_disp(fc), showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = !is_fa(), categoryorder = "array", categoryarray = y_levels))

  p_women <- plot_ly()
  for (fc in field_cats) {
    sf <- d_f %>% filter(field_category == fc)
    if (nrow(sf) > 0) {
      sf$y_label <- y_lab
      hover_texts <- htxt(sprintf(tr("ca_hover_bfly_noage"), fc_disp(fc), gl, tr("ca_women"), fa_num(sf$percentage, 1)))
      p_women <- p_women %>% add_bars(data = sf, y = ~y_label, x = ~percentage, name = fc_disp(fc),
        marker = list(color = colors[fc]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = fc_disp(fc), showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = pct_suffix),
    yaxis = c(list(title = "", showticklabels = is_fa(), categoryorder = "array", categoryarray = y_levels), if (is_fa()) list(side = "right", automargin = TRUE)))

  p <- subplot(p_men, p_women, shareY = !is_fa(), titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = tr("ca_men"), x = 0.22, y = 1.05, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = tr("ca_women"), x = 0.78, y = 1.05, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = if (is_fa()) 20 else 60, r = if (is_fa()) 60 else 20, t = 40, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  colors_disp <- setNames(unname(colors), vapply(names(colors), fc_disp, character(1)))
  leg <- make_html_legend_hover(colors_disp, break_after = 2)
  plotly_div(id_prefix, pj(p), height, source = source, legend_html = leg, highlight_hover = TRUE)
}

grad_pct <- function(cohort, gender) {
  v <- ed1$percentage[ed1$age_cohort == cohort & ed1$gender_label == gender & ed1$education_level == "Graduate degree"]
  if (length(v) == 0) NA_real_ else round(v[1])
}
ed_w_30 <- grad_pct("30-34", "Women")
ed_m_30 <- grad_pct("30-34", "Men")
ed_m_55 <- grad_pct("55-64", "Men")
ed_w_55 <- grad_pct("55-64", "Women")
fos_pct <- function(gender, field) {
  v <- fos$percentage[fos$gender_label == gender & fos$field_category == field & fos$generation == "First-Generation"]
  if (length(v) == 0) NA_real_ else round(v[1])
}
fos_m_stem <- fos_pct("Men", "Science, Technology, Engineering & Math")
fos_w_stem <- fos_pct("Women", "Science, Technology, Engineering & Math")
fos_w_ssh  <- fos_pct("Women", "Social Sciences & Humanities")
fos_w_hlth <- fos_pct("Women", "Health")

# --- CA-WORK -----------------------------------------------------------------
work <- read.csv(file.path(DATA_DIR, "work/industry_sectors_dashboard.csv"))
emp <- read.csv(file.path(DATA_DIR, "work/employment_categories_dashboard.csv"))

make_employment_butterfly <- function(df, gen_val, gen_label, id_prefix, height = "450px", source = NULL) {
  d <- df %>% filter(generation == gen_val)
  emp_cats <- c("Private sector", "Public sector", "Self-employed", "No work in last 5 years")
  colors <- c("Private sector" = "#5a9bd5",
              "Public sector" = "#4a8c6f",
              "Self-employed" = "#d4a943",
              "No work in last 5 years" = "#b0b0b0")
  cat_disp <- function(c) if (is_fa()) unname(CA_EMPCAT_FA[c]) else c
  gl <- gshort(gen_label)
  age_levels <- sort(unique(d$age_cohort))
  d_m <- d %>% filter(gender_label == "Men")
  d_f <- d %>% filter(gender_label == "Women")

  p_men <- plot_ly()
  for (cat_name in emp_cats) {
    sm <- d_m %>% filter(employment_category == cat_name)
    if (nrow(sm) > 0) {
      hover_texts <- htxt(sprintf(tr("ca_hover_bfly_age"), cat_disp(cat_name), gl, tr("ca_men"), sm$age_cohort, fa_num(sm$percentage, 1)))
      p_men <- p_men %>% add_bars(data = sm, y = ~age_cohort, x = ~percentage, name = cat_disp(cat_name),
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_disp(cat_name), showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = !is_fa(), categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (cat_name in emp_cats) {
    sf <- d_f %>% filter(employment_category == cat_name)
    if (nrow(sf) > 0) {
      hover_texts <- htxt(sprintf(tr("ca_hover_bfly_age"), cat_disp(cat_name), gl, tr("ca_women"), sf$age_cohort, fa_num(sf$percentage, 1)))
      p_women <- p_women %>% add_bars(data = sf, y = ~age_cohort, x = ~percentage, name = cat_disp(cat_name),
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_disp(cat_name), showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = pct_suffix),
    yaxis = c(list(title = "", showticklabels = is_fa(), categoryorder = "array", categoryarray = age_levels), if (is_fa()) list(side = "right", automargin = TRUE)))

  p <- subplot(p_men, p_women, shareY = !is_fa(), titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = tr("ca_men"), x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = tr("ca_women"), x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = if (is_fa()) 20 else 60, r = if (is_fa()) 60 else 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  colors_disp <- setNames(unname(colors), vapply(names(colors), cat_disp, character(1)))
  leg <- make_html_legend_hover(colors_disp)
  plotly_div(id_prefix, pj(p), height, source = source, legend_html = leg, highlight_hover = TRUE)
}

make_industry_butterfly <- function(df, gen_val, gen_label, id_prefix, height = "450px", source = NULL) {
  d <- df %>% filter(generation == gen_val)
  ind_cats <- c("Professional & Technical", "Health & Education", "Trade & Services",
                "Manufacturing & Construction", "Public & Other")
  colors <- c("Professional & Technical" = "#1a4e72",
              "Health & Education" = "#4a8c6f",
              "Trade & Services" = "#c4793a",
              "Manufacturing & Construction" = "#d4a943",
              "Public & Other" = "#b0b0b0")
  cat_disp <- function(c) if (is_fa()) unname(CA_INDCAT_FA[c]) else c
  gl <- gshort(gen_label)
  age_levels <- sort(unique(d$age_cohort))
  d_m <- d %>% filter(gender_label == "Men")
  d_f <- d %>% filter(gender_label == "Women")

  p_men <- plot_ly()
  for (cat_name in ind_cats) {
    sm <- d_m %>% filter(industry_category == cat_name)
    if (nrow(sm) > 0) {
      hover_texts <- htxt(sprintf(tr("ca_hover_bfly_age"), cat_disp(cat_name), gl, tr("ca_men"), sm$age_cohort, fa_num(sm$percentage, 1)))
      p_men <- p_men %>% add_bars(data = sm, y = ~age_cohort, x = ~percentage, name = cat_disp(cat_name),
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_disp(cat_name), showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = !is_fa(), categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (cat_name in ind_cats) {
    sf <- d_f %>% filter(industry_category == cat_name)
    if (nrow(sf) > 0) {
      hover_texts <- htxt(sprintf(tr("ca_hover_bfly_age"), cat_disp(cat_name), gl, tr("ca_women"), sf$age_cohort, fa_num(sf$percentage, 1)))
      p_women <- p_women %>% add_bars(data = sf, y = ~age_cohort, x = ~percentage, name = cat_disp(cat_name),
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_disp(cat_name), showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = pct_suffix),
    yaxis = c(list(title = "", showticklabels = is_fa(), categoryorder = "array", categoryarray = age_levels), if (is_fa()) list(side = "right", automargin = TRUE)))

  p <- subplot(p_men, p_women, shareY = !is_fa(), titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = tr("ca_men"), x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = tr("ca_women"), x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = if (is_fa()) 20 else 60, r = if (is_fa()) 60 else 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  colors_disp <- setNames(unname(colors), vapply(names(colors), cat_disp, character(1)))
  leg <- make_html_legend_hover(colors_disp, break_after = 3)
  plotly_div(id_prefix, pj(p), height, source = source, legend_html = leg, highlight_hover = TRUE)
}

work_share <- function(gen, gender, sector, age = NULL) {
  d <- work %>% filter(generation == gen, gender_label == gender, industry_category == sector)
  if (!is.null(age)) d <- d %>% filter(age_cohort == age)
  if (nrow(d) == 0) return(NA_real_)
  round(mean(d$percentage))
}
g1m_pt  <- work_share("First-Generation",  "Men",   "Professional & Technical",     "35-44")
g1m_mc  <- work_share("First-Generation",  "Men",   "Manufacturing & Construction", "35-44")
g1w_ts  <- work_share("First-Generation",  "Women", "Trade & Services",             "35-44")
g1w_pt  <- work_share("First-Generation",  "Women", "Professional & Technical",     "35-44")
g1w_he  <- work_share("First-Generation",  "Women", "Health & Education",           "35-44")
g2m_pt  <- work_share("Second-Generation", "Men",   "Professional & Technical")
g2m_ts  <- work_share("Second-Generation", "Men",   "Trade & Services")
g2w_pt  <- work_share("Second-Generation", "Women", "Professional & Technical")
g2w_he  <- work_share("Second-Generation", "Women", "Health & Education")

# --- CA-INCOME ---------------------------------------------------------------
inc <- read.csv(file.path(DATA_DIR, "work/income_distribution_FINAL.csv"))
inc_age <- read.csv(file.path(DATA_DIR, "work/income_by_age_first_gen.csv"))
inc1 <- inc %>% filter(generation == "First")
inc1$sort_key <- as.numeric(gsub("\\D", "", inc1$income_category))
inc1 <- inc1 %>% arrange(sort_key)
decile_labels <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
inc1$short_label <- decile_labels

age_band_order <- c("Under CA$20k", "CA$20k-CA$40k", "CA$40k-CA$60k", "CA$60k-CA$100k", "CA$100k+")
age_band_colors <- c("Under CA$20k" = "#d4e6f1", "CA$20k-CA$40k" = "#8bbdde",
                      "CA$40k-CA$60k" = "#5a9bd5", "CA$60k-CA$100k" = "#2774AE",
                      "CA$100k+" = "#1a4e72")
age_levels_inc <- c("25-34", "35-44", "45-54", "55-64", "65-74")
inc_age$age_group <- factor(inc_age$age_group, levels = age_levels_inc)
inc_age$income_band <- factor(inc_age$income_band, levels = age_band_order)

ca_d1  <- round(inc1$percentage[inc1$income_category == "Decile 1"])
ca_d10 <- round(inc1$percentage[inc1$income_category == "Decile 10"])
ca_100k_peak <- round(max(inc_age$pct[inc_age$income_band == "CA$100k+"], na.rm = TRUE))
ca_100k_peak_age <- inc_age$age_group[inc_age$income_band == "CA$100k+"][
  which.max(inc_age$pct[inc_age$income_band == "CA$100k+"])]

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Canada [%s] ===\n", LANG))

  pct_suffix <- if (is_fa()) "٪" else "%"   # ٪ on fa charts, % on en

  # --- Per-language source lines ---------------------------------------------
  PUMF_SOURCE        <- paste0(tr("ca_src_prefix"), lnk(PUMF_LINK), tr("ca_src_pumf"))
  PUMF_SRC_IMMIG     <- paste0(tr("ca_src_prefix"), lnk(PUMF_LINK), tr("ca_src_immig"))
  PUMF_SRC_EDUC_1G   <- paste0(tr("ca_src_prefix"), lnk(PUMF_LINK), tr("ca_src_educ_1g"))
  PUMF_SRC_FOS_1G    <- paste0(tr("ca_src_prefix"), lnk(PUMF_LINK), tr("ca_src_fos_1g"))
  PUMF_SRC_WORK_1G   <- paste0(tr("ca_src_prefix"), lnk(PUMF_LINK), tr("ca_src_work_1g"))
  PUMF_SRC_WORK_2G   <- paste0(tr("ca_src_prefix"), lnk(PUMF_LINK), tr("ca_src_work_2g"))
  PUMF_SRC_INCOME    <- paste0(tr("ca_src_prefix"), lnk(PUMF_LINK), tr("ca_src_income"))
  PUMF_SRC_INCOME_AGE<- paste0(tr("ca_src_prefix"), lnk(PUMF_LINK), tr("ca_src_income_age"))
  ONT_SOURCE         <- paste0(tr("ca_src_prefix"), lnk(ONT_LINK), tr("ca_src_ont"))

  # ===========================================================================
  # CA-POPULATION
  # ===========================================================================
  cat("Building ca-population...\n")

  wf_genlabel <- ifelse(wf$is_born, tr("ca_wf_gen1"), tr("ca_wf_gen2"))
  wf$hover <- htxt(sprintf(tr("ca_wf_hover"),
    wf_genlabel, chk(wf$is_born), chk(wf$is_ethnic), chk(wf$is_persian),
    fmtv(wf$Population), fa_num(wf$Percentage_of_Total, 1), fmtv(wf$cumulative)))

  p_waterfall <- plot_ly() %>%
    add_bars(x = wf$xnum, y = wf$Population, base = wf$ymin,
      marker = list(color = wf_colors),
      text = wf$hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("ca_wf_title")),
        font = list(size = 16, family = "Montserrat")),
      # fa: mirror the bars to match the RTL-mirrored criteria_table() below
      # (reversed x range, y ticks right, l/r margins swapped) — see build_us.R.
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE,
        zeroline = FALSE, fixedrange = TRUE,
        range = if (is_fa()) c(nrow(wf) + 0.5, 0.5) else c(0.5, nrow(wf) + 0.5)),
      yaxis = c(list(title = "", tickformat = ",", fixedrange = TRUE),
        if (is_fa()) list(side = "right")),
      showlegend = FALSE,
      margin = list(t = 50, b = 6,
        l = if (is_fa()) 20 else CA_LBL_PX, r = if (is_fa()) CA_LBL_PX else 20),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  ca_matrix_html <- criteria_table(list(
    list(label = tr("ca_crit_born"),    vals = wf$is_born),
    list(label = tr("ca_crit_ethnic"),  vals = wf$is_ethnic),
    list(label = tr("ca_crit_persian"), vals = wf$is_persian)),
    n_cols = nrow(wf), label_px = CA_LBL_PX)

  region_totals$region_disp <- factor(
    if (is_fa()) unname(CA_REGION_FA[as.character(region_totals$region)]) else as.character(region_totals$region),
    levels = if (is_fa()) unname(CA_REGION_FA[region_order]) else region_order)
  region_totals$hover <- htxt(sprintf(tr("ca_region_hover"),
    as.character(region_totals$region_disp), fmtv(round(region_totals$pop)),
    fa_num(region_totals$pct, 1)))
  p_region <- plot_ly(data = region_totals, x = ~region_disp, y = ~pct, type = "bar",
      marker = list(color = region_colors),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("ca_region_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = ""),
      yaxis = list(title = "", ticksuffix = pct_suffix),
      margin = list(t = 55, b = 50), showlegend = FALSE,
      plot_bgcolor = "white", paper_bgcolor = "white") %>%
    config(displayModeBar = FALSE)

  prov_hover <- htxt(sprintf(tr("ca_prov_hover"),
    prov_data$province, fmtv(round(prov_data$pop)),
    fa_num(prov_data$pop / prov_total * 100, 1)))
  p_prov_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = prov_geojson_url,
      locations = prov_data$province, z = prov_data$pop,
      featureidkey = "properties.name",
      text = prov_hover,
      hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.01, "#c6dbef"), c(0.1, "#6baed6"),
        c(0.5, "#2171b5"), c(1, "#08306b")),
      showscale = FALSE,
      marker = list(line = list(color = "white", width = 1), opacity = 0.85)
    ) %>% layout(
      mapbox = list(
        style = "carto-positron",
        center = list(lon = -96, lat = 55),
        zoom = 2.5
      ),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  ont_hover <- htxt(sprintf(tr("ca_ont_hover"),
    ont_data$`Region Name`, fmtv(round(ont_data$pop))))
  p_ont_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = ont_geojson,
      locations = ont_data$GeoUID,
      z = ont_data$pop,
      featureidkey = "properties.GeoUID",
      text = ont_hover,
      hoverinfo = "text",
      colorscale = list(c(0, "#c6dbef"), c(0.05, "#9ecae1"), c(0.15, "#6baed6"),
        c(0.4, "#2171b5"), c(1, "#08306b")),
      showscale = FALSE,
      marker = list(line = list(width = 1, color = "#999"), opacity = 0.85)
    ) %>%
    layout(
      mapbox = list(
        style = "carto-positron",
        center = list(lon = -79.5, lat = 44),
        zoom = 4.5
      ),
      margin = list(t = 10, b = 10, l = 0, r = 0),
      paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  pop_body <- paste0(
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("ca_pop_headline_label"), '</div>',
    sprintf('<div class="number">%s</div>', htxt(ca_pop_headline_number)),
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("ca_pop_headline_caption"), lnk(CA_CENSUS_LINK)), '</div>',
    '<div style="margin:14px auto 0; max-width:460px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("ca_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("ca_pop_idbox_bullet1"), '</li>',
    '<li>', tr("ca_pop_idbox_bullet2"), '</li>',
    '<li>', tr("ca_pop_idbox_bullet3"), '</li>',
    '</ul>',
    '</div>',
    '</div>',
    '<div class="chart-card">',
    plotly_div("ca-waterfall", pj(p_waterfall), "300px"),
    ca_matrix_html,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:6px 0 0; padding-right:2px;">%s</p>', PUMF_SOURCE),
    '</div>',
    '</div>',
    '<div class="chart-row">',
    '<div class="chart-card">', plotly_div("ca-region", pj(p_region), "400px", source = PUMF_SOURCE), '</div>',
    '<div class="chart-card">',
    '<div class="section-title">', tr("ca_geo_section"), '</div>',
    '<div class="tab-bar"><button class="tab-btn active" onclick="switchTab(\'ca-prov-tab\',this,\'ca-geo\')">', tr("ca_tab_by_province"), '</button><button class="tab-btn" onclick="switchTab(\'ca-ont-tab\',this,\'ca-geo\')">', tr("ca_tab_by_ont"), '</button></div>',
    '<div id="ca-prov-tab" class="tab-panel active" data-group="ca-geo">',
    '<div style="position:relative;">',
    plotly_div("ca-prov-map", pj(p_prov_map), "380px", source = PUMF_SOURCE),
    map_overlay_legend(c(
      "#c6dbef" = "1 – 1,000",
      "#6baed6" = "1,000 – 10,000",
      "#2171b5" = "10,000 – 50,000",
      "#08306b" = "50,000 – 150,000")),
    '</div>',
    '<script>if(window.innerWidth<900){setTimeout(function(){var el=document.getElementById("ca-prov-map");if(el&&window.Plotly)Plotly.relayout(el,{"mapbox.zoom":2.0,"mapbox.center.lat":52});},500);}</script>',
    '</div>',
    '<div id="ca-ont-tab" class="tab-panel" data-group="ca-geo">',
    '<div style="position:relative;">',
    plotly_div("ca-ont-map", pj(p_ont_map), "380px", source = ONT_SOURCE),
    map_overlay_legend(c(
      "#c6dbef" = "1 – 1,000",
      "#6baed6" = "1,000 – 10,000",
      "#08306b" = "10,000 – 40,000")),
    '</div>',
    '</div>',
    '</div>',
    '</div>'
  )

  fname_pop <- if (is_fa()) "docs/pages/ca-population.fa.html" else "docs/pages/ca-population.html"
  writeLines(render(tr("ca_pop_title"), pop_body, has_tabs = TRUE), fname_pop)
  cat("  Done\n")


  # ===========================================================================
  # CA-LANGUAGE & RELIGION
  # ===========================================================================
  cat("Building ca-langrelig...\n")

  lang_cat_disp <- function(c) if (is_fa()) unname(CA_LANGCAT_FA[c]) else c
  relig_cat_disp <- function(c) if (is_fa()) unname(CA_RELIGCAT_FA[c]) else c
  genlr_disp <- function(g) if (is_fa()) unname(CA_GENLR_FA[g]) else g
  lang_gen_disp <- if (is_fa()) unname(CA_GENLR_FA[lang_gen_levels]) else lang_gen_levels

  p_lang <- plot_ly()
  for (cat_name in lang_cats) {
    sub <- lang_all %>% filter(short_cat == cat_name)
    if (nrow(sub) > 0) {
      disp <- lang_cat_disp(cat_name)
      sub$gen_disp <- genlr_disp(as.character(sub$gen_label))
      hover_texts <- htxt(sprintf(tr("ca_hover_cat_gen_pct"), disp,
        sub$gen_disp, fa_num(sub$percentage, 1)))
      p_lang <- p_lang %>% add_bars(data = sub, y = ~gen_disp, x = ~percentage, name = disp,
        marker = list(color = lang_colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = disp, showlegend = FALSE, orientation = "h")
    }
  }
  p_lang <- p_lang %>% layout(
    barmode = "stack",
    hoverlabel = list(showarrow = FALSE),
    title = list(text = htxt(tr("ca_lang_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 105)),
    yaxis = list(title = "", categoryorder = "array", categoryarray = rev(lang_gen_disp),
      ticklabelstandoff = 6),
    margin = list(t = 55, b = 40, l = 120), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
    config(displayModeBar = FALSE)
  # fa/RTL: mirror the stacked horizontal bar (BBC Persian) — reverse the
  # percent axis, generation labels to the right, swap l/r margins. fa-only.
  if (is_fa()) p_lang <- p_lang %>% layout(
    xaxis = list(range = c(105, 0)),
    yaxis = list(side = "right"),
    margin = list(t = 55, b = 40, l = 20, r = 120))
  lang_colors_disp <- setNames(unname(lang_colors[lang_cats]),
                               vapply(lang_cats, lang_cat_disp, character(1)))
  lang_leg <- make_html_legend_hover(lang_colors_disp, break_after = 3)

  p_relig <- plot_ly()
  for (cat_name in relig_cats) {
    sub <- relig_all %>% filter(short_cat == cat_name)
    if (nrow(sub) > 0) {
      disp <- relig_cat_disp(cat_name)
      sub$gen_disp <- genlr_disp(as.character(sub$gen_label))
      hover_texts <- htxt(sprintf(tr("ca_hover_cat_gen_pct"), disp,
        sub$gen_disp, fa_num(sub$percentage, 1)))
      p_relig <- p_relig %>% add_bars(data = sub, y = ~gen_disp, x = ~percentage, name = disp,
        marker = list(color = relig_colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = disp, showlegend = FALSE, orientation = "h")
    }
  }
  p_relig <- p_relig %>% layout(
    barmode = "stack",
    hoverlabel = list(showarrow = FALSE),
    title = list(text = htxt(tr("ca_relig_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 105)),
    yaxis = list(title = "", categoryorder = "array", categoryarray = rev(lang_gen_disp),
      ticklabelstandoff = 6),
    margin = list(t = 55, b = 40, l = 120), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
    config(displayModeBar = FALSE)
  # fa/RTL: mirror the stacked horizontal bar (BBC Persian). fa-only.
  if (is_fa()) p_relig <- p_relig %>% layout(
    xaxis = list(range = c(105, 0)),
    yaxis = list(side = "right"),
    margin = list(t = 55, b = 40, l = 20, r = 120))
  relig_colors_disp <- setNames(unname(relig_colors[relig_cats]),
                                vapply(relig_cats, relig_cat_disp, character(1)))
  relig_leg <- make_html_legend_hover(relig_colors_disp, break_after = 3)

  lr1_note <- htxt(sprintf(tr("ca_lr_c1_note"), fa_num(g1_engfr)))
  lr2_note <- htxt(sprintf(tr("ca_lr_c2_note"), fa_num(g2_engfr)))
  lr3_note <- htxt(sprintf(tr("ca_lr_c3_note"), fa_num(g1_noreligion)))
  lr4_note <- htxt(sprintf(tr("ca_lr_c4_note"), fa_num(g2_muslim), fa_num(g2_christian)))

  langrelig_body <- paste0(
    '<div class="page-content-4">',
    card4("p4-t1", bpct(g1_persian), htxt(tr("ca_lr_c1_sentence")), lr1_note),
    card4("p4-t2", bpct(g2_persian), htxt(tr("ca_lr_c2_sentence")), lr2_note),
    card4("p4-t3", bpct(g1_muslim), htxt(tr("ca_lr_c3_sentence")), lr3_note),
    card4("p4-t4", bpct(g2_noreligion), htxt(tr("ca_lr_c4_sentence")), lr4_note),
    '<div class="chart-card p4-c1">', plotly_div("lang", pj(p_lang), "300px", source = PUMF_SOURCE, legend_html = lang_leg, highlight_hover = TRUE), '</div>',
    '<div class="chart-card p4-c2">', plotly_div("relig", pj(p_relig), "300px", source = PUMF_SOURCE, legend_html = relig_leg, highlight_hover = TRUE), '</div>',
    '</div>'
  )

  fname_lr <- if (is_fa()) "docs/pages/ca-langrelig.fa.html" else "docs/pages/ca-langrelig.html"
  writeLines(render(tr("ca_langrelig_title"), langrelig_body, has_tabs = FALSE), fname_lr)
  cat("  Done\n")


  # ===========================================================================
  # CA-IMMIGRATION & CITIZENSHIP
  # ===========================================================================
  cat("Building ca-immigration...\n")

  period_disp <- sub("^Before ", tr("ca_immig_period_before"), immig_annual$period_label)
  immig_annual$hover <- ifelse(immig_annual$is_period,
    htxt(sprintf(tr("ca_immig_hover_period"),
      period_disp,
      fmtv(round(immig_annual$period_total)),
      fmtv(round(immig_annual$count)),
      fa_num(immig_annual$cum_pct, 1))),
    htxt(sprintf(tr("ca_immig_hover_annual"),
      bnum_year(immig_annual$year),
      fmtv(round(immig_annual$count)),
      fa_num(immig_annual$cum_pct, 1))))
  cum_x <- ifelse(immig_annual$is_period, period_disp, as.character(immig_annual$year))
  cum_hover <- htxt(sprintf(tr("ca_immig_hover_cum"), cum_x, fa_num(immig_annual$cum_pct, 1)))
  y2_ticktext <- if (is_fa()) paste0(fa_num(c(0, 25, 50, 75, 100)), "٪") else c("0%", "25%", "50%", "75%", "100%")

  p_ca_immig <- plot_ly() %>%
    add_bars(data = immig_annual, x = ~year, y = ~count,
      width = ~bar_width, marker = list(color = ~bar_color),
      text = ~hover, hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
    add_trace(data = immig_annual, x = ~year,
      y = ~cumulative / max(cumulative) * max(count),
      type = "scatter", mode = "lines",
      yaxis = "y2", line = list(color = "lightblue", width = 2),
      text = cum_hover,
      hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(tr("ca_immig_chart_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickfont = list(size = 10), dtick = 5,
        range = c(1948, 2022)),
      yaxis = list(title = "", tickformat = ","),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
        range = c(0, max(immig_annual$count) * 1.05),
        tickvals = round(seq(0, max(immig_annual$count), length.out = 5)),
        ticktext = y2_ticktext,
        tickfont = list(size = 11)),
      margin = list(t = 65, b = 50, r = 60),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(text = htxt(tr("ca_immig_annot")),
          x = 0.5, y = -0.15, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 9, color = "#6b6b6b"), xanchor = "center"))
    ) %>% config(displayModeBar = FALSE)

  cit1$status_disp <- if (is_fa()) unname(CA_CITSTATUS_FA[cit1$status]) else cit1$status
  cit1$short_label_disp <- factor(
    if (is_fa()) unname(CA_CITSHORT_FA[as.character(cit1$short_label)]) else as.character(cit1$short_label),
    levels = if (is_fa()) unname(CA_CITSHORT_FA[cit1_order]) else cit1_order)
  cit1$hover <- htxt(sprintf(tr("ca_cit_hover"),
    cit1$status_disp, fmtv(round(cit1$count)), fa_num(cit1$percentage, 1)))
  p_ca_cit <- plot_ly(data = cit1, x = ~short_label_disp, y = ~count, type = "bar",
      marker = list(color = cit_colors),
      text = ~hover,
      hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("ca_cit_title")), font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickfont = list(size = 10),
        categoryorder = "array", categoryarray = levels(cit1$short_label_disp)),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 55, b = 60), showlegend = FALSE,
      plot_bgcolor = "white", paper_bgcolor = "white") %>%
    config(displayModeBar = FALSE)

  immtype_disp <- function(c) if (is_fa()) unname(CA_IMMTYPE_FA[c]) else c
  p_ca_immtype <- plot_ly()
  for (cat_name in immtype_cats) {
    sub <- immig_cat_long %>% filter(category == cat_name)
    if (nrow(sub) > 0) {
      disp <- immtype_disp(cat_name)
      hover_texts <- htxt(sprintf(tr("ca_hover_cat_gen_pct"), disp,
        as.character(sub$arrival_period), fa_num(sub$pct, 1)))
      p_ca_immtype <- p_ca_immtype %>% add_bars(data = sub, x = ~arrival_period, y = ~pct, name = disp,
        marker = list(color = immtype_colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = disp, showlegend = FALSE)
    }
  }
  p_ca_immtype <- p_ca_immtype %>% layout(
    barmode = "stack",
    title = list(text = htxt(tr("ca_immtype_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 10),
      categoryorder = "array", categoryarray = period_order_cat),
    yaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 105)),
    margin = list(t = 55, b = 60), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
    config(displayModeBar = FALSE)
  immtype_colors_disp <- setNames(unname(immtype_colors[immtype_cats]),
                                  vapply(immtype_cats, immtype_disp, character(1)))
  immtype_leg <- make_html_legend_hover(immtype_colors_disp)

  cit_div <- paste0(
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'cit-status\',this,\'cit-group\')">', tr("ca_tab_cit_status"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'cit-immtype\',this,\'cit-group\')">', tr("ca_tab_immtype"), '</button>',
    '</div>',
    '<div id="cit-status" class="tab-panel active" data-group="cit-group">',
    plotly_div("ca-cit", pj(p_ca_cit), "400px", source = PUMF_SOURCE),
    '</div>',
    '<div id="cit-immtype" class="tab-panel" data-group="cit-group">',
    plotly_div("ca-immtype", pj(p_ca_immtype), "400px", source = PUMF_SOURCE, legend_html = immtype_leg, highlight_hover = TRUE),
    '</div>'
  )

  im1_big  <- bnum_year(im_median_year)
  im1_sent <- htxt(sprintf(tr("ca_immig_c1_sentence"), bnum_year(im_median_year)))
  im1_note <- htxt(sprintf(tr("ca_immig_c1_note"), fa_num(im_1980s_share)))
  im2_big  <- bpct(cit_naturalized_pct)
  im2_sent <- htxt(tr("ca_immig_c2_sentence"))
  im2_b1   <- htxt(sprintf(tr("ca_immig_c2_b1"), fa_num(cit_not_citizen_pct)))
  im2_b2   <- htxt(tr("ca_immig_c2_b2"))

  immig_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', im1_big, im1_sent, im1_note),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', im2_big, im2_sent, im2_b1, im2_b2),
    '<div class="chart-card pc1">', plotly_div("ca-immig", pj(p_ca_immig), "430px", source = PUMF_SRC_IMMIG), '</div>',
    '<div class="chart-card pc2">', cit_div, '</div>',
    '</div>'
  )

  fname_immig <- if (is_fa()) "docs/pages/ca-immigration.fa.html" else "docs/pages/ca-immigration.html"
  writeLines(render(tr("ca_immig_title"), immig_body, has_tabs = TRUE), fname_immig)
  cat("  Done\n")


  # ===========================================================================
  # CA-EDUCATION
  # ===========================================================================
  cat("Building ca-education...\n")

  ed1_big  <- bpct(ed_w_30)
  ed1_sent <- htxt(sprintf(tr("ca_educ_c1_sentence"), fa_num(ed_m_30)))
  ed1_note <- htxt(sprintf(tr("ca_educ_c1_note"), fa_num(ed_m_55), fa_num(ed_w_55)))
  ed2_big  <- bpct(fos_m_stem)
  ed2_sent <- htxt(sprintf(tr("ca_educ_c2_sentence"), fa_num(fos_w_stem)))
  ed2_note <- htxt(sprintf(tr("ca_educ_c2_note"), fa_num(fos_w_ssh), fa_num(fos_w_hlth)))

  educ_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', ed1_big, ed1_sent, ed1_note),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', ed2_big, ed2_sent, ed2_note),
    '<div class="chart-card pc1">',
    '<div class="section-title">', tr("ca_educ_section1"), '</div>',
    make_ca_educ_butterfly(ed1, "1st Gen", "ca-ed1", source = PUMF_SRC_EDUC_1G),
    '</div>',
    '<div class="chart-card pc2">',
    '<div class="section-title">', tr("ca_educ_section2"), '</div>',
    make_fos_butterfly(fos, "1st Gen", "ca-fos", "300px", source = PUMF_SRC_FOS_1G),
    '</div>',
    '</div>'
  )

  fname_educ <- if (is_fa()) "docs/pages/ca-education.fa.html" else "docs/pages/ca-education.html"
  writeLines(render(tr("ca_educ_title"), educ_body), fname_educ)
  cat("  Done\n")


  # ===========================================================================
  # CA-WORK
  # ===========================================================================
  cat("Building ca-work...\n")

  wk1_note <- htxt(sprintf(tr("ca_work_c1_note"), fa_num(g1m_mc)))
  wk2_note <- htxt(sprintf(tr("ca_work_c2_note"), fa_num(g1w_pt), fa_num(g1w_he)))
  wk3_note <- htxt(sprintf(tr("ca_work_c3_note"), fa_num(g2m_ts)))
  wk4_note <- htxt(sprintf(tr("ca_work_c4_note"), fa_num(g2w_he)))

  work_body <- paste0(
    '<div class="page-content-4">',
    card4("p4-t1", bpct(g1m_pt), htxt(tr("ca_work_c1_sentence")), wk1_note),
    card4("p4-t2", bpct(g1w_ts), htxt(tr("ca_work_c2_sentence")), wk2_note),
    card4("p4-t3", bpct(g2m_pt), htxt(tr("ca_work_c3_sentence")), wk3_note),
    card4("p4-t4", bpct(g2w_pt), htxt(tr("ca_work_c4_sentence")), wk4_note),
    '<div class="chart-card p4-c1">',
    '<div class="section-title">', tr("ca_work_section1"), '</div>',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'wk1-emp\',this,\'wk1\')">', tr("ca_tab_emp_cat"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'wk1-ind\',this,\'wk1\')">', tr("ca_tab_ind_sect"), '</button>',
    '</div>',
    '<div id="wk1-emp" class="tab-panel active" data-group="wk1">',
    make_employment_butterfly(emp, "First-Generation", "1st Gen", "ca-wk1-emp", source = PUMF_SRC_WORK_1G),
    '</div>',
    '<div id="wk1-ind" class="tab-panel" data-group="wk1">',
    make_industry_butterfly(work, "First-Generation", "1st Gen", "ca-wk1-ind", source = PUMF_SRC_WORK_1G),
    '</div>',
    '</div>',
    '<div class="chart-card p4-c2">',
    '<div class="section-title">', tr("ca_work_section2"), '</div>',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'wk2-emp\',this,\'wk2\')">', tr("ca_tab_emp_cat"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'wk2-ind\',this,\'wk2\')">', tr("ca_tab_ind_sect"), '</button>',
    '</div>',
    '<div id="wk2-emp" class="tab-panel active" data-group="wk2">',
    make_employment_butterfly(emp, "Second-Generation", "2nd Gen", "ca-wk2-emp", "300px", source = PUMF_SRC_WORK_2G),
    '</div>',
    '<div id="wk2-ind" class="tab-panel" data-group="wk2">',
    make_industry_butterfly(work, "Second-Generation", "2nd Gen", "ca-wk2-ind", "300px", source = PUMF_SRC_WORK_2G),
    '</div>',
    '</div>',
    '</div>'
  )

  fname_work <- if (is_fa()) "docs/pages/ca-work.fa.html" else "docs/pages/ca-work.html"
  writeLines(render(tr("ca_work_title"), work_body, has_tabs = TRUE), fname_work)
  cat("  Done\n")


  # ===========================================================================
  # CA-INCOME
  # ===========================================================================
  cat("Building ca-income...\n")

  inc1$short_label_disp <- if (is_fa()) fa_digits(as.character(seq_len(nrow(inc1)))) else inc1$short_label
  inc1$income_cat_disp <- if (is_fa()) sub("^Decile ", tr("ca_inc_decile_word"), inc1$income_category) else inc1$income_category
  inc1$dec_hover <- htxt(sprintf(tr("ca_inc_decile_hover"),
    inc1$income_cat_disp, fmtv(round(inc1$households)), fa_num(inc1$percentage, 1)))

  p_inc_decile <- plot_ly(data = inc1, x = ~short_label_disp, y = ~percentage, type = "scatter",
      mode = "markers+lines",
      marker = list(color = "#4A90D9", size = 8),
      line = list(color = "#4A90D9", width = 1),
      text = ~dec_hover,
      hoverinfo = "text", textposition = "none") %>%
    add_trace(y = ~percentage, type = "scatter", mode = "none",
      fill = "tozeroy", fillcolor = "rgba(173,216,230,0.3)",
      marker = list(size = 0, opacity = 0),
      hoverinfo = "skip", showlegend = FALSE) %>%
    add_trace(x = inc1$short_label_disp, y = rep(10, nrow(inc1)), type = "scatter",
      mode = "lines", line = list(color = "#cc0000", width = 1.5, dash = "dot"),
      marker = list(size = 0, opacity = 0),
      hoverinfo = "skip", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(tr("ca_inc_decile_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = tr("ca_inc_decile_xaxis"), titlefont = list(size = 11),
        categoryorder = "array", categoryarray = inc1$short_label_disp),
      yaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, max(inc1$percentage) + 3)),
      showlegend = FALSE,
      margin = list(t = 75, b = 50),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(text = htxt(tr("ca_inc_baseline_annot")), x = inc1$short_label_disp[nrow(inc1) - 2], y = 12,
          showarrow = FALSE, font = list(size = 8, color = "#cc0000"), xanchor = "center"))
    ) %>% config(displayModeBar = FALSE)

  for (i in seq_len(nrow(inc1))) {
    val <- inc1$percentage[i]
    if (val >= 8 && val <= 12) {
      y_pos <- val - 3
    } else {
      y_pos <- val - 1.5
    }
    lab_txt <- if (is_fa()) paste0(fa_num(val, 1), "٪") else sprintf("%.1f%%", val)
    if (is_fa()) {
      # fa/RTL: pin xanchor="center" explicitly — default "auto" resolves to an
      # edge anchor under dir="rtl", offsetting each label off its point.
      p_inc_decile <- p_inc_decile %>% add_annotations(x = inc1$short_label_disp[i],
        y = y_pos, text = lab_txt, xanchor = "center",
        showarrow = FALSE, font = list(size = 10, color = "#4A90D9"))
    } else {
      p_inc_decile <- p_inc_decile %>% add_annotations(x = inc1$short_label_disp[i],
        y = y_pos, text = lab_txt,
        showarrow = FALSE, font = list(size = 10, color = "#4A90D9"))
    }
  }

  incband_disp <- function(b) if (is_fa()) unname(CA_INCBAND_FA[b]) else b
  p_inc_age <- plot_ly()
  for (band in age_band_order) {
    sub <- inc_age %>% filter(income_band == band)
    if (nrow(sub) > 0) {
      disp <- incband_disp(band)
      hover_texts <- htxt(sprintf(tr("ca_inc_age_hover"), disp, sub$age_group, fa_num(sub$pct, 1)))
      p_inc_age <- p_inc_age %>% add_bars(data = sub, y = ~age_group, x = ~pct, name = disp,
        marker = list(color = age_band_colors[band]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = disp, showlegend = FALSE, orientation = "h")
    }
  }
  p_inc_age <- p_inc_age %>% layout(
    barmode = "stack",
    title = list(text = htxt(tr("ca_inc_age_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 105)),
    yaxis = list(title = "", categoryorder = "array", categoryarray = rev(age_levels_inc)),
    margin = list(t = 55, b = 40, l = 60), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)
  # fa/RTL: mirror the stacked horizontal bar (BBC Persian). fa-only.
  if (is_fa()) p_inc_age <- p_inc_age %>% layout(
    xaxis = list(range = c(105, 0)),
    yaxis = list(side = "right"),
    margin = list(t = 55, b = 40, l = 20, r = 60))
  incband_colors_disp <- setNames(unname(age_band_colors[age_band_order]),
                                  vapply(age_band_order, incband_disp, character(1)))
  inc_age_leg <- make_html_legend_hover(incband_colors_disp, break_after = 3)

  inc1_big  <- bpct(ca_d1)
  inc1_sent <- htxt(tr("ca_inc_c1_sentence"))
  inc1_note <- htxt(sprintf(tr("ca_inc_c1_note"), fa_num(ca_d10)))
  inc2_big  <- bpct(ca_100k_peak)
  inc2_sent <- htxt(sprintf(tr("ca_inc_c2_sentence"), htxt(as.character(ca_100k_peak_age))))
  inc2_note <- htxt(tr("ca_inc_c2_note"))

  income_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', inc1_big, inc1_sent, inc1_note),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', inc2_big, inc2_sent, inc2_note),
    '<div class="chart-card pc1">', plotly_div("ca-inc1", pj(p_inc_decile), "500px", source = PUMF_SRC_INCOME), '</div>',
    '<div class="chart-card pc2">', plotly_div("ca-inc-age", pj(p_inc_age), "400px", source = PUMF_SRC_INCOME_AGE, legend_html = inc_age_leg, highlight_hover = TRUE), '</div>',
    '</div>'
  )

  fname_income <- if (is_fa()) "docs/pages/ca-income.fa.html" else "docs/pages/ca-income.html"
  writeLines(render(tr("ca_income_title"), income_body), fname_income)
  cat("  Done\n")
}

cat("\nAll Canada pages built (en + fa).\n")

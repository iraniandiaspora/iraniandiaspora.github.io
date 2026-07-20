# Build Australia pages from pre-exported CSVs
# Run from deployment repo root:
#   Rscript R/build_australia.R
#
# Produces (en + fa):
#   docs/pages/au-population.html   + au-population.fa.html
#   docs/pages/au-immigration.html  + au-immigration.fa.html
#   docs/pages/au-education.html    + au-education.fa.html
#   docs/pages/au-workinc.html      + au-workinc.fa.html
#
# Bilingual (en + fa), following the build_nl.R / build_denmark.R pattern.
# Australia keeps its LOCAL page_template() (tabbed, bespoke grid). The en path
# calls it unchanged so the English output stays BYTE-IDENTICAL; the fa path is
# that same shell run through fa_shell() (the local font token already matches
# the canonical anchor fa_shell() substitutes on, so no alignment is needed).
# All user-facing strings come from R/i18n/strings_australia.R via tr(); numbers
# go through fa_num()/fmtv() so the fa edition renders RTL with Persian digits
# and the Vazirmatn face. The three label-above horizontal-bar charts
# (occupation, industry, qualification) mirror to RTL on fa via the reversed
# x-axis range (hbar_over_labels()$xreversed), like Denmark's industry chart.
#
# Needs the `ozmaps` R package (state boundaries) in addition to
# plotly/dplyr/jsonlite/sf. Install with: install.packages(c("sf", "ozmaps")).

library(plotly)
library(dplyr)
library(jsonlite)
library(sf)
library(ozmaps)
sf_use_s2(FALSE)

DATA_DIR <- "data/australia"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, criteria_table(), chk(),
# map_overlay_legend(), make_html_legend(), hbar_over_labels(), pct_lab(),
# cat_colors(), OKABE_ITO, CAT_OTHER.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# Australia string table + AU_*_FA category lookups (defines the global STR).
source("R/i18n/strings_australia.R")

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
.text-card { background:white; border-radius:8px; padding:20px; text-align:center;
  font-size:15px; line-height:1.6; border:1px solid #e0e0e0; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:visible; min-width:0; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.headline { background:white; border-radius:8px; padding:30px; text-align:center; border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em; }
a { transition: color 0.15s; }
a:hover { color: #1a4e72 !important; text-decoration: underline; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
.stat-row { display:grid; grid-template-columns:repeat(3, 1fr); gap:16px; margin-bottom:20px; }
.stat-card { background:white; border-radius:8px; padding:18px 12px; text-align:center; border:1px solid #e0e0e0; }
.stat-card .stat-num { font-size:24px; font-weight:700; color:#1a4e72; }
.stat-card .stat-label { font-size:12px; color:#666; margin-top:4px; }
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .stat-row { grid-template-columns:1fr 1fr !important; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
}
@media (max-width:600px) {
  .measure-num { font-size:22px !important; }
  .measure-label { font-size:11px !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .stat-row { grid-template-columns:1fr !important; }
  .chart-card { padding:10px; }
  .tab-btn { font-size:11px; padding:4px 8px; }
}', tab_css, '
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

# --- write_page(): en = local page_template() (byte-identical); fa = fa_shell()
write_page <- function(title, body_html, has_tabs, fname_en, fname_fa) {
  html <- page_template(title, body_html, has_tabs = has_tabs)
  if (is_fa()) html <- fa_shell(html)
  writeLines(html, if (is_fa()) fname_fa else fname_en)
}

# --- i18n formatting helpers (build_nl.R pattern) -----------------------------
# lnk():  in fa, isolate a Latin agency link/URL in <bdi> so bidi ordering is
#         correct; in en, pass through unchanged (keeps English byte-identical).
lnk <- function(x) if (is_fa()) bdi(x) else x

# fmtv(): vector-safe big-integer formatter. In en it is LITERALLY
#         format(x, big.mark = ",") — reproducing format()'s common-width
#         PADDING that the committed hover text relies on. In fa it Persian-
#         digits that same padded string (ASCII "," thousands kept).
fmtv <- function(x) {
  s <- format(x, big.mark = ",")
  if (!is_fa()) return(s)
  fa_digits(s)
}

# fmtt(): trimmed variant, where the original used trimws(format(x, ...)).
fmtt <- function(x) trimws(fmtv(x))

# htxt(): Persian-digit any stray Western digits in an assembled display string
#         (hover text, chart titles). Idempotent; NEVER apply to CSS-bearing HTML.
htxt <- function(s) if (is_fa()) fa_digits(s) else s

# --- Latin source links (language-independent, byte-exact per original) -------
L_ABS_FULL_FCD    <- "<a href='https://www.abs.gov.au/census/find-census-data' target='_blank' style='color:#2774AE;'>Australian Bureau of Statistics</a>"
L_ABS_FULL_HEAD   <- "<a href=\"https://www.abs.gov.au/census\" style=\"color:#2774AE;\" target=\"_blank\">Australian Bureau of Statistics</a>"
L_ABS_CENSUS      <- "<a href='https://www.abs.gov.au/census' target='_blank' style='color:#2774AE;'>ABS</a>"
L_ABS_FCD         <- "<a href='https://www.abs.gov.au/census/find-census-data' target='_blank' style='color:#2774AE;'>ABS</a>"

blues <- c("#1a4e72", "#2774AE", "#5a9bd5", "#8bbdde", "#d4e6f1")

# =============================================================================
# LOAD DATA + LANGUAGE-INDEPENDENT PREP (ONCE)
# =============================================================================
cat("Loading Australia extracts...\n")

age_sex   <- read.csv(file.path(DATA_DIR, "age_sex.csv"), stringsAsFactors = FALSE)
state_pop <- read.csv(file.path(DATA_DIR, "state_population.csv"), stringsAsFactors = FALSE)
lga_pop   <- read.csv(file.path(DATA_DIR, "lga_population.csv"), stringsAsFactors = FALSE)
ancestry  <- read.csv(file.path(DATA_DIR, "ancestry_by_state.csv"), stringsAsFactors = FALSE)
persian   <- read.csv(file.path(DATA_DIR, "persian_speakers_by_state.csv"), stringsAsFactors = FALSE)

total_birthplace <- sum(state_pop$count)
total_ancestry   <- sum(ancestry$ancestry_count)
total_persian    <- sum(persian$persian_speakers)

# --- Region rollup (Eastern / Western / Southern / Northern) ------------------
au_region_map <- c(
  "New South Wales" = "Eastern", "Victoria" = "Eastern",
  "Queensland" = "Eastern", "Australian Capital Territory" = "Eastern",
  "Western Australia" = "Western",
  "South Australia" = "Southern", "Tasmania" = "Southern",
  "Northern Territory" = "Northern")

region_totals_au <- state_pop %>%
  filter(state != "Other Territories") %>%
  mutate(region = au_region_map[state]) %>%
  group_by(region) %>%
  summarize(pop = sum(count), .groups = "drop")

region_total_au <- sum(region_totals_au$pop)
region_totals_au$pct <- round(region_totals_au$pop / region_total_au * 100, 1)

region_order_au <- c("Eastern", "Western", "Southern", "Northern")
region_totals_au$region <- factor(region_totals_au$region, levels = region_order_au)
region_totals_au <- region_totals_au %>% arrange(region)

# --- State choropleth geojson -------------------------------------------------
state_sf <- ozmap_states %>% st_transform(4326)
state_pop_map <- state_pop %>% filter(state != "Other Territories")
state_total <- sum(state_pop$count)
state_pop_map$pct <- round(state_pop_map$count / state_total * 100, 1)

state_sf <- state_sf %>% left_join(state_pop_map, by = c("NAME" = "state"))
state_sf$count[is.na(state_sf$count)] <- 0
state_sf$pct[is.na(state_sf$pct)] <- 0

state_geojson_path <- file.path(DATA_DIR, "au_states.geojson")
st_write(state_sf %>% select(NAME, count, geometry),
  state_geojson_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
state_geojson <- jsonlite::fromJSON(state_geojson_path, simplifyVector = FALSE)
state_data <- state_sf %>% st_drop_geometry() %>% filter(NAME != "Other Territories")

# --- Sydney + Melbourne LGA geojson ------------------------------------------
lga_sf <- abs_lga %>% st_transform(4326)
lga_sf$clean_name <- trimws(gsub("\\s*\\([^)]+\\)\\s*$", "", lga_sf$NAME))
lga_sf <- lga_sf %>%
  left_join(lga_pop %>% select(lga_name, count), by = c("clean_name" = "lga_name"))
lga_sf$count[is.na(lga_sf$count)] <- 0

sydney_lgas <- lga_sf %>%
  filter(count > 0) %>%
  st_filter(st_as_sfc(st_bbox(c(xmin = 150.3, ymin = -34.3, xmax = 151.6, ymax = -33.3),
    crs = 4326)))
sydney_lgas <- sydney_lgas %>% filter(count >= 10) %>% arrange(desc(count))
syd_geojson_path <- file.path(DATA_DIR, "au_sydney_lgas.geojson")
st_write(sydney_lgas %>% select(clean_name, count, geometry),
  syd_geojson_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
syd_geojson <- jsonlite::fromJSON(syd_geojson_path, simplifyVector = FALSE)
syd_data <- sydney_lgas %>% st_drop_geometry()
syd_data$pct <- round(syd_data$count / total_birthplace * 100, 1)

melb_lgas <- lga_sf %>%
  filter(count > 0) %>%
  st_filter(st_as_sfc(st_bbox(c(xmin = 144.3, ymin = -38.3, xmax = 146.0, ymax = -37.3),
    crs = 4326)))
melb_lgas <- melb_lgas %>% filter(count >= 10) %>% arrange(desc(count))
melb_geojson_path <- file.path(DATA_DIR, "au_melbourne_lgas.geojson")
st_write(melb_lgas %>% select(clean_name, count, geometry),
  melb_geojson_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
melb_geojson <- jsonlite::fromJSON(melb_geojson_path, simplifyVector = FALSE)
melb_data <- melb_lgas %>% st_drop_geometry()
melb_data$pct <- round(melb_data$count / total_birthplace * 100, 1)

# --- Waterfall prep -----------------------------------------------------------
wf <- read.csv(file.path(DATA_DIR, "au_compound.csv"), stringsAsFactors = FALSE)
wf$ymin <- wf$cumulative - wf$count
wf$ymax <- wf$cumulative
wf$xnum <- seq_len(nrow(wf))
wf$is_birth    <- grepl("Born in Iran", wf$component)
wf$is_ancestry <- grepl("Iranian ancestry", wf$component)
wf$is_language <- grepl("Persian speaker", wf$component)
wf$is_parent   <- grepl("Iran-born parent", wf$component)
wf$grp <- ifelse(wf$is_birth, "First generation",
           ifelse(wf$is_parent, "Second generation", "Iranian ancestry or language"))
wf_colors <- c("#1a4e72", "#2774AE", "#5a9bd5", "#4a8c6f", "#c4793a", "#d4a943", "#7b5ea7", "#e07b54")
AU_LBL_PX <- 80
compound_total <- wf$cumulative[nrow(wf)]

# --- Generation split of the Iranian-ancestry population ----------------------
gen <- read.csv(file.path(DATA_DIR, "au_generation.csv"), stringsAsFactors = FALSE)
g <- function(k) gen$count[gen$component == k]
anc_total <- g("Iranian ancestry total")
gen2_au   <- g("Iranian ancestry born in Australia (2nd gen)")
gen1_os   <- anc_total - gen2_au
gen3rd    <- g("Iranian ancestry born elsewhere (3rd country)")

# --- Immigration: arrivals prep ----------------------------------------------
arrival <- read.csv(file.path(DATA_DIR, "year_of_arrival.csv"), stringsAsFactors = FALSE)
citizenship <- read.csv(file.path(DATA_DIR, "citizenship.csv"), stringsAsFactors = FALSE)

arrival <- arrival %>%
  mutate(period = ifelse(period %in% c("Before 1951", "1951 - 1960", "1961 - 1970"),
    "Before 1971", period)) %>%
  group_by(period) %>%
  summarize(count = sum(count), .groups = "drop")

arr <- arrival %>% mutate(
  year = case_when(
    period == "Before 1971" ~ 1965,
    period == "1971 - 1980" ~ 1975,
    period == "1981 - 1990" ~ 1985,
    period == "1991 - 2000" ~ 1995,
    period == "2001 - 2010" ~ 2005,
    period == "2011 - 2015" ~ 2013,
    grepl("^Arrived", period) ~ as.numeric(gsub("Arrived ", "", period)),
    TRUE ~ NA_real_
  ),
  is_period = !grepl("^Arrived", period),
  period_years = case_when(
    period == "Before 1971" ~ 20, period == "2011 - 2015" ~ 5,
    is_period ~ 10, TRUE ~ 1
  ),
  annual_avg = round(count / period_years),
  bar_width = case_when(
    period == "2011 - 2015" ~ 4.5,
    is_period ~ 9.5,
    TRUE ~ 0.8
  ),
  bar_color = ifelse(is_period, "#5a9bd5", "#2774AE")
) %>% filter(!is.na(year))

arr <- arr %>% arrange(year)
arr$cum_true <- cumsum(arr$count)
total_true <- sum(arr$count)
arr$cum_pct <- round(arr$cum_true / total_true * 100, 1)

# --- Citizenship scalars ------------------------------------------------------
cit_aus <- citizenship$count[citizenship$category == "Australian citizen"]
cit_not <- citizenship$count[citizenship$category == "Not Australian citizen"]
cit_ns  <- citizenship$count[citizenship$category == "Not stated"]
cit_total <- cit_aus + cit_not
cit_pct <- round(cit_aus / cit_total * 100, 1)
cit_data <- data.frame(
  status = c("Australian\ncitizen", "Not an Australian\ncitizen"),
  count = c(cit_aus, cit_not),
  pct = c(cit_pct, round(100 - cit_pct, 1)),
  stringsAsFactors = FALSE)

pre_rev <- sum(arrival$count[arrival$period %in% c("Before 1951", "1951 - 1960",
  "1961 - 1970", "1971 - 1980")])
post_rev_80s <- arrival$count[arrival$period == "1981 - 1990"]
peak_period <- arrival$count[arrival$period == "2011 - 2015"]
peak_period_pct <- round(peak_period / total_birthplace * 100)

# --- Education prep -----------------------------------------------------------
edu <- read.csv(file.path(DATA_DIR, "au_education.csv"), stringsAsFactors = FALSE)
rel <- read.csv(file.path(DATA_DIR, "au_religion.csv"), stringsAsFactors = FALSE)
second_gen <- read.csv(file.path(DATA_DIR, "au_second_gen.csv"), stringsAsFactors = FALSE)

edu_chart <- edu %>%
  filter(!education_level %in% c("Supplementary Codes", "Not stated", "Not applicable")) %>%
  arrange(desc(count))
edu_total <- sum(edu_chart$count)
edu_chart$pct <- round(edu_chart$count / edu_total * 100, 1)
edu_chart$label <- edu_chart$education_level
edu_chart$label <- gsub("Secondary Education - ", "Secondary: ", edu_chart$label)
edu_chart$label <- gsub(" Level$", "", edu_chart$label)
edu_chart$label <- gsub("Graduate Diploma and Graduate Certificate", "Grad. Diploma & Certificate", edu_chart$label)
edu_chart$label <- gsub("Advanced Diploma and Diploma", "Diploma & Adv. Diploma", edu_chart$label)
edu_chart$label <- factor(edu_chart$label, levels = rev(edu_chart$label))

bachelors_plus <- sum(edu_chart$count[edu_chart$education_level %in%
  c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
    "Bachelor Degree Level")])
bachelors_pct <- round(bachelors_plus / edu_total * 100, 1)
postgrad_count <- edu_chart$count[edu_chart$education_level == "Postgraduate Degree Level"][1]
postgrad_pct   <- round(postgrad_count / edu_total * 100)
edu_blues <- colorRampPalette(c("#08306b", "#c6dbef"))(nrow(edu_chart))
edu_ord <- match(levels(edu_chart$label), as.character(edu_chart$label))
edu_xmax <- max(edu_chart$count) * 1.15

# --- Religion by generation prep ----------------------------------------------
rel_gen <- read.csv(file.path(DATA_DIR, "au_religion_by_gen.csv"), stringsAsFactors = FALSE)
rel_cats <- c("Secular/No religion", "Islam", "Other Religions", "Christianity")
rel_gen <- rel_gen %>% filter(religion %in% rel_cats)
rel_gen$gen <- factor(rel_gen$gen, levels = c("1st Generation", "2nd Generation"))
rel_colors <- c(
  "Secular/No religion" = "#d4a943",
  "Islam"               = "#1a4e72",
  "Christianity"        = "#4a8c6f",
  "Other Religions"     = "#7b5ea7"
)
no_relig_pct <- 46.4
islam_pct <- 25.3
second_gen_count <- second_gen$count[1]

# --- Language by generation prep ----------------------------------------------
au_lang <- read.csv(file.path(DATA_DIR, "au_language.csv"), stringsAsFactors = FALSE)
lang_cat_levels <- c("Persian", "Other language", "English only")
lang_cat_colors <- c("Persian" = "#2d6a4f", "Other language" = "#8b6c42",
                     "English only" = "#6c757d")
au_lang$gen <- factor(au_lang$gen, levels = c("2nd Generation", "1st Generation"))
au_lang$lang_cat <- factor(au_lang$lang_cat, levels = lang_cat_levels)

# --- Work & income prep -------------------------------------------------------
lf  <- read.csv(file.path(DATA_DIR, "au_labourforce.csv"), stringsAsFactors = FALSE)
occ <- read.csv(file.path(DATA_DIR, "au_occupation.csv"), stringsAsFactors = FALSE)
ind <- read.csv(file.path(DATA_DIR, "au_industry.csv"), stringsAsFactors = FALSE)
inc <- read.csv(file.path(DATA_DIR, "au_income_weekly.csv"), stringsAsFactors = FALSE)

employed_ft   <- lf$count[lf$status == "Employed, worked full-time"]
employed_pt   <- lf$count[lf$status == "Employed, worked part-time"]
employed_away <- lf$count[lf$status == "Employed, away from work"]
total_employed <- employed_ft + employed_pt + employed_away
unemp_ft <- lf$count[lf$status == "Unemployed, looking for full-time work"]
unemp_pt <- lf$count[lf$status == "Unemployed, looking for part-time work"]
total_unemployed <- unemp_ft + unemp_pt
nilf <- lf$count[lf$status == "Not in the labor force"]
pop_15plus <- total_birthplace - lf$count[lf$status == "Not applicable"]
labour_force <- total_employed + total_unemployed
participation_rate <- round(labour_force / pop_15plus * 100, 1)
unemployment_rate <- round(total_unemployed / labour_force * 100, 1)

occ_chart <- occ %>%
  filter(!occupation %in% c("Inadequately described", "Not stated", "Not applicable")) %>%
  arrange(desc(count))
occ_total <- sum(occ_chart$count)
occ_chart$pct <- round(occ_chart$count / occ_total * 100, 1)
occ_chart$label <- occ_chart$occupation
occ_chart$label <- gsub(" Workers$", "", occ_chart$label)
occ_chart$label <- gsub("Community and Personal Service", "Community Services", occ_chart$label)
occ_chart$label <- gsub("Clerical and Administrative", "Clerical & Admin.", occ_chart$label)
occ_chart$label <- gsub("Technicians and Trades", "Technicians & Trades", occ_chart$label)
occ_chart$label <- gsub("Machinery Operators and Drivers", "Machinery Operators", occ_chart$label)
occ_chart$label <- factor(occ_chart$label, levels = rev(occ_chart$label))
occ_colors <- cat_colors(nrow(occ_chart))
occ_ord <- match(levels(occ_chart$label), as.character(occ_chart$label))
occ_xmax <- max(occ_chart$count) * 1.15

ind_chart <- ind %>%
  filter(!industry %in% c("Inadequately described", "Not stated", "Not applicable")) %>%
  arrange(desc(count)) %>%
  head(15)
ind_total <- sum(ind$count[!ind$industry %in% c("Inadequately described", "Not stated", "Not applicable")])
ind_chart$pct <- round(ind_chart$count / ind_total * 100, 1)
ind_chart$label <- gsub(", Forestry and Fishing", "", ind_chart$industry)
ind_chart$label <- gsub(", Gas, Water and Waste Services", "/Gas/Water", ind_chart$label)
ind_chart$label <- gsub(", Postal and Warehousing", "/Postal", ind_chart$label)
ind_chart$label <- gsub("Information Media and Telecommunications", "Info & Telecom", ind_chart$label)
ind_chart$label <- gsub("Financial and Insurance Services", "Finance & Insurance", ind_chart$label)
ind_chart$label <- gsub(", Hiring and Real Estate Services", "/Real Estate", ind_chart$label)
ind_chart$label <- gsub("Professional, Scientific and Technical Services", "Professional & Technical", ind_chart$label)
ind_chart$label <- gsub("Administrative and Support Services", "Admin. & Support", ind_chart$label)
ind_chart$label <- gsub("Public Administration and Safety", "Public Admin. & Safety", ind_chart$label)
ind_chart$label <- gsub("Education and Training", "Education & Training", ind_chart$label)
ind_chart$label <- gsub("Health Care and Social Assistance", "Health & Social Asst.", ind_chart$label)
ind_chart$label <- gsub("Arts and Recreation Services", "Arts & Recreation", ind_chart$label)
ind_chart$label <- gsub("Accommodation and Food Services", "Accommodation & Food", ind_chart$label)
ind_chart$label <- factor(ind_chart$label, levels = rev(ind_chart$label))
ind_colors <- c(OKABE_ITO, rep(CAT_OTHER, max(0, nrow(ind_chart) - 8)))[seq_len(nrow(ind_chart))]
ind_ord <- match(levels(ind_chart$label), as.character(ind_chart$label))
ind_xmax <- max(ind_chart$count) * 1.15

inc_chart <- inc %>%
  filter(!income_band_weekly %in% c("Not stated", "Not applicable"))
inc_chart$label <- gsub("\\$", "A$", inc_chart$income_band_weekly)
inc_chart$label <- gsub("Nil income", "A$0", inc_chart$label)
inc_chart$label <- gsub("Negative income", "Negative", inc_chart$label)
inc_chart$label <- factor(inc_chart$label, levels = inc_chart$label)
inc_stated <- sum(inc_chart$count)
inc_chart$pct <- round(inc_chart$count / inc_stated * 100, 1)
n_bars <- nrow(inc_chart)
inc_chart$color <- colorRampPalette(c("#c6dbef", "#08306b"))(n_bars)

dec <- read.csv(file.path(DATA_DIR, "au_income_deciles.csv"), stringsAsFactors = FALSE)
decile_labels <- AU_DECILE_EN
dec$label <- decile_labels
dec$label <- factor(dec$label, levels = decile_labels)
dec_d1_pct <- dec$pct[1]
dec_d10_pct <- dec$pct[10]

lf_chart <- lf %>%
  filter(!status %in% c("Not stated", "Not applicable"))
lf_chart$label <- c("Full-time", "Part-time", "Away from work",
  "Unemp. (FT)", "Unemp. (PT)", "Not in\nlabor force")
lf_chart$label <- factor(lf_chart$label, levels = lf_chart$label)
lf_chart$color <- c("#1a4e72", "#2774AE", "#5a9bd5", "#e07b54", "#d4a943", "#b0b0b0")
lf_chart$pct <- round(lf_chart$count / pop_15plus * 100, 1)

top_occ <- occ_chart$occupation[1]
top_occ_pct <- occ_chart$pct[1]
top_ind <- ind_chart$industry[1]
top_ind_pct <- ind_chart$pct[1]

inc_ordered <- inc_chart
inc_ordered$cumsum <- cumsum(inc_ordered$count)
median_idx <- which(inc_ordered$cumsum >= inc_stated / 2)[1]
median_band <- inc_ordered$income_band_weekly[median_idx]

occ_full <- read.csv(file.path(DATA_DIR, "au_occupation.csv"), stringsAsFactors = FALSE)
occ_work <- occ_full[!occ_full$occupation %in%
  c("Not stated", "Not applicable", "Inadequately described"), ]
prof_pct <- round(occ_work$count[occ_work$occupation == "Professionals"] /
                  sum(occ_work$count) * 100)
ind_full <- read.csv(file.path(DATA_DIR, "au_industry.csv"), stringsAsFactors = FALSE)
ind_work <- ind_full[!ind_full$industry %in%
  c("Not stated", "Not applicable", "Inadequately described"), ]
ind_work <- ind_work[order(-ind_work$count), ]
top_ind <- ind_work$industry[1]
top_ind_pct <- round(ind_work$count[1] / sum(ind_work$count) * 100)

inc_dec <- read.csv(file.path(DATA_DIR, "au_income_deciles.csv"), stringsAsFactors = FALSE)
bottom20_pct <- round(sum(inc_dec$pct[inc_dec$decile <= 2]))
top20_pct    <- round(sum(inc_dec$pct[inc_dec$decile >= 9]))

dir.create("docs/pages", showWarnings = FALSE, recursive = TRUE)

# --- local factoid-box helpers ------------------------------------------------
# make_measure_box(): takes a PRE-FORMATTED number string.
make_measure_box <- function(num_str, label, color) {
  sprintf('<div style="background:%s; border-radius:8px; padding:24px 12px; text-align:center; color:white; flex:1; min-width:0;">
  <div class="measure-num" style="font-size:32px; font-weight:700;">%s</div>
  <div class="measure-label" style="font-size:13px; opacity:0.9; margin-top:8px; line-height:1.4;">%s</div>
</div>', color, num_str, label)
}
# make_gen_box() (local variant): takes a PRE-FORMATTED number string and a
# fully-built "N% of total" string (already language-aware).
make_gen_box <- function(val_str, pct_full, label, sublabel, color) {
  sprintf(
    '<div style="background:%s; border-radius:6px; padding:22px 14px; text-align:center; color:white; flex:1; min-width:0;">
      <div style="font-size:30px; font-weight:700; line-height:1.1;">%s</div>
      <div style="font-size:13px; margin-top:4px; font-weight:600;">%s</div>
      <div style="font-size:12px; margin-top:2px; opacity:0.85;">%s</div>
      <div style="font-size:11.5px; margin-top:6px; opacity:0.8;">%s</div>
    </div>', color, val_str, label, pct_full, sublabel)
}
# gpct_full(): "N% of total" — en byte-identical to paste0(round(x), "% of total").
gpct_full <- function(x) {
  p <- round(x)
  if (is_fa()) paste0(fa_num(p, 0), " درصد ", tr("au_of_total"))
  else paste0(p, "% ", "of total")
}

# =============================================================================
# BILINGUAL BUILD LOOP: en (byte-identical) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

  cat(sprintf("=== Building Australia [%s] ===\n", LANG))

  pct_suffix <- if (is_fa()) "٪" else "%"   # ٪ on fa axis ticks

  # Source lines (link passed via %s; lnk() isolates the Latin <a> on fa).
  ABS_SOURCE     <- sprintf(tr("au_src_abs"),       lnk(L_ABS_FULL_FCD))
  WF_SOURCE      <- sprintf(tr("au_src_waterfall"), lnk(L_ABS_CENSUS))
  GENBOX_SOURCE  <- sprintf(tr("au_src_genboxes"),  lnk(L_ABS_CENSUS))
  LANG_SOURCE    <- sprintf(tr("au_src_language"),  lnk(L_ABS_CENSUS))
  REL_SOURCE     <- sprintf(tr("au_src_religion"),  lnk(L_ABS_CENSUS))
  AU_INC_SOURCE  <- sprintf(tr("au_src_income"),    lnk(L_ABS_FCD))

  # ===========================================================================
  # AU-POPULATION
  # ===========================================================================
  cat("Building au-population...\n")

  measures_html <- paste0(
    '<div style="display:flex; flex-direction:column; gap:12px; height:100%; justify-content:center; width:100%;">',
    '<div style="font-size:15px; font-weight:600; text-align:center; color:#333;">', tr("au_measures_title"), '</div>',
    '<div style="display:flex; gap:12px; width:100%;">',
    make_measure_box(fmtv(total_ancestry), tr("au_box_ancestry"), "#1a4e72"),
    make_measure_box(fmtv(total_persian), tr("au_box_persian"), "#5a9bd5"),
    make_measure_box(fmtv(total_birthplace), tr("au_box_born"), "#2774AE"),
    '</div>',
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0;">%s</p>', ABS_SOURCE),
    '</div>'
  )

  # --- Region chart ----------------------------------------------------------
  reg_disp <- if (is_fa()) unname(AU_REGION_FA[as.character(region_totals_au$region)]) else as.character(region_totals_au$region)
  region_plot <- region_totals_au
  region_plot$region_disp <- factor(reg_disp, levels = reg_disp)
  reg_hover <- htxt(sprintf(tr("au_region_hover"), reg_disp,
    fmtv(region_totals_au$pop), fa_num(region_totals_au$pct, 1)))

  p_region <- plot_ly(data = region_plot, x = ~region_disp, y = ~pct, type = "bar",
      marker = list(color = c("#7b5ea7", "#d4a943", "#2ca089", "#e07b54")),
      text = reg_hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("au_region_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = ""),
      yaxis = list(title = "", ticksuffix = pct_suffix),
      margin = list(t = 55, b = 50), showlegend = FALSE,
      plot_bgcolor = "white", paper_bgcolor = "white") %>%
    config(displayModeBar = FALSE)

  # --- State choropleth map --------------------------------------------------
  state_hover <- htxt(sprintf(tr("au_map_hover"), state_data$NAME,
    fmtv(state_data$count), fa_num(state_data$pct, 1)))
  p_state_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = state_geojson,
      locations = state_data$NAME, z = state_data$count,
      featureidkey = "properties.NAME",
      text = state_hover, hoverinfo = "text",
      colorscale = list(c(0, "#e8e8e8"), c(0.01, "#c6dbef"), c(0.1, "#6baed6"),
        c(0.5, "#2171b5"), c(1, "#08306b")),
      showscale = FALSE,
      marker = list(line = list(color = "white", width = 1), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron", center = list(lon = 134, lat = -28), zoom = 3),
      margin = list(t = 10, b = 10, l = 0, r = 0), paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Sydney LGA map --------------------------------------------------------
  syd_hover <- htxt(sprintf(tr("au_map_hover"), syd_data$clean_name,
    fmtv(syd_data$count), fa_num(syd_data$pct, 1)))
  p_sydney_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = syd_geojson,
      locations = syd_data$clean_name, z = syd_data$count,
      featureidkey = "properties.clean_name",
      text = syd_hover, hoverinfo = "text",
      colorscale = list(c(0, "#c6dbef"), c(0.05, "#9ecae1"), c(0.15, "#6baed6"),
        c(0.4, "#2171b5"), c(1, "#08306b")),
      showscale = FALSE,
      marker = list(line = list(width = 1, color = "#999"), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron", center = list(lon = 151.0, lat = -33.85), zoom = 7),
      margin = list(t = 10, b = 10, l = 0, r = 0), paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Melbourne LGA map -----------------------------------------------------
  melb_hover <- htxt(sprintf(tr("au_map_hover"), melb_data$clean_name,
    fmtv(melb_data$count), fa_num(melb_data$pct, 1)))
  p_melbourne_map <- plot_ly() %>%
    add_trace(type = "choroplethmapbox",
      geojson = melb_geojson,
      locations = melb_data$clean_name, z = melb_data$count,
      featureidkey = "properties.clean_name",
      text = melb_hover, hoverinfo = "text",
      colorscale = list(c(0, "#c6dbef"), c(0.05, "#9ecae1"), c(0.15, "#6baed6"),
        c(0.4, "#2171b5"), c(1, "#08306b")),
      showscale = FALSE,
      marker = list(line = list(width = 1, color = "#999"), opacity = 0.85)
    ) %>% layout(
      mapbox = list(style = "carto-positron", center = list(lon = 145.0, lat = -37.85), zoom = 6.8),
      margin = list(t = 10, b = 10, l = 0, r = 0), paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE, scrollZoom = TRUE)

  # --- Waterfall + criteria grid ---------------------------------------------
  wf_grp_disp <- if (is_fa()) unname(AU_WFGRP_FA[wf$grp]) else wf$grp
  wf_hover <- htxt(sprintf(tr("au_wf_hover"),
    wf_grp_disp, chk(wf$is_birth), chk(wf$is_ancestry), chk(wf$is_language), chk(wf$is_parent),
    fmtv(wf$count), fa_num(wf$pct, 1), fmtv(wf$cumulative)))

  p_waterfall <- plot_ly() %>%
    add_bars(x = wf$xnum, y = wf$count, base = wf$ymin,
      marker = list(color = wf_colors),
      text = wf_hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("au_wf_title")),
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
        l = if (is_fa()) 20 else AU_LBL_PX, r = if (is_fa()) AU_LBL_PX else 20),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  au_matrix_html <- criteria_table(list(
    list(label = tr("au_crit_born"),     vals = wf$is_birth),
    list(label = tr("au_crit_ancestry"), vals = wf$is_ancestry),
    list(label = tr("au_crit_language"), vals = wf$is_language),
    list(label = tr("au_crit_parent"),   vals = wf$is_parent)),
    n_cols = nrow(wf), label_px = AU_LBL_PX, sym_px = 15)

  # --- Generation boxes (rendered on immigration page) -----------------------
  au_gen_boxes <- paste0(
    '<div class="section-title" style="margin-top:0;">', tr("au_genbox_title"), '</div>',
    '<div style="display:flex; gap:12px; margin:8px 0 0;">',
    make_gen_box(fmtv(gen1_os), gpct_full(gen1_os / anc_total * 100),
      tr("au_genbox_1_label"), tr("au_genbox_1_sub"), "#1a4e72"),
    make_gen_box(fmtv(gen2_au), gpct_full(gen2_au / anc_total * 100),
      tr("au_genbox_2_label"), tr("au_genbox_2_sub"), "#5a9bd5"),
    '</div>',
    sprintf('<p style="font-size:12.5px; color:#555; margin:14px auto 0; max-width:430px; line-height:1.55;">%s</p>',
      htxt(sprintf(tr("au_genbox_caption"), fmtv(anc_total), fa_num(round(gen3rd / gen1_os * 100), 0)))),
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:10px 0 0;">%s</p>', GENBOX_SOURCE)
  )

  # --- Headline / assemble population page -----------------------------------
  pop_body <- paste0(
    '<div class="chart-row">',
    '<div class="headline">',
    '<div class="label">', tr("au_pop_headline_label"), '</div>',
    '<div class="number">', fmtv(compound_total), '</div>',
    '<div class="label" style="margin-top:6px; font-size:13px; color:#555;">',
    sprintf(tr("au_pop_headline_caption"), lnk(L_ABS_FULL_HEAD)), '</div>',
    '<div style="margin:14px auto 0; max-width:460px; font-size:13px; color:#444; text-align:left; line-height:1.7;">',
    '<p style="margin-bottom:8px;">', tr("au_pop_idbox_intro"), '</p>',
    '<ul style="padding-left:20px; margin:0; line-height:1.5;">',
    '<li>', tr("au_pop_idbox_b1"), '</li>',
    '<li>', tr("au_pop_idbox_b2"), '</li>',
    '<li>', tr("au_pop_idbox_b3"), '</li>',
    '<li>', tr("au_pop_idbox_b4"), '</li>',
    '</ul>',
    '<p style="margin-top:10px; font-size:11px; color:#999; line-height:1.5;">', tr("au_pop_idbox_note"), '</p>',
    '</div>',
    '</div>',
    '<div class="chart-card">',
    plotly_div("au-waterfall", pj(p_waterfall), "320px"),
    au_matrix_html,
    sprintf('<p style="font-size:11px; color:#666; text-align:right; margin:6px 0 0; padding-right:2px;">%s</p>', WF_SOURCE),
    '</div>',
    '</div>',

    '<div class="chart-row">',
    '<div class="chart-card">', plotly_div("au-region", pj(p_region), "400px", source = ABS_SOURCE), '</div>',
    '<div class="chart-card">',
    '<div class="section-title">', tr("au_geo_section"), '</div>',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'au-state-tab\',this,\'au-geo\')">', tr("au_tab_bystate"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'au-syd-tab\',this,\'au-geo\')">', tr("au_tab_sydney"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'au-melb-tab\',this,\'au-geo\')">', tr("au_tab_melbourne"), '</button>',
    '</div>',
    '<div id="au-state-tab" class="tab-panel active" data-group="au-geo">',
    '<div style="position:relative;">',
    plotly_div("au-state-map", pj(p_state_map), "420px", source = ABS_SOURCE),
    map_overlay_legend(c(
      "#c6dbef" = "1 – 1,000",
      "#6baed6" = "1,000 – 10,000",
      "#08306b" = "10,000 – 30,000")),
    '</div>',
    '<script>if(window.innerWidth<900){setTimeout(function(){var el=document.getElementById("au-state-map");if(el&&window.Plotly)Plotly.relayout(el,{"mapbox.zoom":2.2,"mapbox.center.lat":-26});},500);}</script>',
    '</div>',
    '<div id="au-syd-tab" class="tab-panel" data-group="au-geo">',
    '<div style="position:relative;">',
    plotly_div("au-syd-map", pj(p_sydney_map), "420px",
      source = paste0(ABS_SOURCE, tr("au_src_syd_suffix"))),
    map_overlay_legend(c(
      "#c6dbef" = "1 – 500",
      "#6baed6" = "500 – 1,500",
      "#08306b" = "1,500 – 4,000")),
    '</div>',
    '</div>',
    '<div id="au-melb-tab" class="tab-panel" data-group="au-geo">',
    '<div style="position:relative;">',
    plotly_div("au-melb-map", pj(p_melbourne_map), "420px",
      source = paste0(ABS_SOURCE, tr("au_src_melb_suffix"))),
    map_overlay_legend(c(
      "#c6dbef" = "1 – 500",
      "#6baed6" = "500 – 1,500",
      "#08306b" = "1,500 – 3,500")),
    '</div>',
    '<script>if(window.innerWidth<900){setTimeout(function(){var el=document.getElementById("au-melb-map");if(el&&window.Plotly)Plotly.relayout(el,{"mapbox.zoom":6.5});},500);}</script>',
    '</div>',
    '</div>',
    '</div>'
  )

  write_page(tr("au_pop_title"), pop_body, TRUE,
    "docs/pages/au-population.html", "docs/pages/au-population.fa.html")
  cat("  Done\n")


  # ===========================================================================
  # AU-IMMIGRATION
  # ===========================================================================
  cat("Building au-immigration...\n")

  arr_period_disp <- if (is_fa()) ifelse(arr$period == "Before 1971",
    tr("au_arrival_before1971"), arr$period) else arr$period
  arr_hover_period <- sprintf(tr("au_arrival_hover_period"),
    arr_period_disp, fmtt(arr$count), fmtt(arr$annual_avg), fa_num(arr$cum_pct, 1))
  arr_hover_annual <- sprintf(tr("au_arrival_hover_annual"),
    fa_num(arr$year, 0, big = FALSE), fmtt(arr$count), fa_num(arr$cum_pct, 1))
  arr_hover <- htxt(ifelse(arr$is_period, arr_hover_period, arr_hover_annual))
  arr_cum_hover <- htxt(sprintf(tr("au_arrival_hover_cum"),
    ifelse(arr$is_period, arr_period_disp, as.character(arr$year)), fa_num(arr$cum_pct, 1)))

  p_arrival <- plot_ly() %>%
    add_bars(data = arr, x = ~year, y = ~annual_avg,
      width = ~bar_width, marker = list(color = ~bar_color),
      text = arr_hover, hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
    add_trace(data = arr, x = ~year, y = ~cum_pct, type = "scatter",
      mode = "lines", yaxis = "y2",
      line = list(color = "lightblue", width = 2),
      text = arr_cum_hover, hoverinfo = "text", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(tr("au_arrival_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = "", tickfont = list(size = 11), dtick = 10, range = c(1958, 2023)),
      yaxis = list(title = "", tickformat = ","),
      yaxis2 = list(title = "", overlaying = "y", side = "right",
        ticksuffix = pct_suffix, range = c(0, 105), showgrid = FALSE,
        tickfont = list(size = 10, color = "#6b6b6b")),
      margin = list(t = 65, b = 50, r = 40),
      showlegend = FALSE,
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(text = htxt(tr("au_arrival_footnote")),
          x = 0.5, y = -0.12, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 9, color = "#6b6b6b"), xanchor = "center"))
    ) %>% config(displayModeBar = FALSE)

  # --- Citizenship chart -----------------------------------------------------
  cit_status_disp <- if (is_fa()) unname(AU_CIT_X_FA[cit_data$status]) else cit_data$status
  cit_plot <- cit_data
  cit_plot$status_disp <- factor(cit_status_disp, levels = cit_status_disp)
  cit_hover_lab <- if (is_fa()) unname(AU_CIT_HOVER_FA[gsub("\n", " ", cit_data$status)]) else gsub("\n", " ", cit_data$status)
  cit_hover <- htxt(sprintf(tr("au_cit_hover"), cit_hover_lab, fmtv(cit_data$count), fa_num(cit_data$pct, 1)))

  p_cit <- plot_ly(data = cit_plot, x = ~status_disp, y = ~count, type = "bar",
      marker = list(color = c("#2774AE", "#e07b54")),
      text = cit_hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("au_cit_title")),
        font = list(size = 16, family = "Montserrat")),
      xaxis = list(title = ""),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 55, b = 60),
      showlegend = FALSE,
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  im1_big  <- sprintf(tr("au_bignum_pct"), fa_num(peak_period_pct, 0))
  im2_big  <- sprintf(tr("au_bignum_pct"), fa_num(cit_pct, 0))

  immig_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', im1_big, tr("au_im_card1_primary"), tr("au_im_card1_note")),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', im2_big, tr("au_im_card2_primary"), tr("au_im_card2_note")),
    '<div class="chart-card pc1">', plotly_div("au-arrival", pj(p_arrival), "430px", source = ABS_SOURCE), '</div>',
    '<div class="chart-card pc2">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'au-cit-tab\',this,\'au-imm2\')">', tr("au_tab_citizenship"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'au-imm-gen-tab\',this,\'au-imm2\')">', tr("au_tab_bygeneration"), '</button>',
    '</div>',
    '<div id="au-cit-tab" class="tab-panel active" data-group="au-imm2">',
    plotly_div("au-cit", pj(p_cit), "450px", source = ABS_SOURCE),
    '</div>',
    '<div id="au-imm-gen-tab" class="tab-panel" data-group="au-imm2">',
    au_gen_boxes,
    '</div>',
    '</div>',
    '</div>'
  )

  write_page(tr("au_immig_title"), immig_body, TRUE,
    "docs/pages/au-immigration.html", "docs/pages/au-immigration.fa.html")
  cat("  Done\n")


  # ===========================================================================
  # AU-EDUCATION & RELIGION
  # ===========================================================================
  cat("Building au-education...\n")

  # --- Education label-above bar (RTL-mirrored on fa) ------------------------
  edu_cats <- if (is_fa()) unname(AU_EDU_FA[edu_chart$education_level[edu_ord]]) else levels(edu_chart$label)
  edu_end_text <- pct_lab(edu_chart$pct[edu_ord])
  if (is_fa()) edu_end_text <- gsub("%", "٪", fa_digits(edu_end_text))
  ov_edu <- hbar_over_labels(edu_cats, ends = edu_chart$count[edu_ord], end_text = edu_end_text)
  edu_xrange <- if (isTRUE(ov_edu$xreversed)) c(edu_xmax, 0) else c(0, edu_xmax)
  edu_hlab <- if (is_fa()) unname(AU_EDU_FA[edu_chart$education_level]) else edu_chart$education_level
  edu_hover <- htxt(sprintf(tr("au_edu_hover"), edu_hlab, fmtv(edu_chart$count), fa_num(edu_chart$pct, 1)))

  p_edu <- plot_ly(edu_chart, y = ~label, x = ~count, type = "bar",
      orientation = "h", marker = list(color = edu_blues),
      text = edu_hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("au_edu_chart_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE, range = edu_xrange),
      yaxis = ov_edu$yaxis,
      annotations = ov_edu$annotations, bargap = ov_edu$bargap,
      margin = list(l = ov_edu$margin_l, r = 20, t = ov_edu$margin_t, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Religion by generation (100% stacked horizontal) ----------------------
  rel_gen_disp <- if (is_fa()) unname(AU_GEN_FA[as.character(rel_gen$gen)]) else as.character(rel_gen$gen)
  rel_gen_lvl  <- if (is_fa()) unname(AU_GEN_FA[levels(rel_gen$gen)]) else levels(rel_gen$gen)
  rel_gen2 <- rel_gen
  rel_gen2$gen_disp <- factor(rel_gen_disp, levels = rel_gen_lvl)

  p_rel <- plot_ly()
  for (cat in rel_cats) {
    sub <- rel_gen2 %>% filter(religion == cat)
    cat_disp <- if (is_fa()) unname(AU_REL_FA[cat]) else cat
    p_rel <- p_rel %>% add_bars(data = sub, y = ~gen_disp, x = ~pct, name = cat_disp,
      orientation = "h", marker = list(color = rel_colors[cat]),
      hovertext = htxt(sprintf(tr("au_rel_hover"), cat_disp, sub$gen_disp, fa_num(sub$pct, 1))),
      hoverinfo = "text", textposition = "none",
      legendgroup = cat, showlegend = FALSE)
  }
  p_rel <- p_rel %>% layout(
    barmode = "stack",
    title = list(text = htxt(tr("au_rel_title")),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 105)),
    yaxis = list(title = "", categoryorder = "array",
      categoryarray = rev(rel_gen_lvl), ticklabelstandoff = 6),
    margin = list(t = 55, b = 40, l = 120), showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)
  # fa/RTL: mirror the stacked horizontal bar (BBC Persian). fa-only.
  if (is_fa()) p_rel <- p_rel %>% layout(
    xaxis = list(range = c(105, 0)),
    yaxis = list(side = "right"),
    margin = list(t = 55, b = 40, l = 20, r = 120))

  rel_leg_labels <- if (is_fa()) unname(AU_REL_FA[names(rel_colors)]) else names(rel_colors)
  rel_leg <- make_html_legend(rel_colors, labels = rel_leg_labels)

  # --- Language by generation (100% stacked horizontal) ----------------------
  au_lang2 <- au_lang
  au_lang2$gen_disp <- factor(
    if (is_fa()) unname(AU_GEN_FA[as.character(au_lang$gen)]) else as.character(au_lang$gen),
    levels = if (is_fa()) unname(AU_GEN_FA[levels(au_lang$gen)]) else levels(au_lang$gen))

  p_language <- plot_ly()
  for (lc in lang_cat_levels) {
    s <- au_lang2[au_lang2$lang_cat == lc, ]
    lc_disp <- if (is_fa()) unname(AU_LANG_FA[lc]) else lc
    s$hover <- htxt(sprintf(tr("au_lang_hover"), lc_disp, s$gen_disp, fmtv(s$count), fa_num(s$pct, 0)))
    p_language <- p_language %>% add_bars(
      data = s, y = ~gen_disp, x = ~pct, name = lc_disp,
      marker = list(color = lang_cat_colors[[lc]]),
      text = s$hover, hoverinfo = "text", textposition = "none",
      orientation = "h")
  }
  p_language <- p_language %>% layout(
    barmode = "stack",
    title = list(text = htxt(tr("au_lang_chart_title")),
      font = list(size = 14, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 100)),
    yaxis = list(title = "", tickfont = list(size = 12)),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18,
      font = list(size = 11)),
    margin = list(l = 110, r = 20, t = 55, b = 50),
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
    config(displayModeBar = FALSE)
  # fa/RTL: mirror the stacked horizontal bar (BBC Persian). Unlike its HTML-
  # legend siblings, this chart uses a NATIVE plotly legend that does NOT reflow
  # under dir="rtl", so reverse the legend order to keep it aligned with the
  # now-reversed segment order (first category rightmost). fa-only.
  if (is_fa()) p_language <- p_language %>% layout(
    xaxis = list(range = c(100, 0)),
    yaxis = list(side = "right"),
    legend = list(traceorder = "reversed"),
    margin = list(l = 20, r = 110, t = 55, b = 50))

  ed1_big <- sprintf(tr("au_bignum_pct"), fa_num(bachelors_pct, 0))
  ed1_note <- htxt(sprintf(tr("au_ed_card1_note"), fa_num(postgrad_pct, 0)))
  ed2_big <- sprintf(tr("au_bignum_pct"), fa_num(no_relig_pct, 0))
  ed2_islam <- htxt(sprintf(tr("au_ed_islam"), fa_num(islam_pct, 0)))
  ed2_christ <- tr("au_ed_christ")

  edu_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">%s</div>
  </div>', ed1_big, tr("au_ed_card1_primary"), ed1_note),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', ed2_big, tr("au_ed_card2_primary"), ed2_islam, ed2_christ),
    '<div class="chart-card pc1">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'au-edu-tab\',this,\'au-edulang\')">', tr("au_tab_qualifications"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'au-lang-tab\',this,\'au-edulang\')">', tr("au_tab_language"), '</button>',
    '</div>',
    '<div id="au-edu-tab" class="tab-panel active" data-group="au-edulang">',
    plotly_div("au-edu", pj(p_edu), ov_edu$height, source = ABS_SOURCE),
    '</div>',
    '<div id="au-lang-tab" class="tab-panel" data-group="au-edulang">',
    plotly_div("au-language", pj(p_language), "430px", source = LANG_SOURCE),
    '</div>',
    '</div>',
    '<div class="chart-card pc2">', plotly_div("au-rel", pj(p_rel), "430px",
      source = REL_SOURCE, legend_html = rel_leg), '</div>',
    '</div>'
  )

  write_page(tr("au_edu_title"), edu_body, TRUE,
    "docs/pages/au-education.html", "docs/pages/au-education.fa.html")
  cat("  Done\n")


  # ===========================================================================
  # AU-WORK & INCOME
  # ===========================================================================
  cat("Building au-workinc...\n")

  # --- Occupation label-above bar (RTL-mirrored on fa) -----------------------
  occ_cats <- if (is_fa()) unname(AU_OCC_FA[occ_chart$occupation[occ_ord]]) else levels(occ_chart$label)
  occ_end_text <- pct_lab(occ_chart$pct[occ_ord])
  if (is_fa()) occ_end_text <- gsub("%", "٪", fa_digits(occ_end_text))
  ov_occ <- hbar_over_labels(occ_cats, ends = occ_chart$count[occ_ord], end_text = occ_end_text)
  occ_xrange <- if (isTRUE(ov_occ$xreversed)) c(occ_xmax, 0) else c(0, occ_xmax)
  occ_hlab <- if (is_fa()) unname(AU_OCC_FA[occ_chart$occupation]) else occ_chart$occupation
  occ_hover <- htxt(sprintf(tr("au_occ_hover"), occ_hlab, fmtv(occ_chart$count), fa_num(occ_chart$pct, 1)))

  p_occ <- plot_ly(occ_chart, y = ~label, x = ~count, type = "bar",
      orientation = "h", marker = list(color = occ_colors),
      text = occ_hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("au_occ_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE, range = occ_xrange),
      yaxis = ov_occ$yaxis,
      annotations = ov_occ$annotations, bargap = ov_occ$bargap,
      margin = list(l = ov_occ$margin_l, r = 20, t = ov_occ$margin_t, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Industry label-above bar (RTL-mirrored on fa) -------------------------
  ind_cats <- if (is_fa()) unname(AU_IND_FA[ind_chart$industry[ind_ord]]) else levels(ind_chart$label)
  ind_end_text <- pct_lab(ind_chart$pct[ind_ord])
  if (is_fa()) ind_end_text <- gsub("%", "٪", fa_digits(ind_end_text))
  ov_ind <- hbar_over_labels(ind_cats, ends = ind_chart$count[ind_ord], end_text = ind_end_text)
  ind_xrange <- if (isTRUE(ov_ind$xreversed)) c(ind_xmax, 0) else c(0, ind_xmax)
  ind_hlab <- if (is_fa()) unname(AU_IND_FA[ind_chart$industry]) else ind_chart$industry
  ind_hover <- htxt(sprintf(tr("au_ind_hover"), ind_hlab, fmtv(ind_chart$count), fa_num(ind_chart$pct, 1)))

  p_ind <- plot_ly(ind_chart, y = ~label, x = ~count, type = "bar",
      orientation = "h", marker = list(color = ind_colors),
      text = ind_hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("au_ind_title")),
        font = list(size = 14, family = "Montserrat")),
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE, range = ind_xrange),
      yaxis = ov_ind$yaxis,
      annotations = ov_ind$annotations, bargap = ov_ind$bargap,
      margin = list(l = ov_ind$margin_l, r = 20, t = ov_ind$margin_t, b = 30),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Weekly income bar -----------------------------------------------------
  if (is_fa()) {
    inc_lab_disp <- ifelse(as.character(inc_chart$label) %in% names(AU_INCBAND_X_FA),
      unname(AU_INCBAND_X_FA[as.character(inc_chart$label)]), as.character(inc_chart$label))
    inc_band_disp <- ifelse(inc_chart$income_band_weekly %in% names(AU_INCBAND_HOVER_FA),
      unname(AU_INCBAND_HOVER_FA[inc_chart$income_band_weekly]), inc_chart$income_band_weekly)
  } else {
    inc_lab_disp <- as.character(inc_chart$label)
    inc_band_disp <- inc_chart$income_band_weekly
  }
  inc_plot <- inc_chart
  inc_plot$label_disp <- factor(inc_lab_disp, levels = inc_lab_disp)
  inc_hover <- htxt(sprintf(tr("au_inc_hover"), inc_band_disp, fmtv(inc_chart$count), fa_num(inc_chart$pct, 1)))

  p_inc <- plot_ly(inc_plot, x = ~label_disp, y = ~count, type = "bar",
      marker = list(color = ~color),
      text = inc_hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("au_inc_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", tickangle = -45, tickfont = list(size = 9)),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 55, b = 90),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Personal income decile line -------------------------------------------
  dec_lab_disp <- if (is_fa()) AU_DECILE_FA else AU_DECILE_EN
  dec_plot <- dec
  dec_plot$label_disp <- factor(dec_lab_disp, levels = dec_lab_disp)
  dec_hover <- htxt(sprintf(tr("au_dec_hover"), dec_lab_disp, fmtv(dec$count), fa_num(dec$pct, 1)))

  p_dec <- plot_ly(data = dec_plot, x = ~label_disp, y = ~pct, type = "scatter", mode = "markers+lines",
      marker = list(color = "#00897b", size = 8),
      line = list(color = "#00897b", width = 1),
      text = dec_hover, hoverinfo = "text", textposition = "none") %>%
    add_trace(x = ~label_disp, y = ~pct, type = "scatter", mode = "lines",
      fill = "tozeroy", fillcolor = "rgba(178,223,219,0.3)",
      marker = list(size = 0, opacity = 0),
      hoverinfo = "skip", showlegend = FALSE) %>%
    add_trace(x = dec_lab_disp, y = rep(10, 10), type = "scatter", mode = "lines",
      line = list(color = "#cc0000", width = 1.5, dash = "dot"),
      marker = list(size = 0, opacity = 0),
      hoverinfo = "skip", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(tr("au_dec_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = htxt(tr("au_dec_xaxis")), titlefont = list(size = 11),
        categoryorder = "array", categoryarray = dec_lab_disp),
      yaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, max(dec$pct) + 3)),
      showlegend = FALSE,
      margin = list(t = 75, b = 70),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(text = htxt(tr("au_dec_annotation")), x = dec_lab_disp[5], y = 13,
          showarrow = FALSE, font = list(size = 8, color = "#cc0000"), xanchor = "center"))
    ) %>% config(displayModeBar = FALSE)

  # --- Labour force status bar -----------------------------------------------
  lf_lab_disp <- if (is_fa()) unname(AU_LF_SHORT_FA[as.character(lf_chart$label)]) else as.character(lf_chart$label)
  lf_plot <- lf_chart
  lf_plot$label_disp <- factor(lf_lab_disp, levels = lf_lab_disp)
  lf_status_disp <- if (is_fa()) unname(AU_LF_FULL_FA[lf_chart$status]) else lf_chart$status
  lf_hover <- htxt(sprintf(tr("au_lf_hover"), lf_status_disp, fmtv(lf_chart$count), fa_num(lf_chart$pct, 1)))

  p_lf <- plot_ly(lf_plot, x = ~label_disp, y = ~count, type = "bar",
      marker = list(color = ~color),
      text = lf_hover, hoverinfo = "text", textposition = "none") %>%
    layout(
      title = list(text = htxt(tr("au_lf_title")),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "", tickfont = list(size = 10)),
      yaxis = list(title = "", tickformat = ","),
      margin = list(t = 55, b = 60),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  # --- Text cards ------------------------------------------------------------
  wi1_big  <- sprintf(tr("au_bignum_pct"), fa_num(prof_pct, 0))
  top_ind_disp <- if (is_fa() && top_ind %in% names(AU_IND_FA)) unname(AU_IND_FA[top_ind]) else top_ind
  wi1_ind  <- htxt(sprintf(tr("au_wi_card1_ind"), fa_num(top_ind_pct, 0), top_ind_disp))
  wi1_part <- htxt(sprintf(tr("au_wi_card1_part"), fa_num(participation_rate, 0)))
  wi2_big  <- sprintf(tr("au_bignum_pct"), fa_num(bottom20_pct, 0))
  wi2_top  <- htxt(sprintf(tr("au_wi_card2_top"), fa_num(top20_pct, 0)))
  median_disp <- if (is_fa() && median_band %in% names(AU_INCBAND_HOVER_FA)) unname(AU_INCBAND_HOVER_FA[median_band]) else median_band
  wi2_med  <- htxt(sprintf(tr("au_wi_card2_median"), median_disp))
  wi2_hh   <- tr("au_wi_card2_household")

  workinc_body <- paste0(
    '<div class="page-content">',
    sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', wi1_big, tr("au_wi_card1_primary"), wi1_ind, wi1_part),
    sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', wi2_big, tr("au_wi_card2_primary"), wi2_top, wi2_med, wi2_hh),

    '<div class="chart-card pc1">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'au-tab-occ\',this,\'work-tabs\')">', tr("au_tab_occupation"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'au-tab-ind\',this,\'work-tabs\')">', tr("au_tab_industry"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'au-tab-lf\',this,\'work-tabs\')">', tr("au_tab_laborforce"), '</button>',
    '</div>',
    '<div id="au-tab-occ" class="tab-panel active" data-group="work-tabs">',
    plotly_div("au-occ", pj(p_occ), ov_occ$height, source = ABS_SOURCE),
    '</div>',
    '<div id="au-tab-ind" class="tab-panel" data-group="work-tabs">',
    plotly_div("au-ind", pj(p_ind), ov_ind$height, source = ABS_SOURCE),
    '</div>',
    '<div id="au-tab-lf" class="tab-panel" data-group="work-tabs">',
    plotly_div("au-lf", pj(p_lf), "430px", source = ABS_SOURCE),
    '</div>',
    '</div>',

    '<div class="chart-card pc2">',
    '<div class="tab-bar">',
    '<button class="tab-btn active" onclick="switchTab(\'au-tab-dec\',this,\'inc-tabs\')">', tr("au_tab_incdeciles"), '</button>',
    '<button class="tab-btn" onclick="switchTab(\'au-tab-inc\',this,\'inc-tabs\')">', tr("au_tab_weeklyincome"), '</button>',
    '</div>',
    '<div id="au-tab-dec" class="tab-panel active" data-group="inc-tabs">',
    plotly_div("au-dec", pj(p_dec), "430px", source = AU_INC_SOURCE),
    '</div>',
    '<div id="au-tab-inc" class="tab-panel" data-group="inc-tabs">',
    plotly_div("au-inc", pj(p_inc), "430px", source = ABS_SOURCE),
    '</div>',
    '</div>',
    '</div>'
  )

  write_page(tr("au_workinc_title"), workinc_body, TRUE,
    "docs/pages/au-workinc.html", "docs/pages/au-workinc.fa.html")
  cat("  Done\n")
}

cat("All Australia pages built (en + fa).\n")

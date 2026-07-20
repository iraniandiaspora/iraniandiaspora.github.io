# Master build: generate all US chart JSON and assemble us-*.html pages
# Each page = hand-crafted HTML with Plotly.newPlot() calls
# Run from the iraniandiaspora.github.io/ directory:
#   Rscript R/build_us.R
#
# Bilingual (en + fa) following the build_germany.R / build_nl.R pattern.
# All language-independent data prep (reads, factors, numeric shares, PMTiles)
# runs ONCE above the `for (LANG in c("en","fa"))` loop. Every user-facing string
# comes from R/i18n/strings_us.R via tr(); numbers go through fa_num()/fmtv() so
# the eight English editions stay BYTE-IDENTICAL while the Persian editions
# render RTL with Persian digits and the Vazirmatn face. The en path calls the
# local page_template() unchanged; the fa path runs that same shell (and the
# custom MapLibre population shell) through fa_shell().

library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)

DATA_DIR <- "data/us"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS, hbar_over_labels(), pct_lab(),
# criteria_table(), chk(), share_of(), OKABE_ITO.
source("R/_helpers.R")
# Persian-edition helpers: LANG, is_fa(), fa_digits(), fa_num(), bdi(), tr(),
# pj(), fa_shell().
source("R/_helpers_i18n.R")
# US string table (global STR consumed by tr(), plus fa-only display vectors
# US_WATERFALL_FA / US_CITIZEN_FA / US_ADMCAT_FA / ...).
source("R/i18n/strings_us.R")

# Generate a horizontal HTML legend from named color vector.
# break_after: index after which to insert a line break (e.g., 3 = break after 3rd).
# keys: the data-lg value used for legend<->trace highlight linking. Defaults to
# `labels`; the fa editions pass keys = the ENGLISH category (matching each
# trace's legendgroup) while labels carry the Persian display text — so the en
# output stays byte-identical (keys == labels == names) and the fa highlight
# still links correctly.
make_html_legend <- function(colors, labels = names(colors), break_after = NULL, keys = labels) {
  items <- mapply(function(col, lab, key) {
    html_lab <- gsub("&", "&amp;", lab)
    sprintf('<span data-lg="%s" style="display:inline-flex; align-items:center; gap:4px; margin-right:14px; cursor:pointer; transition:opacity 0.2s;" onmouseenter="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOn)el.__hlOn(this.getAttribute(\'data-lg\'));" onmouseleave="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOff)el.__hlOff();"><span style="width:12px; height:12px; background:%s; border-radius:2px; display:inline-block;"></span> %s</span>',
      key, col, html_lab)
  }, colors, labels, keys, SIMPLIFY = TRUE)
  if (!is.null(break_after) && break_after < length(items)) {
    items <- c(items[1:break_after], "<br>", items[(break_after+1):length(items)])
  }
  sprintf('<div style="text-align:center; font-size:12px; color:#444; margin:6px 0 2px; line-height:2;">%s</div>',
    paste(items, collapse = ""))
}

IPUMS_LINK <- "<a href='https://doi.org/10.18128/D010.V16.0' target='_blank' style='color:#2774AE;'>IPUMS USA</a>"
ACS_LINK <- "<a href='https://www.census.gov/programs-surveys/acs/microdata.html' target='_blank' style='color:#2774AE;'>U.S. Census Bureau</a>"

# Tab switching (mirrors build_germany.R). reportHeight() comes from
# iframe_resize_script (already appended to every page body).
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

page_template <- function(title, body_html, has_tabs = FALSE) {
  tab_css <- if (has_tabs) '
.tab-bar { display:flex; justify-content:center; flex-wrap:wrap; gap:4px; margin:12px 0 0; }
.tab-btn { padding:6px 16px; border:1px solid #ddd; background:#f0f0f0; cursor:pointer;
  font-family:"Montserrat",sans-serif; font-size:13px; color:#333; border-radius:4px; margin:0 2px; transition:background 0.15s; white-space:nowrap; }
.tab-btn.active { background:#2774AE; color:white; font-weight:600; border-color:#2774AE; }
.tab-btn:hover:not(.active) { background:#e0e0e0; }
.tab-panel { display:none; }
.tab-panel.active { display:block; }' else ''
  tab_js <- if (has_tabs) tab_switch_script else ''
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
.source { font-size:12px; color:#666; text-align:right; padding:4px 0; margin-top:10px; }
.source a { color:#2774AE; }
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }', tab_css, '
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .text-row-4 { grid-template-columns:1fr 1fr; }
  .text-row, .text-row-4 { order:1; } /* push text below charts on mobile */
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
}
@media (max-width:600px) {
  .text-row-4 { grid-template-columns:1fr !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .chart-card { padding:10px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
</head>
<body>
', body_html, '
', tab_js, '
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

# =============================================================================
# DATA LOAD + LANGUAGE-INDEPENDENT PREP (numeric / factor / colour; ONCE)
# =============================================================================

# --- US-IMMIGRATION ----------------------------------------------------------
immig <- read_csv(file.path(DATA_DIR, "by_yrimmig.csv"), show_col_types = FALSE)

# Rebuild citizenship from iran_data (by_citizen.xlsx had weighting bug)
iran_data <- read_csv(file.path(DATA_DIR, "iran_data.csv"), show_col_types = FALSE)
citizen <- iran_data %>%
  mutate(CITIZEN2 = case_when(
    CITIZEN2 == "US-born" ~ "Born in the US",
    TRUE ~ CITIZEN2)) %>%
  group_by(CITIZEN2) %>%
  summarise(n = sum(PERWT), .groups = "drop")

immig$cum_pct <- round(immig$cumulative_immigrants / max(immig$cumulative_immigrants) * 100, 1)

# Weighted median year of arrival for the first-generation Iran-born population.
fg_yrim <- iran_data %>%
  filter(gen == "1st gen", !is.na(YRIMMIG)) %>%
  mutate(YRIMMIG_num = suppressWarnings(as.numeric(YRIMMIG))) %>%
  filter(!is.na(YRIMMIG_num), YRIMMIG_num > 0) %>%
  arrange(YRIMMIG_num)
fg_yrim$cum <- cumsum(fg_yrim$PERWT) / sum(fg_yrim$PERWT, na.rm = TRUE)
fg_median_year <- fg_yrim$YRIMMIG_num[which(fg_yrim$cum >= 0.5)[1]]

cit_order <- c("Naturalized citizen", "Born in the US", "Not a citizen")
citizen$CITIZEN2 <- factor(citizen$CITIZEN2, levels = cit_order)
citizen <- citizen[order(citizen$CITIZEN2), ]
cit_total <- sum(citizen$n)
citizen$pct <- round(citizen$n / cit_total * 100)
pct_nat    <- citizen$pct[citizen$CITIZEN2 == "Naturalized citizen"]
pct_usborn <- citizen$pct[citizen$CITIZEN2 == "Born in the US"]
pct_noncit <- citizen$pct[citizen$CITIZEN2 == "Not a citizen"]

# --- US-ADMISSIONS -----------------------------------------------------------
lpr <- read_csv(file.path(DATA_DIR, "iran_lpr_1970_2024.csv"), show_col_types = FALSE)
lpr[is.na(lpr)] <- 0
lpr_cat <- lpr

cat_names  <- c("Family", "Employment", "Refugee/Asylee", "Diversity", "Other")
cat_cols   <- c("family", "employment", "refugee_asylee", "diversity", "other")
adm_cat_colors <- c("Family" = "#2774AE", "Employment" = "#8bbdde",
                    "Refugee/Asylee" = "#c0504d", "Diversity" = "#d4a943",
                    "Other" = "#999999")

# Cumulative bases so traces stack Family-at-bottom -> Other-at-top.
base_family   <- rep(0, nrow(lpr_cat))
base_employ   <- lpr_cat$family
base_refugee  <- lpr_cat$family + lpr_cat$employment
base_divers   <- lpr_cat$family + lpr_cat$employment + lpr_cat$refugee_asylee
base_other    <- lpr_cat$family + lpr_cat$employment + lpr_cat$refugee_asylee + lpr_cat$diversity
cat_bases <- list(family = base_family, employment = base_employ,
                  refugee_asylee = base_refugee, diversity = base_divers,
                  other = base_other)

# --- US-EDUCATION ------------------------------------------------------------
e1 <- list(gen_1_wide = read_csv(file.path(DATA_DIR, "gen_1_wide.csv"), show_col_types = FALSE))
e2 <- list(gen_2_wide = read_csv(file.path(DATA_DIR, "gen_2_wide.csv"), show_col_types = FALSE))
{
  bap_share <- function(df, age, gender) {
    share_of(df %>% filter(age_group == age), "educ_factor",
             c("Bachelors degree", "Graduate degree"), gender)
  }
  g1_young_f <- round(bap_share(e1$gen_1_wide, "25-34", "Female"))
  g1_young_m <- round(bap_share(e1$gen_1_wide, "25-34", "Male"))
  g1_old_f   <- round(bap_share(e1$gen_1_wide, "75-84", "Female"))
  g1_old_m   <- round(bap_share(e1$gen_1_wide, "75-84", "Male"))
  g2_mid_f   <- round(bap_share(e2$gen_2_wide, "35-44", "Female"))
  g2_mid_m   <- round(bap_share(e2$gen_2_wide, "35-44", "Male"))
  NULL
}

# --- US-WORK -----------------------------------------------------------------
ec <- list(class = read_csv(file.path(DATA_DIR, "class.csv"), show_col_types = FALSE))

csh <- function(df, g_ = NULL, gen_ = NULL) {
  sub <- df %>% filter(!class_wkrd %in% c("N/A", "Unpaid family member"))
  if (!is.null(g_))   sub <- sub %>% filter(gender == g_)
  if (!is.null(gen_)) sub <- sub %>% filter(gen == gen_)
  tot <- sum(sub$n)
  f <- function(cls) round(sum(sub$n[sub$class_wkrd == cls]) / tot * 100)
  list(priv = f("Private sector employee"), pub = f("Public sector employee"),
       np = f("Non-profit employee"), self = f("Self-employed"))
}
cl_all <- csh(ec$class)
cl_m   <- csh(ec$class, "Male");          cl_f  <- csh(ec$class, "Female")
cl_g1  <- csh(ec$class, gen_ = "1st gen"); cl_g2 <- csh(ec$class, gen_ = "2nd gen")

# ---- Business ownership (self-employment) + occupation data -----------------
br <- read_csv(file.path(DATA_DIR, "us_business_rate.csv"), show_col_types = FALSE) %>%
  arrange(rate_pct)
br$origin <- factor(br$origin, levels = br$origin)
br$col <- ifelse(br$is_iran, "#1a4e72", ifelse(br$is_benchmark, "#c4793a", "#b0b0b0"))
br_xmax <- max(br$rate_pct) * 1.15

bi <- read_csv(file.path(DATA_DIR, "us_business_industry.csv"), show_col_types = FALSE) %>%
  arrange(share_pct)
bi$industry <- dplyr::recode(bi$industry,
  "Arts, entertainment & food service"      = "Arts, food & hospitality",
  "Education, health & social services"     = "Education & health",
  "Finance, real estate & professional"     = "Finance & real estate",
  "Professional & scientific services"      = "Professional & scientific",
  "Transportation, utilities & information" = "Transport & utilities",
  "Wholesale & retail trade"                = "Wholesale & retail")
bi$industry <- factor(bi$industry, levels = bi$industry)
bi_cols <- rev(OKABE_ITO[seq_len(nrow(bi))])
bi_xmax <- max(bi$share_pct) * 1.15

oc <- read_csv(file.path(DATA_DIR, "us_occupation.csv"), show_col_types = FALSE) %>%
  arrange(share_pct)
oc$group <- dplyr::recode(oc$group,
  "Management, business & finance" = "Management & finance",
  "Education, law, social & arts"  = "Education, law & arts",
  "Production, trades & transport" = "Trades & transport")
oc$group <- factor(oc$group, levels = oc$group)
oc_cols <- rev(OKABE_ITO[seq_len(nrow(oc))])
oc_xmax <- max(oc$share_pct) * 1.15

biz_iran <- br$rate_pct[br$is_iran]
biz_imm  <- br$rate_pct[br$is_benchmark]
biz_top  <- round(max(bi$share_pct))
occ_mbsa <- c("Management, business & finance", "Computer & engineering", "Science",
              "Education, law, social & arts", "Health care")
occ_prof <- round(100 * sum(oc$weighted_n[oc$group %in% occ_mbsa]) / sum(oc$weighted_n))

# --- US-MARRIAGE -------------------------------------------------------------
em <- list(spouse_gender = read_csv(file.path(DATA_DIR, "spouse_gender.csv"), show_col_types = FALSE))
sp <- em$spouse_gender %>%
  filter(!is.na(age_group)) %>%
  mutate(
    spouse = case_when(
      spouse == "Iranian" ~ "Iranian",
      spouse == "Other MENA" ~ "Middle Eastern (Non-Iranian)",
      spouse == "Other White" ~ "White (Non-Iranian, Non-Hispanic)",
      spouse == "Hispanic" ~ "Hispanic",
      spouse == "Other" ~ "Asian, Black, Native American",
      TRUE ~ spouse
    )
  )

# --- US-INCOME ---------------------------------------------------------------
iran <- read_csv(file.path(DATA_DIR, "iran_data.csv"), show_col_types = FALSE)
pctiles <- read_csv(file.path(DATA_DIR, "national_reference.csv"), show_col_types = FALSE)
breaks <- c(-Inf, pctiles$p10, pctiles$p20, pctiles$p30, pctiles$p40,
            pctiles$p50, pctiles$p60, pctiles$p70, pctiles$p80, pctiles$p90, Inf)
inc <- iran %>%
  filter(AGE >= 25 & AGE <= 54 & !is.na(HHINCOME)) %>%
  mutate(gen_rank = ifelse(generation == "1st gen", 0L, 1L)) %>%
  arrange(SERIAL, gen_rank, AGE) %>%
  group_by(SERIAL) %>%
  slice(1) %>%
  ungroup() %>%
  select(-gen_rank) %>%
  mutate(decile = cut(HHINCOME, breaks = breaks, labels = paste0("D", 1:10),
                      include.lowest = TRUE, right = TRUE))

decile_share <- function(gen_val, target_decile) {
  d <- inc %>% filter(gen == gen_val) %>%
    group_by(decile) %>%
    summarize(weighted = sum(HHWT, na.rm = TRUE), .groups = "drop") %>%
    mutate(share = weighted / sum(weighted) * 100)
  round(d$share[d$decile == target_decile], 1)
}
fg_d10 <- decile_share("1st gen", "D10")
fg_d1  <- decile_share("1st gen", "D1")
sg_d10 <- decile_share("2nd gen", "D10")
sg_d1  <- decile_share("2nd gen", "D1")

# --- US-LANGUAGE -------------------------------------------------------------
lang_age <- read_csv(file.path(DATA_DIR, "language_age.csv"), show_col_types = FALSE)
lang_cat_levels <- c("Persian", "Other language", "English only")
lang_cat_colors <- c(
  "Persian"        = "#2d6a4f",
  "Other language" = "#8b6c42",
  "English only"   = "#6c757d"
)
lang_long <- lang_age %>%
  select(gen, age_cohort, population, pct_persian, pct_other, pct_english_only,
         persian, other, english_only) %>%
  pivot_longer(
    cols = c(pct_persian, pct_other, pct_english_only),
    names_to = "lang_cat", values_to = "pct"
  ) %>%
  mutate(lang_cat = case_when(
    lang_cat == "pct_persian"      ~ "Persian",
    lang_cat == "pct_other"        ~ "Other language",
    lang_cat == "pct_english_only" ~ "English only"
  ),
  count = case_when(
    lang_cat == "Persian"        ~ persian,
    lang_cat == "Other language" ~ other,
    lang_cat == "English only"   ~ english_only
  )) %>%
  select(gen, age_cohort, lang_cat, count, pct, population)

g1_cohorts <- c("5-24", "25-44", "45-64", "65+")
g2_cohorts <- c("5-17", "18-34", "35+")

gen_totals <- lang_age %>% group_by(gen) %>%
  summarize(
    pop = sum(population),
    persian = sum(persian),
    other = sum(other),
    english_only = sum(english_only),
    pct_persian = round(persian / pop * 100),
    pct_other = round(other / pop * 100),
    pct_english_only = round(english_only / pop * 100),
    .groups = "drop")
g1_lpct <- gen_totals %>% filter(gen == "1st generation")
g2_lpct <- gen_totals %>% filter(gen == "2nd generation")

lang_other <- read_csv(file.path(DATA_DIR, "language_top_other.csv"), show_col_types = FALSE)

# --- US-POPULATION -----------------------------------------------------------
wf <- read_csv(file.path(DATA_DIR, "waterfall_components.csv"), show_col_types = FALSE)
wf$ymin <- wf$cumulative - wf$weighted_n
wf$ymax <- wf$cumulative
wf$xnum <- seq_len(nrow(wf))
wf_colors <- c("#1a4e72", "#2774AE", "#5a9bd5", "#4a8c6f",
               "#c4793a", "#d4a943", "#7b5ea7", "#e07b54")
US_LBL_PX <- 80

region_dat <- iran_data %>%
  filter(!is.na(REGION)) %>%
  group_by(REGION) %>%
  summarize(pop = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(REGION = case_when(
    REGION == 1 ~ "Northeast", REGION == 2 ~ "Midwest",
    REGION == 3 ~ "South", REGION == 4 ~ "West",
    TRUE ~ as.character(REGION))) %>%
  arrange(desc(pop))
region_dat$REGION <- factor(region_dat$REGION, levels = region_dat$REGION)
total_pop <- sum(region_dat$pop)

# --- Generate PMTiles for maps (freestiler) — ONCE, language-independent -----
has_freestiler <- requireNamespace("freestiler", quietly = TRUE)
if (!has_freestiler) {
  cat("  Skipping PMTiles (freestiler not installed) — using existing tiles\n")
} else {
library(freestiler)
library(sf)

state_sf <- st_read(file.path(DATA_DIR, "us_states.geojson"), quiet = TRUE)
state_sf <- st_transform(state_sf, 4326)
state_sf$population_est <- as.numeric(state_sf$population_est)
state_sf <- state_sf[, c("state", "population_est", "geometry")]
state_sf$state <- tools::toTitleCase(state_sf$state)

freestile(state_sf, output = "docs/pages/tiles/us_states.pmtiles",
  layer_name = "states", tile_format = "mvt",
  min_zoom = 2, max_zoom = 7, overwrite = TRUE, quiet = TRUE)
cat("  State PMTiles:", round(file.size("docs/pages/tiles/us_states.pmtiles") / 1e3), "KB\n")

county_sf <- st_read(file.path(DATA_DIR, "ca_counties.geojson"), quiet = TRUE)
county_sf <- st_transform(county_sf, 4326)
county_sf <- county_sf[, c("NAME", "NAMELSAD", "pop_estimate", "moe", "percent", "geometry")]

freestile(county_sf, output = "docs/pages/tiles/ca_counties.pmtiles",
  layer_name = "counties", tile_format = "mvt",
  min_zoom = 4, max_zoom = 10, overwrite = TRUE, quiet = TRUE)
cat("  County PMTiles:", round(file.size("docs/pages/tiles/ca_counties.pmtiles") / 1e3), "KB\n")

library(tigris)
ca_pumas_official <- pumas(state = "06", year = 2020, cb = TRUE)
la_counties_fips <- c("037", "059", "111")  # LA, Orange, Ventura
la_pumas <- ca_pumas_official %>%
  filter(substr(GEOID20, 3, 5) %in% la_counties_fips) %>%
  st_transform(4326)

la_pop <- read.csv(file.path(DATA_DIR, "iranian_americans_LA_CSA_detailed_2024_5yr.csv"))
la_pop$PUMACE20 <- sprintf("%05d", la_pop$puma7 %% 100000)
la_pop$total <- round(la_pop$total)
la_pumas <- la_pumas %>% left_join(la_pop %>% select(PUMACE20, total), by = "PUMACE20")
la_pumas$total[is.na(la_pumas$total)] <- 0
la_pumas$name <- gsub(" PUMA$", "", la_pumas$NAMELSAD20)
la_pumas <- la_pumas[, c("GEOID20", "name", "total", "geometry")]
names(la_pumas)[1] <- "puma_id"

freestile(la_pumas, output = "docs/pages/tiles/la_pumas.pmtiles",
  layer_name = "pumas", tile_format = "mvt",
  min_zoom = 7, max_zoom = 13, overwrite = TRUE, quiet = TRUE)
cat("  LA PUMA PMTiles:", round(file.size("docs/pages/tiles/la_pumas.pmtiles") / 1e3), "KB\n")
} # end if (has_freestiler)

# =============================================================================
# LANGUAGE-AWARE CHART-BUILDER FUNCTIONS (called inside the LANG loop; they read
# the current LANG via is_fa()/tr() and the loop globals SRC_* / pct_suffix).
# =============================================================================

# --- BUTTERFLY: Education (men LEFT reversed x, women RIGHT) ------------------
make_butterfly_educ <- function(df_raw, gen_label, age_collapse = FALSE, height = "500px", id_prefix = "ed") {
  df <- df_raw %>%
    mutate(educ_factor = case_when(
      educ_factor %in% c("Some college", "High school degree", "Less than high school",
                          "No schooling or N/A", "Some high school") ~ "Less than BA degree",
      educ_factor == "Bachelors degree" ~ "BA degree",
      educ_factor == "Graduate degree" ~ "Graduate degree",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(educ_factor))

  if (age_collapse) {
    df <- df %>%
      mutate(age_group = ifelse(as.character(age_group) %in% c("45-54", "55-64", "65-74", "75-84"), "45+", as.character(age_group)))
  } else {
    df$age_group <- as.character(df$age_group)
  }
  df <- df %>%
    group_by(educ_factor, age_group) %>%
    summarize(Male = sum(Male, na.rm = TRUE), Female = sum(Female, na.rm = TRUE), .groups = "drop")

  age_levels <- if (age_collapse) c("25-34", "35-44", "45+") else c("25-34", "35-44", "45-54", "55-64", "65-74", "75-84")
  df$age_group <- factor(df$age_group, levels = age_levels)

  educ_levels <- c("Less than BA degree", "BA degree", "Graduate degree")
  df$educ_factor <- factor(df$educ_factor, levels = educ_levels)
  colors <- c("Less than BA degree" = "#1a4e72", "BA degree" = "#2774AE", "Graduate degree" = "#8bbdde")
  edisp <- function(e) if (is_fa()) unname(US_EDUC_FA[e]) else e

  df <- df %>%
    group_by(age_group) %>%
    mutate(male_pct = round(Male / sum(Male) * 100, 1),
           female_pct = round(Female / sum(Female) * 100, 1)) %>%
    ungroup()

  p_men <- plot_ly()
  for (ed in educ_levels) {
    sub <- df %>% filter(educ_factor == ed)
    hover_texts <- htxt(sprintf(tr("us_educ_hover_m"), edisp(ed), gen_label, fa_num(sub$male_pct, 1)))
    p_men <- p_men %>%
      add_bars(data = sub, y = ~age_group, x = ~male_pct, name = edisp(ed),
        marker = list(color = colors[ed]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = ed, showlegend = TRUE, orientation = "h")
  }
  p_men <- p_men %>% layout(
    barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (ed in educ_levels) {
    sub <- df %>% filter(educ_factor == ed)
    hover_texts <- htxt(sprintf(tr("us_educ_hover_w"), edisp(ed), gen_label, fa_num(sub$female_pct, 1)))
    p_women <- p_women %>%
      add_bars(data = sub, y = ~age_group, x = ~female_pct, name = edisp(ed),
        marker = list(color = colors[ed]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = ed, showlegend = FALSE, orientation = "h")
  }
  p_women <- p_women %>% layout(
    barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = tr("us_men"), x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = tr("us_women"), x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg_labels <- if (is_fa()) unname(US_EDUC_FA[names(colors)]) else names(colors)
  leg <- make_html_legend(colors, labels = leg_labels, break_after = 3, keys = names(colors))
  plotly_div(id_prefix, pj(p), height, source = SRC_EDUC, legend_html = leg, highlight_hover = TRUE)
}

# --- BUTTERFLY: Work ---------------------------------------------------------
make_butterfly_work <- function(df, gen_val, gen_label, age_collapse = FALSE, height = "500px", id_prefix = "wk") {
  d <- df %>%
    filter(gen == gen_val) %>%
    mutate(class_wkrd = ifelse(class_wkrd == "N/A", "No work in last 5 years", class_wkrd)) %>%
    filter(class_wkrd != "Unpaid family member")

  d$age_group <- as.character(d$age_group)
  if (age_collapse) {
    d <- d %>%
      mutate(age_group = ifelse(age_group %in% c("45-54", "55-64", "65-74", "75-84"), "45+", age_group))
  }
  age_levels <- if (age_collapse) c("25-34", "35-44", "45+") else c("25-34", "35-44", "45-54", "55-64", "65-74", "75-84")
  d$age_group <- factor(d$age_group, levels = age_levels)

  d_m <- d %>% filter(gender == "Male") %>%
    group_by(class_wkrd, age_group) %>%
    summarize(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
    group_by(age_group) %>% mutate(pct = round(n / sum(n) * 100, 1)) %>% ungroup()

  d_f <- d %>% filter(gender == "Female") %>%
    group_by(class_wkrd, age_group) %>%
    summarize(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
    group_by(age_group) %>% mutate(pct = round(n / sum(n) * 100, 1)) %>% ungroup()

  cat_order <- c("Private sector employee", "Public sector employee",
                 "Non-profit employee", "Self-employed", "No work in last 5 years")
  colors <- c("Private sector employee" = "#5a9bd5", "Public sector employee" = "#4a8c6f",
    "Non-profit employee" = "#c4793a", "Self-employed" = "#d4a943",
    "No work in last 5 years" = "#b0b0b0")
  wdisp <- function(c) if (is_fa()) unname(US_WORKCLASS_FA[c]) else c

  p_men <- plot_ly()
  for (cat_name in cat_order) {
    sub_m <- d_m %>% filter(class_wkrd == cat_name)
    if (nrow(sub_m) > 0) {
      hover_texts <- htxt(sprintf(tr("us_work_hover_m"), wdisp(cat_name), gen_label, fa_num(sub_m$pct, 1)))
      p_men <- p_men %>% add_bars(data = sub_m, y = ~age_group, x = ~pct, name = wdisp(cat_name),
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_name, showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (cat_name in cat_order) {
    sub_f <- d_f %>% filter(class_wkrd == cat_name)
    if (nrow(sub_f) > 0) {
      hover_texts <- htxt(sprintf(tr("us_work_hover_w"), wdisp(cat_name), gen_label, fa_num(sub_f$pct, 1)))
      p_women <- p_women %>% add_bars(data = sub_f, y = ~age_group, x = ~pct, name = wdisp(cat_name),
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_name, showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = tr("us_men"), x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = tr("us_women"), x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg_labels <- if (is_fa()) unname(US_WORKCLASS_FA[names(colors)]) else names(colors)
  leg <- make_html_legend(colors, labels = leg_labels, break_after = 3, keys = names(colors))
  plotly_div(id_prefix, pj(p), height, source = SRC_WORK, legend_html = leg, highlight_hover = TRUE)
}

# --- BUTTERFLY: Marriage -----------------------------------------------------
make_butterfly_marriage <- function(df, gen_val, gen_label, age_collapse = FALSE, height = "550px", id_prefix = "mar") {
  d <- df %>% filter(gen == gen_val)

  if (age_collapse) {
    d <- d %>% mutate(age_group = ifelse(
      as.character(age_group) %in% c("40-49", "50-59", "60-69", "70-79", "80-89"), "40+",
      as.character(age_group)))
  } else {
    d$age_group <- as.character(d$age_group)
  }

  age_levels <- if (age_collapse) c("20-29", "30-39", "40+") else sort(unique(d$age_group))

  d_m <- d %>% filter(gender == "Male") %>%
    group_by(spouse, age_group) %>%
    summarize(n = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
    group_by(age_group) %>% mutate(pct = round(n / sum(n) * 100, 1)) %>% ungroup()

  d_f <- d %>% filter(gender == "Female") %>%
    group_by(spouse, age_group) %>%
    summarize(n = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
    group_by(age_group) %>% mutate(pct = round(n / sum(n) * 100, 1)) %>% ungroup()

  d_m$age_group <- factor(d_m$age_group, levels = age_levels)
  d_f$age_group <- factor(d_f$age_group, levels = age_levels)

  sp_order <- c("Iranian", "Middle Eastern (Non-Iranian)",
                "White (Non-Iranian, Non-Hispanic)", "Hispanic",
                "Asian, Black, Native American")
  colors <- c("Iranian" = "#4a8c6f",
              "Middle Eastern (Non-Iranian)" = "#1a4e72",
              "White (Non-Iranian, Non-Hispanic)" = "#d4a943",
              "Hispanic" = "#c4793a",
              "Asian, Black, Native American" = "#7b5ea7")
  sdisp <- function(c) if (is_fa()) unname(US_SPOUSE_FA[c]) else c

  p_men <- plot_ly()
  for (sp_name in sp_order) {
    sub <- d_m %>% filter(spouse == sp_name)
    if (nrow(sub) > 0) {
      hover_texts <- htxt(sprintf(tr("us_marriage_hover_m"),
        sdisp(sp_name), gen_label, sub$age_group, fmtv(round(sub$n)), fa_num(sub$pct, 1)))
      p_men <- p_men %>% add_bars(data = sub, y = ~age_group, x = ~pct, name = sdisp(sp_name),
        marker = list(color = colors[sp_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = sp_name, showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (sp_name in sp_order) {
    sub <- d_f %>% filter(spouse == sp_name)
    if (nrow(sub) > 0) {
      hover_texts <- htxt(sprintf(tr("us_marriage_hover_w"),
        sdisp(sp_name), gen_label, sub$age_group, fmtv(round(sub$n)), fa_num(sub$pct, 1)))
      p_women <- p_women %>% add_bars(data = sub, y = ~age_group, x = ~pct, name = sdisp(sp_name),
        marker = list(color = colors[sp_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = sp_name, showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = pct_suffix),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = tr("us_men"), x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = tr("us_women"), x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg_labels <- if (is_fa()) unname(US_SPOUSE_FA[names(colors)]) else names(colors)
  leg <- make_html_legend(colors, labels = leg_labels, break_after = 3, keys = names(colors))
  plotly_div(id_prefix, pj(p), height, source = SRC_MARRIAGE, legend_html = leg, highlight_hover = TRUE)
}

# --- INCOME decile chart -----------------------------------------------------
make_income_chart <- function(df, gen_val, gen_label, id_prefix) {
  d <- df %>% filter(gen == gen_val) %>%
    group_by(decile) %>%
    summarize(n = n(), weighted = sum(HHWT, na.rm = TRUE), .groups = "drop") %>%
    mutate(share = round(weighted / sum(weighted) * 100, 1))

  decile_labels <- if (is_fa()) as.character(1:10) else
    c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
  d$label <- decile_labels[as.numeric(gsub("D", "", d$decile))]
  d$label <- factor(d$label, levels = decile_labels)
  d$hover <- htxt(sprintf(tr("us_income_hover"), d$label, fmtv(d$n), fa_num(d$share, 1)))

  p <- plot_ly(data = d, x = ~label, y = ~share, type = "scatter", mode = "markers+lines",
    marker = list(color = "#4A90D9", size = 8),
    line = list(color = "#4A90D9", width = 1),
    text = ~hover,
    hoverinfo = "text", textposition = "none") %>%
    add_trace(x = ~label, y = ~share, type = "scatter", mode = "none",
      fill = "tozeroy", fillcolor = "rgba(173,216,230,0.3)",
      marker = list(size = 0, opacity = 0),
      hoverinfo = "skip", showlegend = FALSE) %>%
    add_trace(x = decile_labels, y = rep(10, 10), type = "scatter", mode = "lines",
      line = list(color = "#cc0000", width = 1.5, dash = "dot"),
      marker = list(size = 0, opacity = 0),
      hoverinfo = "skip", showlegend = FALSE) %>%
    layout(
      title = list(text = htxt(sprintf(tr("us_income_chart_title"), gen_label)),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = tr("us_income_xaxis"), titlefont = list(size = 11),
        categoryorder = "array", categoryarray = decile_labels),
      yaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, max(d$share) + 3)),
      showlegend = FALSE,
      margin = list(t = 75, b = 70),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(text = htxt(tr("us_income_baseline_annot")), x = decile_labels[5], y = 13,
          showarrow = FALSE, font = list(size = 8, color = "#cc0000"), xanchor = "center"))
    ) %>% config(displayModeBar = FALSE)

  for (i in seq_len(nrow(d))) {
    val <- d$share[i]
    if (val >= 8 && val <= 12) {
      y_pos <- val - 3
    } else {
      y_pos <- val + 1
    }
    lbl <- if (is_fa()) paste0(fa_num(val, 1), "٪") else sprintf("%.1f%%", val)
    if (is_fa()) {
      # fa/RTL: pin xanchor="center" explicitly. The default "auto" resolves to
      # an edge anchor under dir="rtl" (offsetting each label off its point);
      # explicit "center" centers it above/below the point, matching EN.
      p <- p %>% add_annotations(x = d$label[i], y = y_pos, text = lbl,
        xanchor = "center", showarrow = FALSE, font = list(size = 10, color = "#4A90D9"))
    } else {
      p <- p %>% add_annotations(x = d$label[i], y = y_pos, text = lbl,
        showarrow = FALSE, font = list(size = 10, color = "#4A90D9"))
    }
  }

  plotly_div(id_prefix, pj(p), "500px", source = SRC_INCOME)
}

# --- LANGUAGE stacked bars ---------------------------------------------------
make_lang_stack <- function(gen_value, cohort_levels, gen_label) {
  ldisp <- function(c) if (is_fa()) unname(US_LANGCAT_FA[c]) else c
  sub <- lang_long %>%
    filter(gen == gen_value) %>%
    mutate(age_cohort = factor(age_cohort, levels = cohort_levels),
           lang_cat   = factor(lang_cat,   levels = lang_cat_levels)) %>%
    arrange(age_cohort, lang_cat)
  p <- plot_ly()
  for (lc in lang_cat_levels) {
    s <- sub %>% filter(lang_cat == lc) %>%
      mutate(hover_text = htxt(sprintf(tr("us_lang_hover"),
        ldisp(lc), gen_label, age_cohort, fa_num(pct, 0))))
    p <- p %>% add_bars(
      data = s, y = ~age_cohort, x = ~pct, name = ldisp(lc),
      marker = list(color = lang_cat_colors[[lc]]),
      text = ~hover_text,
      hoverinfo = "text", textposition = "none",
      orientation = "h", showlegend = FALSE, legendgroup = lc)
  }
  p <- p %>% layout(
    barmode = "stack",
    title = list(
      text = htxt(sprintf(tr("us_lang_chart_title"), gen_label)),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, range = c(0, 100)),
    yaxis = list(title = "", showticklabels = TRUE,
                 categoryorder = "array", categoryarray = cohort_levels,
                 tickfont = list(size = 12),
                 ticks = "outside", ticklen = 8,
                 tickcolor = "rgba(0,0,0,0)"),
    margin = list(t = 55, b = 40, l = 70, r = 20),
    showlegend = FALSE,
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)
  # fa/RTL: mirror the stacked horizontal bar (BBC Persian). This helper builds
  # both generation charts, so one fa-only branch covers both.
  if (is_fa()) p <- p %>% layout(
    xaxis = list(range = c(100, 0)),
    yaxis = list(side = "right"),
    margin = list(t = 55, b = 40, l = 20, r = 70))
  p
}

# =============================================================================
# Bilingual build loop: en (byte-identical to committed) then fa (RTL Persian).
# =============================================================================
for (LANG in c("en", "fa")) {

cat(sprintf("=== Building United States [%s] ===\n", LANG))

sfx <- if (is_fa()) ".fa.html" else ".html"
pct_suffix <- if (is_fa()) "٪" else "%"

# --- Per-language source citations -------------------------------------------
SRC_POP_1YR  <- sprintf(tr("us_src_pop_1yr"), lnk(IPUMS_LINK))
SRC_POP_5YR  <- sprintf(tr("us_src_pop_5yr"), lnk(ACS_LINK))
SRC_IMMIG    <- sprintf(tr("us_src_immig"), lnk(ACS_LINK))
SRC_CITIZEN  <- sprintf(tr("us_src_citizen"), lnk(ACS_LINK))
SRC_MARRIAGE <- sprintf(tr("us_src_marriage"), lnk(ACS_LINK))
SRC_EDUC     <- sprintf(tr("us_src_educ"), lnk(ACS_LINK))
SRC_WORK     <- sprintf(tr("us_src_work"), lnk(ACS_LINK))
SRC_LANG     <- sprintf(tr("us_src_lang"), lnk(ACS_LINK))
SRC_INCOME   <- sprintf(tr("us_src_income"), lnk(ACS_LINK))
SRC_LPR      <- tr("us_src_lpr")
SRC_BIZ      <- sprintf(tr("us_src_biz"), lnk(ACS_LINK))
SRC_OCC      <- sprintf(tr("us_src_occ"), lnk(ACS_LINK))


# =====================================================
# US IMMIGRATION & CITIZENSHIP
# =====================================================
cat("Building us-immigration...\n")

immig$hover_bar <- htxt(sprintf(tr("us_immig_hover_bar"),
  fa_num(immig$YRIMMIG, 0, big = FALSE), fmtv(immig$n), fa_num(immig$cum_pct, 1)))
immig$hover_line <- htxt(sprintf(tr("us_immig_hover_line"),
  fa_num(immig$YRIMMIG, 0, big = FALSE), fa_num(immig$cum_pct, 1)))
y2_ticktext <- if (is_fa()) c("0٪", "25٪", "50٪", "75٪", "100٪")
               else c("0%", "25%", "50%", "75%", "100%")

p_immig <- plot_ly() %>%
  add_bars(data = immig, x = ~YRIMMIG, y = ~n, marker = list(color = "#2774AE"),
    text = ~hover_bar,
    hoverinfo = "text", textposition = "none", showlegend = FALSE) %>%
  add_lines(data = immig, x = ~YRIMMIG,
    y = ~cumulative_immigrants / max(cumulative_immigrants) * max(n),
    yaxis = "y2", line = list(color = "lightblue", width = 2),
    text = ~hover_line,
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = htxt(tr("us_immig_chart_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = ""), yaxis = list(title = "", tickformat = ","),
    yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
      range = c(0, max(immig$n) * 1.05),
      tickvals = round(seq(0, max(immig$n), length.out = 5)),
      ticktext = y2_ticktext,
      tickfont = list(size = 11)),
    margin = list(t = 55, b = 60, r = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

citizen$disp_chr <- if (is_fa()) unname(US_CITIZEN_FA[as.character(citizen$CITIZEN2)]) else as.character(citizen$CITIZEN2)
citizen$disp <- factor(citizen$disp_chr,
  levels = if (is_fa()) unname(US_CITIZEN_FA[cit_order]) else cit_order)
citizen$hover <- htxt(sprintf(tr("us_citizen_hover"),
  citizen$disp_chr, fmtv(citizen$n), fa_num(citizen$n / cit_total * 100, 1)))

p_citizen <- plot_ly(data = citizen, x = ~disp, y = ~n, type = "bar",
    marker = list(color = c("#2774AE", "#5a9bd5", "#e07b54")),
    text = ~hover,
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = htxt(tr("us_citizen_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = ""), yaxis = list(title = "", tickformat = ","),
    margin = list(t = 55, b = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

immig_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
  </div>', htxt(as.character(fg_median_year)),
    htxt(sprintf(tr("us_immig_c1_primary"), fa_num(fg_median_year, 0, big = FALSE)))),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
  </div>', tr("us_immig_c2_big"),
    htxt(sprintf(tr("us_immig_c2_primary"), fa_num(pct_nat, 0), fa_num(pct_usborn, 0)))),
  '<div class="chart-card pc1">', plotly_div("immig", pj(p_immig), "430px", source = SRC_IMMIG), '</div>',
  '<div class="chart-card pc2">', plotly_div("citizen", pj(p_citizen), source = SRC_CITIZEN), '</div>',
  '</div>'
)
writeLines(render(tr("us_immig_title"), immig_body), paste0("docs/pages/us-immigration", sfx))
cat("  Done\n")


# =====================================================
# US ADMISSIONS HISTORY (INS/DHS official records, 1970-2024)
# =====================================================
cat("Building us-admissions...\n")

lpr$hover_total <- htxt(sprintf(tr("us_lpr_total_hover"),
  fa_num(lpr$year, 0, big = FALSE), fmtv(lpr$total)))

p_lpr_total <- plot_ly(data = lpr, x = ~year, y = ~total,
    type = "scatter", mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2),
    marker = list(color = "#1a4e72", size = 3),
    text = ~hover_total, hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = htxt(tr("us_lpr_total_title")),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5, range = c(1968.5, 2024.5),
      tickfont = list(size = 11)),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero",
      tickfont = list(size = 11)),
    margin = list(t = 75, b = 40),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

p_lpr_cat <- plot_ly()
for (i in rev(seq_along(cat_names))) {
  vals <- lpr_cat[[cat_cols[i]]]
  pre78 <- lpr_cat$year < 1978
  pre92 <- lpr_cat$year < 1992 & !pre78
  disp_cat <- if (is_fa()) unname(US_ADMCAT_FA[cat_names[i]]) else cat_names[i]
  hover <- ifelse(pre78 & cat_cols[i] == "family",
    htxt(sprintf(tr("us_lpr_hover_total_pre78"), fmtv(vals))),
    ifelse(pre92 & cat_cols[i] == "family",
      htxt(sprintf(tr("us_lpr_hover_famemp_pre92"), fmtv(vals))),
      htxt(sprintf(tr("us_lpr_hover_cat"), disp_cat, fmtv(vals)))))
  p_lpr_cat <- p_lpr_cat %>%
    add_bars(x = lpr_cat$year, y = vals,
      base = cat_bases[[cat_cols[i]]],
      marker = list(color = adm_cat_colors[cat_names[i]],
                    line = list(width = 0)),
      name = disp_cat,
      legendgroup = cat_names[i],
      showlegend = FALSE,
      text = hover, hovertemplate = "%{text}<extra></extra>",
      textposition = "none")
}
p_lpr_cat <- p_lpr_cat %>%
  layout(
    barmode = "overlay",
    hovermode = "x unified",
    title = list(text = htxt(tr("us_lpr_cat_title")),
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5, range = c(1968.5, 2024.5),
      tickfont = list(size = 11)),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero",
      tickfont = list(size = 11)),
    annotations = list(
      list(x = 0.5, y = -0.12, xref = "paper", yref = "paper",
           text = htxt(tr("us_lpr_cat_annot")),
           showarrow = FALSE, font = list(size = 9, color = "#6b6b6b"),
           xanchor = "center")
    ),
    margin = list(t = 55, b = 55),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

adm_leg_labels <- if (is_fa()) unname(US_ADMCAT_FA[names(adm_cat_colors)]) else names(adm_cat_colors)
cat_leg <- make_html_legend(adm_cat_colors, labels = adm_leg_labels, keys = names(adm_cat_colors))

adm_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
    fmtv(sum(lpr$total)), htxt(tr("us_adm_c1_primary")),
    htxt(tr("us_adm_c1_b1")), htxt(tr("us_adm_c1_b2")), htxt(tr("us_adm_c1_b3"))),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:15px; font-weight:700; color:#1a4e72; line-height:1.45;">%s</div>
    <ul style="margin:10px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.6;">
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
    </ul>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', tr("us_adm_c2_head"), tr("us_adm_c2_b1"), tr("us_adm_c2_b2"),
    tr("us_adm_c2_b3"), tr("us_adm_c2_b4"), htxt(tr("us_adm_c2_b5")), htxt(tr("us_adm_c2_b6"))),
  '<div class="chart-card pc1">', plotly_div("lpr-total", pj(p_lpr_total), "450px", source = SRC_LPR), '</div>',
  '<div class="chart-card pc2">', plotly_div("lpr-cat", pj(p_lpr_cat), "450px", source = SRC_LPR, legend_html = cat_leg, highlight_hover = TRUE),
  '<script>(function(){var el=document.getElementById("lpr-cat");if(el){el.removeAllListeners("plotly_hover");el.removeAllListeners("plotly_unhover");el.removeAllListeners("plotly_click");}})();</script>',
  '</div>',
  '</div>'
)
writeLines(render(tr("us_adm_title"), adm_body), paste0("docs/pages/us-admissions", sfx))
cat("  Done\n")


# =====================================================
# US EDUCATION
# =====================================================
cat("Building us-education...\n")

educ_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
    </ul>
  </div>',
    htxt(sprintf(tr("us_bignum_pct"), fa_num(g1_young_f, 0))),
    htxt(sprintf(tr("us_educ_c1_primary"), fa_num(g1_young_m, 0))),
    htxt(sprintf(tr("us_educ_c1_b1"), fa_num(g1_old_m, 0), fa_num(g1_old_f, 0)))),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
    </ul>
  </div>',
    htxt(sprintf(tr("us_bignum_pct"), fa_num(g2_mid_f, 0))),
    htxt(sprintf(tr("us_educ_c2_primary"), fa_num(g2_mid_m, 0))),
    htxt(tr("us_educ_c2_b1"))),
  '<div class="chart-card pc1">',
  '<div class="section-title">', tr("us_educ_sec1"), '</div>',
  make_butterfly_educ(e1$gen_1_wide, tr("us_gen1_label"), FALSE, "500px", "ed1"),
  '</div>',
  '<div class="chart-card pc2">',
  '<div class="section-title">', tr("us_educ_sec2"), '</div>',
  make_butterfly_educ(e2$gen_2_wide, tr("us_gen2_label"), TRUE, "500px", "ed2"),
  '</div>',
  '</div>'
)
writeLines(render(tr("us_educ_title"), educ_body), paste0("docs/pages/us-education", sfx))
cat("  Done\n")


# =====================================================
# US WORK
# =====================================================
cat("Building us-work...\n")

# self-employment (label-above bars; RTL-mirrored on fa)
br_end_text <- pct_lab(br$rate_pct)
if (is_fa()) br_end_text <- gsub("%", "٪", fa_digits(br_end_text))
br_ord <- match(levels(br$origin), as.character(br$origin))
br_cats_disp <- if (is_fa()) unname(US_ORIGIN_FA[levels(br$origin)]) else levels(br$origin)
ov_br <- hbar_over_labels(br_cats_disp,
  ends = br$rate_pct[br_ord], end_text = br_end_text[br_ord])
br$origin_disp <- if (is_fa()) unname(US_ORIGIN_FA[as.character(br$origin)]) else as.character(br$origin)
br$hover <- htxt(sprintf(tr("us_bizrate_hover"), br$origin_disp, fa_num(br$rate_pct, 0)))
p_bizrate <- plot_ly(br, x = ~rate_pct, y = ~origin, type = "bar", orientation = "h",
    marker = list(color = br$col),
    text = ~hover,
    hoverinfo = "text", textposition = "none") %>%
  layout(title = list(text = htxt(tr("us_bizrate_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE,
      fixedrange = TRUE, range = if (ov_br$xreversed) c(br_xmax, 0) else c(0, br_xmax)),
    yaxis = ov_br$yaxis,
    annotations = ov_br$annotations, bargap = ov_br$bargap,
    margin = list(t = ov_br$margin_t, b = 40, l = ov_br$margin_l, r = 12),
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

# business sectors (label-above bars)
bi_end_text <- pct_lab(bi$share_pct)
if (is_fa()) bi_end_text <- gsub("%", "٪", fa_digits(bi_end_text))
bi_cats_disp <- if (is_fa()) unname(US_BIZIND_FA[levels(bi$industry)]) else levels(bi$industry)
ov_bi <- hbar_over_labels(bi_cats_disp, ends = bi$share_pct, end_text = bi_end_text)
bi$industry_disp <- if (is_fa()) unname(US_BIZIND_FA[as.character(bi$industry)]) else as.character(bi$industry)
bi$hover <- htxt(sprintf(tr("us_bizind_hover"), bi$industry_disp, fa_num(bi$share_pct, 0)))
p_bizind <- plot_ly(bi, x = ~share_pct, y = ~industry, type = "bar", orientation = "h",
    marker = list(color = bi_cols),
    text = ~hover,
    hoverinfo = "text", textposition = "none") %>%
  layout(title = list(text = htxt(tr("us_bizind_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, zeroline = FALSE, fixedrange = TRUE,
      range = if (ov_bi$xreversed) c(bi_xmax, 0) else c(0, bi_xmax)),
    yaxis = ov_bi$yaxis,
    annotations = ov_bi$annotations, bargap = ov_bi$bargap,
    margin = list(t = ov_bi$margin_t, b = 40, l = ov_bi$margin_l, r = 12),
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

# occupations (label-above bars)
oc_end_text <- pct_lab(oc$share_pct)
if (is_fa()) oc_end_text <- gsub("%", "٪", fa_digits(oc_end_text))
oc_cats_disp <- if (is_fa()) unname(US_OCC_FA[levels(oc$group)]) else levels(oc$group)
ov_oc <- hbar_over_labels(oc_cats_disp, ends = oc$share_pct, end_text = oc_end_text)
oc$group_disp <- if (is_fa()) unname(US_OCC_FA[as.character(oc$group)]) else as.character(oc$group)
oc$hover <- htxt(sprintf(tr("us_occ_hover"), oc$group_disp, fa_num(oc$share_pct, 0)))
p_occ <- plot_ly(oc, x = ~share_pct, y = ~group, type = "bar", orientation = "h",
    marker = list(color = oc_cols),
    text = ~hover,
    hoverinfo = "text", textposition = "none") %>%
  layout(title = list(text = htxt(tr("us_occ_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", ticksuffix = pct_suffix, zeroline = FALSE, fixedrange = TRUE,
      range = if (ov_oc$xreversed) c(oc_xmax, 0) else c(0, oc_xmax)),
    yaxis = ov_oc$yaxis,
    annotations = ov_oc$annotations, bargap = ov_oc$bargap,
    margin = list(t = ov_oc$margin_t, b = 40, l = ov_oc$margin_l, r = 12),
    plot_bgcolor = "white", paper_bgcolor = "white") %>%
  config(displayModeBar = FALSE)

work_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
    htxt(sprintf(tr("us_bignum_pct"), fa_num(cl_all$priv, 0))),
    htxt(tr("us_work_c1_primary")),
    htxt(sprintf(tr("us_work_c1_b1"), fa_num(cl_f$pub, 0), fa_num(cl_m$pub, 0), fa_num(cl_f$np, 0), fa_num(cl_m$np, 0))),
    htxt(sprintf(tr("us_work_c1_b2"), fa_num(cl_m$self, 0), fa_num(cl_f$self, 0), fa_num(cl_g1$self, 0), fa_num(cl_g2$self, 0)))),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
    htxt(sprintf(tr("us_bignum_pct"), fa_num(occ_prof, 0))),
    htxt(tr("us_work_c2_primary")),
    htxt(sprintf(tr("us_work_c2_b1"), fa_num(biz_iran, 0), fa_num(biz_imm, 0))),
    htxt(sprintf(tr("us_work_c2_b2"), fa_num(biz_top, 0)))),
  '<div class="chart-card pc1">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'wk-1gen\',this,\'wk-gen\')">', tr("us_tab_1gen"), '</button>',
  '<button class="tab-btn" onclick="switchTab(\'wk-2gen\',this,\'wk-gen\')">', tr("us_tab_2gen"), '</button>',
  '</div>',
  '<div id="wk-1gen" class="tab-panel active" data-group="wk-gen">',
  '<div class="section-title">', tr("us_work_sec1"), '</div>',
  make_butterfly_work(ec$class, "1st gen", tr("us_gen1_label"), FALSE, "500px", "wk1"),
  '</div>',
  '<div id="wk-2gen" class="tab-panel" data-group="wk-gen">',
  '<div class="section-title">', tr("us_work_sec2"), '</div>',
  make_butterfly_work(ec$class, "2nd gen", tr("us_gen2_label"), TRUE, "500px", "wk2"),
  '</div>',
  '</div>',
  '<div class="chart-card pc2">',
  '<div class="tab-bar">',
  '<button class="tab-btn active" onclick="switchTab(\'bo-occ\',this,\'bo-tabs\')">', tr("us_tab_occ"), '</button>',
  '<button class="tab-btn" onclick="switchTab(\'bo-rate\',this,\'bo-tabs\')">', tr("us_tab_selfemp"), '</button>',
  '<button class="tab-btn" onclick="switchTab(\'bo-ind\',this,\'bo-tabs\')">', tr("us_tab_sectors"), '</button>',
  '</div>',
  '<div id="bo-occ" class="tab-panel active" data-group="bo-tabs">',
  plotly_div("us-occ", pj(p_occ), "500px", source = SRC_OCC),
  '</div>',
  '<div id="bo-rate" class="tab-panel" data-group="bo-tabs">',
  plotly_div("biz-rate", pj(p_bizrate), ov_br$height, source = SRC_BIZ),
  '</div>',
  '<div id="bo-ind" class="tab-panel" data-group="bo-tabs">',
  plotly_div("biz-ind", pj(p_bizind), "500px", source = SRC_BIZ),
  '</div>',
  '</div>',
  '</div>'
)
writeLines(render(tr("us_work_title"), work_body, has_tabs = TRUE), paste0("docs/pages/us-work", sfx))
cat("  Done\n")


# =====================================================
# US MARRIAGE — butterfly with spouse ethnicity
# =====================================================
cat("Building us-marriage...\n")

marriage_body <- paste0(
  '<div class="page-content">',
  {
    raw <- em$spouse_gender
    g1 <- raw %>% dplyr::filter(gen == "1st gen", !is.na(age_group))
    g1_pct <- round(sum(g1$PERWT[g1$spouse_iran == "Yes"]) / sum(g1$PERWT) * 100)
    g2 <- raw %>% dplyr::filter(gen == "2nd gen", !is.na(age_group))
    g2_iran <- round(sum(g2$PERWT[g2$spouse_iran == "Yes"]) / sum(g2$PERWT) * 100)
    g2_white <- round(sum(g2$PERWT[g2$spouse == "Other White"]) / sum(g2$PERWT) * 100)
    paste0(
      sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
  </div>', htxt(sprintf(tr("us_bignum_pct"), fa_num(g1_pct, 0))), htxt(tr("us_marriage_c1_primary"))),
      sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:10px auto 0; padding-left:18px; max-width:400px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>', htxt(sprintf(tr("us_bignum_pct"), fa_num(g2_iran, 0))), htxt(tr("us_marriage_c2_primary")),
        htxt(sprintf(tr("us_marriage_c2_b1"), fa_num(g2_white, 0))), tr("us_marriage_c2_b2"))
    )
  },
  '<div class="chart-card pc1">',
  '<div class="section-title">', tr("us_marriage_sec1"), '</div>',
  make_butterfly_marriage(sp, "1st gen", tr("us_gen1_label"), FALSE, "500px", "mar1"),
  '</div>',
  '<div class="chart-card pc2">',
  '<div class="section-title">', tr("us_marriage_sec2"), '</div>',
  make_butterfly_marriage(sp, "2nd gen", tr("us_gen2_label"), TRUE, "500px", "mar2"),
  '</div>',
  '</div>'
)
writeLines(render(tr("us_marriage_title"), marriage_body), paste0("docs/pages/us-marriage", sfx))
cat("  Done\n")


# =====================================================
# US INCOME — decile distribution by generation
# =====================================================
cat("Building us-income...\n")

income_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
    htxt(sprintf(tr("us_bignum_pct"), fa_num(round(fg_d10), 0))),
    htxt(tr("us_income_c1_primary")),
    htxt(tr("us_income_c1_b1")),
    htxt(sprintf(tr("us_income_c1_b2"), fa_num(round(fg_d1), 0)))),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
    htxt(sprintf(tr("us_bignum_pct"), fa_num(round(sg_d10), 0))),
    htxt(tr("us_income_c2_primary")),
    htxt(tr("us_income_c2_b1")),
    htxt(sprintf(tr("us_income_c2_b2"), fa_num(round(sg_d1), 0)))),
  '<div class="chart-card pc1">', make_income_chart(inc, "1st gen", tr("us_income_gen1"), "inc1"), '</div>',
  '<div class="chart-card pc2">', make_income_chart(inc, "2nd gen", tr("us_income_gen2"), "inc2"), '</div>',
  '</div>'
)
writeLines(render(tr("us_income_title"), income_body), paste0("docs/pages/us-income", sfx))
cat("  Done\n")


# =====================================================
# US LANGUAGE — Persian-language retention
# =====================================================
cat("Building us-language...\n")

p_lang_g1 <- make_lang_stack("1st generation", g1_cohorts, tr("us_lang_gen1"))
p_lang_g2 <- make_lang_stack("2nd generation", g2_cohorts, tr("us_lang_gen2"))
lang_leg_labels <- if (is_fa()) unname(US_LANGCAT_FA[names(lang_cat_colors)]) else names(lang_cat_colors)
lang_leg <- make_html_legend(lang_cat_colors, labels = lang_leg_labels, keys = names(lang_cat_colors))

# top non-Persian, non-English languages among 1st gen
lang_other_names <- if (is_fa()) unname(US_LANGOTHER_FA[lang_other$language]) else lang_other$language
lang_other_str <- paste(
  ifelse(lang_other$pct_of_1st_gen == 0,
    htxt(sprintf(tr("us_lang_other_lt1"), lang_other_names)),
    htxt(sprintf(tr("us_lang_other_pct"), lang_other_names, fa_num(lang_other$pct_of_1st_gen, 0)))),
  collapse = if (is_fa()) "، " else ", "
)

lang_body <- paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
    htxt(sprintf(tr("us_bignum_pct"), fa_num(g1_lpct$pct_persian, 0))),
    htxt(tr("us_lang_c1_primary")),
    htxt(sprintf(tr("us_lang_c1_b1"), fa_num(g1_lpct$pct_other, 0))),
    sprintf(tr("us_lang_c1_b2"), lang_other_str),
    htxt(sprintf(tr("us_lang_c1_b3"), fa_num(g1_lpct$pct_english_only, 0)))),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">%s</div>
    <ul style="margin:12px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%s</li>
      <li>%s</li>
    </ul>
  </div>',
    htxt(sprintf(tr("us_bignum_pct"), fa_num(g2_lpct$pct_persian, 0))),
    htxt(tr("us_lang_c2_primary")),
    htxt(sprintf(tr("us_lang_c2_b1"), fa_num(g2_lpct$pct_english_only, 0))),
    htxt(sprintf(tr("us_lang_c2_b2"), fa_num(g2_lpct$pct_other, 0)))),
  '<div class="chart-card pc1">',
  plotly_div("us-lang-g1", pj(p_lang_g1), "440px",
             source = SRC_LANG, legend_html = lang_leg, highlight_hover = TRUE),
  '</div>',
  '<div class="chart-card pc2">',
  plotly_div("us-lang-g2", pj(p_lang_g2), "440px",
             source = SRC_LANG, legend_html = lang_leg, highlight_hover = TRUE),
  '</div>',
  '</div>'
)
writeLines(render(tr("us_lang_title"), lang_body), paste0("docs/pages/us-language", sfx))
cat("  Done\n")


# =====================================================
# US POPULATION — headline, waterfall, region bar, maps
# =====================================================
cat("Building us-population...\n")

wf_comp_disp <- if (is_fa()) unname(US_WATERFALL_FA[wf$component]) else wf$component
wf$hover <- htxt(sprintf(tr("us_waterfall_hover"),
  wf_comp_disp, chk(wf$is_birth), chk(wf$is_ancestry), chk(wf$is_race), chk(wf$is_children),
  fmtv(wf$weighted_n), fa_num(wf$pct, 1), fmtv(wf$cumulative)))

p_waterfall <- plot_ly() %>%
  add_bars(x = wf$xnum, y = wf$weighted_n, base = wf$ymin,
    marker = list(color = wf_colors[seq_len(nrow(wf))]),
    text = wf$hover, hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = htxt(tr("us_waterfall_title")),
      font = list(size = 16, family = "Montserrat")),
    # fa: the RTL document mirrors the criteria_table() below (label column on
    # the RIGHT, criterion columns right-to-left), so mirror the bars to match:
    # reversed x range, y ticks on the right, l/r margins swapped. Same plot-
    # area geometry as EN, just flipped — columns align under the bars again.
    xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE,
      zeroline = FALSE, fixedrange = TRUE,
      range = if (is_fa()) c(nrow(wf) + 0.5, 0.5) else c(0.5, nrow(wf) + 0.5)),
    yaxis = c(list(title = "", tickformat = ",", fixedrange = TRUE),
      if (is_fa()) list(side = "right")),
    showlegend = FALSE,
    margin = list(t = 50, b = 6,
      l = if (is_fa()) 20 else US_LBL_PX, r = if (is_fa()) US_LBL_PX else 20),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

us_matrix_html <- criteria_table(list(
  list(label = tr("us_crit_birth"),    vals = wf$is_birth),
  list(label = tr("us_crit_ancestry"), vals = wf$is_ancestry),
  list(label = tr("us_crit_race"),     vals = wf$is_race),
  list(label = tr("us_crit_parent"),   vals = wf$is_children)),
  n_cols = nrow(wf), label_px = US_LBL_PX, sym_px = 15)

# fa: show the four Census regions in Persian (US_REGION_FA), English otherwise.
# Keep the desc-pop order (so the fixed marker-colour vector stays aligned) by
# reusing the same order for the display factor levels.
region_disp_names <- as.character(region_dat$REGION)
if (is_fa()) {
  m <- US_REGION_FA[region_disp_names]
  region_disp_names <- ifelse(is.na(m), region_disp_names, unname(m))
}
region_dat$REGION_disp <- factor(region_disp_names, levels = region_disp_names)
region_dat$hover <- htxt(sprintf(tr("us_region_hover"),
  region_disp_names, fmtv(region_dat$pop),
  fa_num(round(region_dat$pop / total_pop * 100), 0)))
p_region <- plot_ly(data = region_dat, x = ~REGION_disp, y = ~pop, type = "bar",
    marker = list(color = c("#7b5ea7", "#d4a943", "#2ca089", "#e07b54")),
    text = ~hover,
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = htxt(tr("us_region_title")),
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = ""), yaxis = list(title = "", tickformat = ","),
    margin = list(t = 60, b = 40),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# Population page — custom template with MapLibre GL JS
pop_page <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>', tr("us_pop_title"), '</title>
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap" rel="stylesheet">
<script src="lib/plotly-3.4.0.min.js"></script>
<link rel="stylesheet" href="lib/maplibre-gl-4.7.1.css">
<script src="lib/maplibre-gl-4.7.1.js"></script>
<script src="lib/pmtiles-3.2.1.js"></script>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family:"Montserrat",sans-serif; background:#fafafa; color:#333; padding:15px 40px; max-width:100%; overflow-x:hidden; }
.text-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.text-card { background:white; border-radius:8px; padding:20px; text-align:center;
  font-size:15px; line-height:1.6; border:1px solid #e0e0e0; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:hidden; min-width:0; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.source { font-size:12px; color:#666; text-align:right; padding:4px 0; margin-top:10px; }
.source a { color:#2774AE; }
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .text-row-4 { grid-template-columns:1fr 1fr; }
  .text-row, .text-row-4 { order:1; }
  .headline .number { font-size:28px; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
  .map-legend { font-size:10px; padding:6px 8px; }
}
@media (max-width:600px) {
  .text-row-4 { grid-template-columns:1fr !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .headline .number { font-size:24px; }
  .chart-card { padding:10px; }
  .map-legend { position:relative; top:auto; right:auto; margin-top:8px; box-shadow:none; border:1px solid #e0e0e0; }
}
.headline { background:white; border-radius:8px; padding:30px; text-align:center;
  border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em; }
a { transition: color 0.15s; }
a:hover { color: #1a4e72 !important; text-decoration: underline; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
.tab-bar { display:flex; justify-content:center; gap:0; margin:12px 0 0; }
.tab-btn { padding:6px 16px; border:1px solid #ddd; background:#f0f0f0; cursor:pointer;
  font-family:"Montserrat",sans-serif; font-size:13px; }
.tab-btn.active { background:#2774AE; color:white; font-weight:600; border-color:#2774AE; }
.tab-panel { display:none; }
.tab-panel.active { display:block; }
.map-container { position:relative; width:100%; }
.map-container .maplibregl-map { border-radius:4px; }
.map-popup { font-family:"Montserrat",sans-serif; font-size:13px; }
.map-popup b { color:#1a4e72; }
.map-legend { position:absolute; top:10px; right:10px; background:rgba(255,255,255,0.9); padding:8px 12px;
  border-radius:6px; font-size:12px; box-shadow:0 1px 4px rgba(0,0,0,0.15); z-index:1; line-height:1.8; }
.map-legend .leg-item { display:flex; align-items:center; gap:6px; }
.map-legend .leg-swatch { width:16px; height:16px; border-radius:2px; flex-shrink:0; }
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
</head>
<body>

<!-- Top row: headline + waterfall -->
<div class="chart-row">
<div class="headline">
  <div class="label">', tr("us_pop_headline_label"), '</div>
  <div class="number">794,915</div>
  <div class="label" style="margin-top:6px; font-size:13px; color:#555;">', tr("us_pop_caption"), '</div>
  <div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">
    <p style="margin-bottom:8px;">', tr("us_pop_idbox_intro"), '</p>
    <ul style="padding-left:20px; margin:0; line-height:1.5;">
      <li>', tr("us_pop_idbox_b1"), '</li>
      <li>', tr("us_pop_idbox_b2"), '</li>
      <li>', tr("us_pop_idbox_b3"), '</li>
      <li>', tr("us_pop_idbox_b4"), '</li>
    </ul>
  </div>
</div>
<div class="chart-card">', plotly_div("waterfall", pj(p_waterfall), "320px"),
us_matrix_html,
'<p style="font-size:11px; color:#666; text-align:right; margin:6px 0 0; padding-right:2px;">', SRC_POP_1YR, '</p>
</div>
</div>

<!-- Bottom row: region bar + map -->
<div class="chart-row">
<div class="chart-card">', plotly_div("region", pj(p_region), "400px", source = SRC_POP_5YR), '</div>
<div class="chart-card" style="margin-bottom:0; padding-bottom:8px;">
  <div class="section-title" style="margin-top:0;">', tr("us_geo_section"), '</div>
  <div class="tab-bar" style="margin-bottom:8px;">
    <button class="tab-btn active" data-btn-group="geo" onclick="switchGeoTab(\'geo-state\',this)">', tr("us_geo_tab_state"), '</button>
    <button class="tab-btn" data-btn-group="geo" onclick="switchGeoTab(\'geo-county\',this)">', tr("us_geo_tab_county"), '</button>
    <button class="tab-btn" data-btn-group="geo" onclick="switchGeoTab(\'geo-la\',this)">', tr("us_geo_tab_la"), '</button>
  </div>
  <div id="geo-state" class="tab-panel active" data-group="geo">
    <div class="map-container">
      <div id="state-map" style="width:100%;height:380px;"></div>
      <div class="map-legend">
        <div class="leg-item"><div class="leg-swatch" style="background:#c6dbef;"></div> 0 &ndash; 20,000</div>
        <div class="leg-item"><div class="leg-swatch" style="background:#6baed6;"></div> 20,000 &ndash; 100,000</div>
        <div class="leg-item"><div class="leg-swatch" style="background:#08306b;"></div> 100,000 &ndash; 300,000</div>
      </div>
    </div>
    <p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0;">', tr("us_geo_src_state"), '</p>
  </div>
  <div id="geo-county" class="tab-panel" data-group="geo">
    <div class="map-container">
      <div id="county-map" style="width:100%;height:380px;"></div>
      <div class="map-legend">
        <div class="leg-item"><div class="leg-swatch" style="background:#c6dbef;"></div> 0 &ndash; 5,000</div>
        <div class="leg-item"><div class="leg-swatch" style="background:#6baed6;"></div> 5,000 &ndash; 50,000</div>
        <div class="leg-item"><div class="leg-swatch" style="background:#08306b;"></div> 50,000 &ndash; 110,000</div>
      </div>
    </div>
    <p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0;">', tr("us_geo_src_county"), '</p>
  </div>
  <div id="geo-la" class="tab-panel" data-group="geo">
    <div class="map-container">
      <div id="la-map" style="width:100%;height:380px;"></div>
      <div class="map-legend">
        <div class="leg-item"><div class="leg-swatch" style="background:#e8e8e8;"></div> 0 &ndash; 500</div>
        <div class="leg-item"><div class="leg-swatch" style="background:#c6dbef;"></div> 500 &ndash; 1,000</div>
        <div class="leg-item"><div class="leg-swatch" style="background:#6baed6;"></div> 1,000 &ndash; 5,000</div>
        <div class="leg-item"><div class="leg-swatch" style="background:#2171b5;"></div> 5,000 &ndash; 15,000</div>
        <div class="leg-item"><div class="leg-swatch" style="background:#08306b;"></div> 15,000 &ndash; 30,000</div>
      </div>
    </div>
    <p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0;">', tr("us_geo_src_la"), '</p>
  </div>
</div><!-- end chart-card (map) -->
</div><!-- end chart-row -->

<script>
// PMTiles protocol
let protocol = new pmtiles.Protocol();
maplibregl.addProtocol("pmtiles", protocol.tile);

// Determine base URL for tiles (works for both local and GitHub Pages)
const base = window.location.href.replace(/\\/[^\\/]*$/, "/");

// --- State Map ---
const stateMap = new maplibregl.Map({
  container: "state-map",
  style: "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
  center: [-98.5, 39.8],
  zoom: window.innerWidth < 600 ? 2.1 : 2.8,
  minZoom: 2,
  maxZoom: 7,
  attributionControl: false,
  preserveDrawingBuffer: true
});
stateMap.addControl(new maplibregl.NavigationControl({showCompass:false}), "bottom-right");

stateMap.on("load", function() {
  stateMap.addSource("states-src", {
    type: "vector",
    url: "pmtiles://" + base + "tiles/us_states.pmtiles"
  });
  stateMap.addLayer({
    id: "states-fill",
    type: "fill",
    source: "states-src",
    "source-layer": "states",
    paint: {
      "fill-color": ["step", ["get", "population_est"],
        "#e8e8e8", 1, "#c6dbef", 20000, "#6baed6", 100000, "#08306b"],
      "fill-opacity": 0.85
    }
  });
  stateMap.addLayer({
    id: "states-line",
    type: "line",
    source: "states-src",
    "source-layer": "states",
    paint: { "line-color": "#fff", "line-width": 1 }
  });

  // Hover popup
  const popup = new maplibregl.Popup({ closeButton: false, closeOnClick: false, className: "map-popup" });
  stateMap.on("mousemove", "states-fill", function(e) {
    stateMap.getCanvas().style.cursor = "pointer";
    const f = e.features[0];
    const pop = Number(f.properties.population_est);
    const pct = (pop / 754595 * 100).toFixed(1);
    popup.setLngLat(e.lngLat)
      .setHTML("<b>" + f.properties.state + "</b><br>" + pop.toLocaleString() + "', tr("us_map_label_ia"), '" + pct + "', tr("us_map_pct_us"), '")
      .addTo(stateMap);
  });
  stateMap.on("mouseleave", "states-fill", function() {
    stateMap.getCanvas().style.cursor = "";
    popup.remove();
  });
});

// --- County Map ---
let countyMapInited = false;
let countyMap;

function initCountyMap() {
  if (countyMapInited) { countyMap.resize(); return; }
  countyMapInited = true;
  countyMap = new maplibregl.Map({
    container: "county-map",
    style: "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
    center: [-119.5, 37.5],
    zoom: 4.2,
    minZoom: 4,
    maxZoom: 10,
    attributionControl: false,
    preserveDrawingBuffer: true
  });
  countyMap.addControl(new maplibregl.NavigationControl({showCompass:false}), "bottom-right");

  countyMap.on("load", function() {
    countyMap.addSource("counties-src", {
      type: "vector",
      url: "pmtiles://" + base + "tiles/ca_counties.pmtiles"
    });
    countyMap.addLayer({
      id: "counties-fill",
      type: "fill",
      source: "counties-src",
      "source-layer": "counties",
      paint: {
        "fill-color": ["step", ["get", "pop_estimate"],
          "#e8e8e8", 1, "#c6dbef", 5000, "#6baed6", 25000, "#2171b5", 100000, "#08306b"],
        "fill-opacity": 0.85
      }
    });
    countyMap.addLayer({
      id: "counties-line",
      type: "line",
      source: "counties-src",
      "source-layer": "counties",
      paint: { "line-color": "#fff", "line-width": 1 }
    });

    const popup = new maplibregl.Popup({ closeButton: false, closeOnClick: false, className: "map-popup" });
    countyMap.on("mousemove", "counties-fill", function(e) {
      countyMap.getCanvas().style.cursor = "pointer";
      const f = e.features[0];
      const pop = Number(f.properties.pop_estimate);
      const pct = Number(f.properties.percent).toFixed(1);
      popup.setLngLat(e.lngLat)
        .setHTML("<b>" + f.properties.NAMELSAD + "</b><br>" +
          pop.toLocaleString() + "', tr("us_map_label_ia"), '" + pct + "', tr("us_map_pct_ca"), '")
        .addTo(countyMap);
    });
    countyMap.on("mouseleave", "counties-fill", function() {
      countyMap.getCanvas().style.cursor = "";
      popup.remove();
    });
  });
}

// Tab switching with lazy map init
// --- LA Neighborhood Map ---
let laMapInited = false;
let laMap;

function initLaMap() {
  if (laMapInited) { laMap.resize(); return; }
  laMapInited = true;
  laMap = new maplibregl.Map({
    container: "la-map",
    style: "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
    center: [-118.3, 34.05],
    zoom: 7.5,
    minZoom: 7,
    maxZoom: 13,
    attributionControl: false,
    preserveDrawingBuffer: true
  });
  laMap.addControl(new maplibregl.NavigationControl({showCompass:false}), "bottom-right");

  laMap.on("load", function() {
    laMap.addSource("pumas-src", {
      type: "vector",
      url: "pmtiles://" + base + "tiles/la_pumas.pmtiles"
    });
    laMap.addLayer({
      id: "pumas-fill",
      type: "fill",
      source: "pumas-src",
      "source-layer": "pumas",
      paint: {
        "fill-color": ["step", ["get", "total"],
          "#e8e8e8", 500, "#c6dbef", 1000, "#6baed6", 5000, "#2171b5", 15000, "#08306b"],
        "fill-opacity": 0.85
      }
    });
    laMap.addLayer({
      id: "pumas-line",
      type: "line",
      source: "pumas-src",
      "source-layer": "pumas",
      paint: { "line-color": "#fff", "line-width": 0.5 }
    });

    const popup = new maplibregl.Popup({ closeButton: false, closeOnClick: false, className: "map-popup" });
    laMap.on("mousemove", "pumas-fill", function(e) {
      laMap.getCanvas().style.cursor = "pointer";
      const f = e.features[0];
      const pop = Number(f.properties.total);
      const name = f.properties.name || "PUMA " + f.properties.puma_id;
      const laPct = (pop / 353221 * 100).toFixed(1);
      popup.setLngLat(e.lngLat)
        .setHTML("<b>" + name + "</b><br>" +
          pop.toLocaleString() + "', tr("us_map_label_ia"), '" + laPct + "', tr("us_map_pct_ca"), '")
        .addTo(laMap);
    });
    laMap.on("mouseleave", "pumas-fill", function() {
      laMap.getCanvas().style.cursor = "";
      popup.remove();
    });
  });
}

// Tab switching with lazy map init
function switchGeoTab(tabId, btn) {
  document.querySelectorAll("[data-group=\\"geo\\"]").forEach(function(e){e.classList.remove("active");});
  document.querySelectorAll("[data-btn-group=\\"geo\\"]").forEach(function(e){e.classList.remove("active");});
  document.getElementById(tabId).classList.add("active");
  btn.classList.add("active");
  if (tabId === "geo-county") setTimeout(initCountyMap, 50);
  if (tabId === "geo-la") setTimeout(initLaMap, 50);
  if (tabId === "geo-state") stateMap.resize();
}
</script>
', iframe_resize_script, '
</body>
</html>')

if (is_fa()) pop_page <- fa_shell(pop_page)
writeLines(pop_page, paste0("docs/pages/us-population", sfx))
cat("  Done\n")

}  # end for (LANG)

cat("\nAll US pages rebuilt (en + fa).\n")

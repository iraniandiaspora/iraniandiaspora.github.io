# Master build: generate all US chart JSON and assemble us-*.html pages
# Each page = hand-crafted HTML with Plotly.newPlot() calls
# Run from the iraniandiaspora.github.io/ directory:
#   Rscript R/build_us.R

library(plotly)
library(dplyr)
library(readr)
library(jsonlite)

DATA_DIR <- "data/us"

# Shared helpers: strip_internal_classes(), plotly_to_json(), plotly_div(),
# iframe_resize_script, MAPBOX_ATTRIB_HIDE_CSS.
source("R/_helpers.R")
# Generate a horizontal HTML legend from named color vector
# break_after: index after which to insert a line break (e.g., 3 means break after 3rd item)
make_html_legend <- function(colors, labels = names(colors), break_after = NULL) {
  items <- mapply(function(col, lab) {
    html_lab <- gsub("&", "&amp;", lab)
    sprintf('<span data-lg="%s" style="display:inline-flex; align-items:center; gap:4px; margin-right:14px; cursor:pointer; transition:opacity 0.2s;" onmouseenter="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOn)el.__hlOn(this.getAttribute(\'data-lg\'));" onmouseleave="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOff)el.__hlOff();"><span style="width:12px; height:12px; background:%s; border-radius:2px; display:inline-block;"></span> %s</span>',
      lab, col, html_lab)
  }, colors, labels, SIMPLIFY = TRUE)
  if (!is.null(break_after) && break_after < length(items)) {
    items <- c(items[1:break_after], "<br>", items[(break_after+1):length(items)])
  }
  sprintf('<div style="text-align:center; font-size:12px; color:#444; margin:6px 0 2px; line-height:2;">%s</div>',
    paste(items, collapse = ""))
}

IPUMS_LINK <- "<a href='https://doi.org/10.18128/D010.V16.0' target='_blank' style='color:#2774AE;'>IPUMS USA</a>"
ACS_LINK <- "<a href='https://www.census.gov/programs-surveys/acs/microdata.html' target='_blank' style='color:#2774AE;'>U.S. Census Bureau</a>"
SRC_POP_1YR <- paste0("Source: ", IPUMS_LINK, " \u2014 ACS 2024 1-Year<br>",
  "Weighted estimates produced by the Census Bureau from an annual survey of about 3.5 million U.S. households.")
SRC_POP_5YR <- paste0("Source: ", ACS_LINK, " \u2014 ACS 2020\u20132024 5-Year PUMS<br>",
  "Pools five annual surveys for more reliable state- and metro-level estimates.")
SRC_IMMIG <- paste0("Source: ", ACS_LINK, " \u2014 ACS 2020\u20132024 5-Year PUMS<br>",
  "Iran-born respondents only.<br>Year of immigration is self-reported and may reflect most recent entry.")
SRC_CITIZEN <- paste0("Source: ", ACS_LINK, " \u2014 ACS 2020\u20132024 5-Year PUMS<br>",
  "Includes birthplace, ancestry, and parental origin.")
SRC_MARRIAGE <- paste0("Source: ", ACS_LINK, " \u2014 ACS 2020\u20132024 5-Year PUMS<br>",
  "Currently married or partnered Iranian-Americans.<br>Spouse ethnicity based on the spouse\u2019s own census responses.")
SRC_EDUC <- paste0("Source: ", ACS_LINK, " \u2014 ACS 2020\u20132024 5-Year PUMS<br>",
  "Ages 25+, when most have completed their education.")
SRC_WORK <- paste0("Source: ", ACS_LINK, " \u2014 ACS 2020\u20132024 5-Year PUMS<br>",
  "Employment type reflects primary job held in the past year.")
SRC_INCOME <- paste0("Source: ", ACS_LINK, " \u2014 ACS 2020\u20132024 5-Year PUMS<br>",
  "Ages 25\u201354 (prime working years).<br>Each decile holds 10% of all U.S. households, ranked by pre-tax household income.")

page_template <- function(title, body_html) {
  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>', title, '</title>
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap" rel="stylesheet">
<script src="lib/plotly-3.4.0.min.js"></script>
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
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }
@media (max-width:900px) {
  body { padding:10px 15px; display:flex; flex-direction:column; }
  .text-row, .chart-row { grid-template-columns:1fr !important; }
  .text-row-4 { grid-template-columns:1fr 1fr; }
  .text-row, .text-row-4 { order:1; } /* push text below charts on mobile */
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .headline .number { font-size:34px; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
}
@media (max-width:600px) {
  .text-row-4 { grid-template-columns:1fr !important; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .text-card { font-size:13px; padding:14px; }
  .headline .number { font-size:28px; }
  .chart-card { padding:10px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
</head>
<body>
', body_html, '
', iframe_resize_script, '
</body>
</html>')
}

# =====================================================
# US IMMIGRATION & CITIZENSHIP (ACS data — original page)
# =====================================================
cat("Building us-immigration...\n")

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

# Weighted median year of arrival for the first-generation Iran-born
# population. Computed dynamically so the text card always names the
# correct "half arrived in or after year X" boundary. Previously the
# card hard-coded "over 50 percent arriving after 1994" which became
# stale as the distribution shifted forward (the current median is
# 1999 under 2020-2024 5-year pool).
fg_yrim <- iran_data %>%
  filter(gen == "1st gen", !is.na(YRIMMIG)) %>%
  mutate(YRIMMIG_num = suppressWarnings(as.numeric(YRIMMIG))) %>%
  filter(!is.na(YRIMMIG_num), YRIMMIG_num > 0) %>%
  arrange(YRIMMIG_num)
fg_yrim$cum <- cumsum(fg_yrim$PERWT) / sum(fg_yrim$PERWT, na.rm = TRUE)
fg_median_year <- fg_yrim$YRIMMIG_num[which(fg_yrim$cum >= 0.5)[1]]

p_immig <- plot_ly() %>%
  add_bars(data = immig, x = ~YRIMMIG, y = ~n, marker = list(color = "#2774AE"),
    text = ~sprintf("<b>Year:</b> %d<br><b>Arrivals:</b> %s<br><b>Cumulative:</b> %s (%s%%)",
      YRIMMIG, format(n, big.mark = ","), format(cumulative_immigrants, big.mark = ","), cum_pct),
    hoverinfo = "text", showlegend = FALSE) %>%
  add_lines(data = immig, x = ~YRIMMIG,
    y = ~cumulative_immigrants / max(cumulative_immigrants) * max(n),
    yaxis = "y2", line = list(color = "lightblue", width = 2),
    text = ~sprintf("<b>By %d:</b><br>%s arrivals<br>(%s%% of total)",
      YRIMMIG, format(cumulative_immigrants, big.mark = ","), cum_pct),
    hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iranian Migration to the US:<br>Annual Arrivals and Cumulative Trends</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = ""), yaxis = list(title = "", tickformat = ","),
    yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
      range = c(0, max(immig$n) * 1.05),
      tickvals = round(seq(0, max(immig$n), length.out = 5)),
      ticktext = c("0%", "25%", "50%", "75%", "100%"),
      tickfont = list(size = 11)),
    margin = list(t = 55, b = 60, r = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

cit_order <- c("Naturalized citizen", "Born in the US", "Not a citizen")
citizen$CITIZEN2 <- factor(citizen$CITIZEN2, levels = cit_order)
citizen <- citizen[order(citizen$CITIZEN2), ]

cit_total <- sum(citizen$n)
citizen$pct <- round(citizen$n / cit_total * 100)
pct_nat    <- citizen$pct[citizen$CITIZEN2 == "Naturalized citizen"]
pct_usborn <- citizen$pct[citizen$CITIZEN2 == "Born in the US"]
pct_noncit <- citizen$pct[citizen$CITIZEN2 == "Not a citizen"]

p_citizen <- plot_ly(data = citizen, x = ~CITIZEN2, y = ~n, type = "bar",
    marker = list(color = c("#2774AE", "#5a9bd5", "#e07b54")),
    text = ~sprintf("<b>%s</b><br>%s (%s%%)", CITIZEN2,
      format(n, big.mark = ","), round(n / cit_total * 100)),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Americans by<br>Citizenship Status</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = ""), yaxis = list(title = "", tickformat = ","),
    margin = list(t = 55, b = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

writeLines(page_template("Immigration & Citizenship", paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">Half of first-generation Iranian-Americans in the U.S. arrived in %d or later.</div>
  </div>', fg_median_year, fg_median_year),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">Nearly 9 in 10</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">Iranian-Americans are U.S. citizens &mdash; %d%% naturalized and %d%% born in the United States.</div>
  </div>', pct_nat, pct_usborn),
  '<div class="chart-card pc1">', plotly_div("immig", plotly_to_json(p_immig), "430px", source = SRC_IMMIG), '</div>',
  '<div class="chart-card pc2">', plotly_div("citizen", plotly_to_json(p_citizen), source = SRC_CITIZEN), '</div>',
  '</div>'
)), "docs/pages/us-immigration.html")
cat("  Done\n")


# =====================================================
# US ADMISSIONS HISTORY (INS/DHS official records, 1970-2023)
# =====================================================
cat("Building us-admissions...\n")

# Official admissions from INS Annual Reports (1970-1977) + INS/DHS yearbooks (1978-2023)
lpr <- read_csv(file.path(DATA_DIR, "iran_lpr_1970_2023.csv"), show_col_types = FALSE)
lpr[is.na(lpr)] <- 0

SRC_LPR <- "Source: INS Annual Reports (1970\u20131977); INS Statistical Yearbooks (1978\u20132004); DHS Yearbook of Immigration Statistics (2005\u20132023)"

# --- Chart 1: Total admissions line chart (1978-2023) ---
lpr$hover_total <- sprintf("<b>%d</b><br>Granted: %s",
  lpr$year, format(lpr$total, big.mark = ","))

p_lpr_total <- plot_ly(data = lpr, x = ~year, y = ~total,
    type = "scatter", mode = "lines+markers",
    line = list(color = "#1a4e72", width = 2),
    marker = list(color = "#1a4e72", size = 3),
    text = ~hover_total, hoverinfo = "text", showlegend = FALSE) %>%
  layout(
    title = list(text = "<b>Iranians Granted<br>US Permanent Resident Status,<br>1970\u20132023</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5, range = c(1968.5, 2024.5),
      tickfont = list(size = 11)),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero",
      tickfont = list(size = 11)),
    margin = list(t = 75, b = 40),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Chart 2: Stacked bar by category (full period 1978-2023) ---
# Stacked bars instead of stacked area for reliable hover behavior.
# Pre-1992: employment is 0 (included in family), diversity is 0
# Post-1992: all 5 categories separated
lpr_cat <- lpr

# Stack order: Family, Employment (light blue, adjacent to family),
# Refugee/Asylee, Diversity, Other.
# Employment is light blue so readers can see that pre-1992 family
# (which included employment) visually matches family + employment post-1992.
cat_names  <- c("Family", "Employment", "Refugee/Asylee", "Diversity", "Other")
cat_cols   <- c("family", "employment", "refugee_asylee", "diversity", "other")
cat_colors <- c("Family" = "#2774AE", "Employment" = "#8bbdde",
                "Refugee/Asylee" = "#c0504d", "Diversity" = "#d4a943",
                "Other" = "#999999")
cat_defs   <- c("Family" = "immediate relatives and family preferences",
                "Employment" = "employment-based immigrants and family",
                "Refugee/Asylee" = "adjusting to permanent residence",
                "Diversity" = "Diversity Visa program",
                "Other" = "special immigrants and other categories")

# Compute cumulative bases so we can add traces in REVERSE order
# (for tooltip: Other at top, Family at bottom) while keeping the
# visual stack in FORWARD order (Family at bottom, Other at top).
base_family   <- rep(0, nrow(lpr_cat))
base_employ   <- lpr_cat$family
base_refugee  <- lpr_cat$family + lpr_cat$employment
base_divers   <- lpr_cat$family + lpr_cat$employment + lpr_cat$refugee_asylee
base_other    <- lpr_cat$family + lpr_cat$employment + lpr_cat$refugee_asylee + lpr_cat$diversity
cat_bases <- list(family = base_family, employment = base_employ,
                  refugee_asylee = base_refugee, diversity = base_divers,
                  other = base_other)

p_lpr_cat <- plot_ly()
for (i in rev(seq_along(cat_names))) {
  vals <- lpr_cat[[cat_cols[i]]]
  pre78 <- lpr_cat$year < 1978
  pre92 <- lpr_cat$year < 1992 & !pre78
  hover <- ifelse(pre78 & cat_cols[i] == "family",
    sprintf("<b>Total</b> (categories not reported separately): %s",
      format(vals, big.mark = ",")),
    ifelse(pre92 & cat_cols[i] == "family",
      sprintf("<b>Family + Employment</b> (combined in source): %s",
        format(vals, big.mark = ",")),
      sprintf("<b>%s</b>: %s",
        cat_names[i], format(vals, big.mark = ","))))
  p_lpr_cat <- p_lpr_cat %>%
    add_bars(x = lpr_cat$year, y = vals,
      base = cat_bases[[cat_cols[i]]],
      marker = list(color = cat_colors[cat_names[i]],
                    line = list(width = 0)),
      name = cat_names[i],
      legendgroup = cat_names[i],
      showlegend = FALSE,
      text = hover, hovertemplate = "%{text}<extra></extra>",
      textposition = "none")
}
p_lpr_cat <- p_lpr_cat %>%
  layout(
    barmode = "overlay",
    hovermode = "x unified",
    title = list(text = "<b>Iranian US Permanent Residence Grants<br>by Category, 1970\u20132023</b>",
      font = list(size = 15, family = "Montserrat")),
    xaxis = list(title = "", dtick = 5, range = c(1968.5, 2024.5),
      tickfont = list(size = 11)),
    yaxis = list(title = "", tickformat = ",", rangemode = "tozero",
      tickfont = list(size = 11)),
    annotations = list(
      list(x = 0.5, y = -0.12, xref = "paper", yref = "paper",
           text = "1970\u20131977: total grants only (no category breakdown in source). 1978\u20131991: family and employment combined in published tabulations.",
           showarrow = FALSE, font = list(size = 9, color = "#888"),
           xanchor = "center")
    ),
    margin = list(t = 55, b = 55),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

cat_leg <- make_html_legend(cat_colors)

writeLines(page_template("US: Immigration History", paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%s</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">Iranians granted U.S. permanent residence between 1970 and 2023.</div>
    <div style="font-size:15px; font-weight:700; color:#1a4e72; line-height:1.45; margin-top:16px;">About these figures</div>
    <ul style="margin:8px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>Counts green cards granted, not the current Iran-born population</li>
      <li>Five decades of grants &mdash; includes people who later died, returned, or moved elsewhere</li>
      <li>Many recipients first entered on temporary visas (student, work) before adjusting</li>
    </ul>
  </div>',
    format(sum(lpr$total), big.mark = ",")),
  '<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:15px; font-weight:700; color:#1a4e72; line-height:1.45;">Green card pathways</div>
    <ul style="margin:10px auto 0; padding-left:18px; max-width:420px; text-align:left; font-size:13.5px; color:#555; line-height:1.6;">
      <li><b>Family</b> \u2014 immediate relatives or family sponsorship</li>
      <li><b>Employment</b> \u2014 employer or skill-based admission</li>
      <li><b>Refugee/Asylee</b> \u2014 adjusting from refugee or asylee status</li>
      <li><b>Diversity</b> \u2014 annual visa lottery for countries with low U.S. immigration</li>
    </ul>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Family has been the largest category throughout; the 1989\u201391 spike reflects a 1986 law that granted permanent residence to many previously undocumented immigrants.</div>
  </div>',
  '<div class="chart-card pc1">', plotly_div("lpr-total", plotly_to_json(p_lpr_total), "450px", source = SRC_LPR), '</div>',
  '<div class="chart-card pc2">', plotly_div("lpr-cat", plotly_to_json(p_lpr_cat), "450px", source = SRC_LPR, legend_html = cat_leg, highlight_hover = TRUE),
  '<script>(function(){var el=document.getElementById("lpr-cat");if(el){el.removeAllListeners("plotly_hover");el.removeAllListeners("plotly_unhover");el.removeAllListeners("plotly_click");}})();</script>',
  '</div>',
  '</div>'
)), "docs/pages/us-admissions.html")
cat("  Done\n")


# =====================================================
# BUTTERFLY HELPER: Education
# True butterfly: men LEFT (negative x), women RIGHT (positive x)
# =====================================================
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
  # Aggregate after collapsing both education and age labels
  df <- df %>%
    group_by(educ_factor, age_group) %>%
    summarize(Male = sum(Male, na.rm = TRUE), Female = sum(Female, na.rm = TRUE), .groups = "drop")

  age_levels <- if (age_collapse) c("25-34", "35-44", "45+") else c("25-34", "35-44", "45-54", "55-64", "65-74", "75-84")
  df$age_group <- factor(df$age_group, levels = age_levels)

  educ_levels <- c("Less than BA degree", "BA degree", "Graduate degree")
  df$educ_factor <- factor(df$educ_factor, levels = educ_levels)
  colors <- c("Less than BA degree" = "#1a4e72", "BA degree" = "#2774AE", "Graduate degree" = "#8bbdde")

  # Compute percentages within age group
  df <- df %>%
    group_by(age_group) %>%
    mutate(male_pct = round(Male / sum(Male) * 100, 1),
           female_pct = round(Female / sum(Female) * 100, 1)) %>%
    ungroup()

  # Men panel (reversed x-axis so bars go left)
  p_men <- plot_ly()
  for (ed in educ_levels) {
    sub <- df %>% filter(educ_factor == ed)
    hover_texts <- sprintf("<b>%s</b><br>%s, Men<br>%.1f%%", ed, gen_label, sub$male_pct)
    p_men <- p_men %>%
      add_bars(data = sub, y = ~age_group, x = ~male_pct, name = ed,
        marker = list(color = colors[ed]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = ed, showlegend = TRUE, orientation = "h")
  }
  p_men <- p_men %>% layout(
    barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  # Women panel
  p_women <- plot_ly()
  for (ed in educ_levels) {
    sub <- df %>% filter(educ_factor == ed)
    hover_texts <- sprintf("<b>%s</b><br>%s, Women<br>%.1f%%", ed, gen_label, sub$female_pct)
    p_women <- p_women %>%
      add_bars(data = sub, y = ~age_group, x = ~female_pct, name = ed,
        marker = list(color = colors[ed]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = ed, showlegend = FALSE, orientation = "h")
  }
  p_women <- p_women %>% layout(
    barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  # Combine as subplot
  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = "Men", x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = "Women", x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg <- make_html_legend(colors, break_after = 3)
  plotly_div(id_prefix, plotly_to_json(p), height, source = SRC_EDUC, legend_html = leg, highlight_hover = TRUE)
}


# =====================================================
# US EDUCATION
# =====================================================
cat("Building us-education...\n")

e1 <- list(gen_1_wide = read_csv(file.path(DATA_DIR, "gen_1_wide.csv"), show_col_types = FALSE))
e2 <- list(gen_2_wide = read_csv(file.path(DATA_DIR, "gen_2_wide.csv"), show_col_types = FALSE))

{
  bap_share <- function(df, age, gender) {
    sub <- df %>% filter(age_group == age)
    tot <- sum(sub[[gender]])
    sum(sub[[gender]][sub$educ_factor %in% c("BA degree","Graduate degree")]) / tot * 100
  }
  g1_young_f <- round(bap_share(e1$gen_1_wide, "25-34", "Female"))
  g1_young_m <- round(bap_share(e1$gen_1_wide, "25-34", "Male"))
  g1_old_f   <- round(bap_share(e1$gen_1_wide, "75-84", "Female"))
  g1_old_m   <- round(bap_share(e1$gen_1_wide, "75-84", "Male"))
  g2_mid_f   <- round(bap_share(e2$gen_2_wide, "35-44", "Female"))
  g2_mid_m   <- round(bap_share(e2$gen_2_wide, "35-44", "Male"))
  NULL
}
writeLines(page_template("Education", paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian-American women ages 25&ndash;34 hold a bachelor&rsquo;s degree or higher &mdash; now ahead of men (%d%%).</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Among older first-generation (ages 75&ndash;84): %d%% of men hold a BA+ vs only %d%% of women.</div>
  </div>', g1_young_f, g1_young_m, g1_old_m, g1_old_f),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of second-generation Iranian-American women ages 35&ndash;44 hold a bachelor&rsquo;s degree or higher &mdash; vs %d%% of men.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Both generations now show women outpacing men in higher education.</div>
  </div>', g2_mid_f, g2_mid_m),
  '<div class="chart-card pc1">',
  '<div class="section-title">Educational Attainment of Iranian-Americans: First Generation</div>',
  make_butterfly_educ(e1$gen_1_wide, "1st Generation", FALSE, "500px", "ed1"),
  '</div>',
  '<div class="chart-card pc2">',
  '<div class="section-title">Educational Attainment of Iranian-Americans: Second Generation</div>',
  make_butterfly_educ(e2$gen_2_wide, "2nd Generation", TRUE, "500px", "ed2"),
  '</div>',
  '</div>'
)), "docs/pages/us-education.html")
cat("  Done\n")


# =====================================================
# BUTTERFLY HELPER: Work
# =====================================================
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
  # Categorical palette — no dark blue at center (blocks black hover border)
  colors <- c("Private sector employee" = "#5a9bd5", "Public sector employee" = "#4a8c6f",
    "Non-profit employee" = "#c4793a", "Self-employed" = "#d4a943",
    "No work in last 5 years" = "#b0b0b0")

  # Men panel (reversed x)
  p_men <- plot_ly()
  for (cat_name in cat_order) {
    sub_m <- d_m %>% filter(class_wkrd == cat_name)
    if (nrow(sub_m) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Men<br>%.1f%%", cat_name, gen_label, sub_m$pct)
      p_men <- p_men %>% add_bars(data = sub_m, y = ~age_group, x = ~pct, name = cat_name,
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_name, showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  # Women panel
  p_women <- plot_ly()
  for (cat_name in cat_order) {
    sub_f <- d_f %>% filter(class_wkrd == cat_name)
    if (nrow(sub_f) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Women<br>%.1f%%", cat_name, gen_label, sub_f$pct)
      p_women <- p_women %>% add_bars(data = sub_f, y = ~age_group, x = ~pct, name = cat_name,
        marker = list(color = colors[cat_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = cat_name, showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = "Men", x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = "Women", x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg <- make_html_legend(colors, break_after = 3)
  plotly_div(id_prefix, plotly_to_json(p), height, source = SRC_WORK, legend_html = leg, highlight_hover = TRUE)
}


# =====================================================
# US WORK
# =====================================================
cat("Building us-work...\n")

ec <- list(class = read_csv(file.path(DATA_DIR, "class.csv"), show_col_types = FALSE))

# Compute gen x gender x sector shares for factoid cards
work_share <- function(df, g, gen_) {
  sub <- df %>% filter(class_wkrd != "N/A", class_wkrd != "Unpaid family member", gen == gen_, gender == g)
  tot <- sum(sub$n)
  list(
    self = round(sum(sub$n[sub$class_wkrd == "Self-employed"]) / tot * 100),
    np   = round(sum(sub$n[sub$class_wkrd == "Non-profit employee"]) / tot * 100),
    priv = round(sum(sub$n[sub$class_wkrd == "Private sector employee"]) / tot * 100)
  )
}
g1_m <- work_share(ec$class, "Male", "1st gen")
g1_f <- work_share(ec$class, "Female", "1st gen")
g2_m <- work_share(ec$class, "Male", "2nd gen")
g2_f <- work_share(ec$class, "Female", "2nd gen")

writeLines(page_template("Work", paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian-American men are self-employed &mdash; nearly double the rate for women (%d%%).</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Women are roughly twice as likely as men to work in non-profits (%d%% vs %d%%).</div>
  </div>', g1_m$self, g1_f$self, g1_f$np, g1_m$np),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of second-generation Iranian-American men work in the private sector &mdash; up from %d%% in the first generation.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Self-employment falls to %d%% for men and %d%% for women; the gender gap narrows.</div>
  </div>', g2_m$priv, g1_m$priv, g2_m$self, g2_f$self),
  '<div class="chart-card pc1">',
  '<div class="section-title">Employment of Iranian-Americans: First Generation</div>',
  make_butterfly_work(ec$class, "1st gen", "1st Generation", FALSE, "500px", "wk1"),
  '</div>',
  '<div class="chart-card pc2">',
  '<div class="section-title">Employment of Iranian-Americans: Second Generation</div>',
  make_butterfly_work(ec$class, "2nd gen", "2nd Generation", TRUE, "500px", "wk2"),
  '</div>',
  '</div>'
)), "docs/pages/us-work.html")
cat("  Done\n")

# =====================================================
# US MARRIAGE — butterfly with spouse ethnicity
# =====================================================
cat("Building us-marriage...\n")

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

  # Aggregate by spouse category, age_group, gender
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
  # Categorical palette — Iranian green so black border is visible against it
  colors <- c("Iranian" = "#4a8c6f",
              "Middle Eastern (Non-Iranian)" = "#1a4e72",
              "White (Non-Iranian, Non-Hispanic)" = "#d4a943",
              "Hispanic" = "#c4793a",
              "Asian, Black, Native American" = "#7b5ea7")

  p_men <- plot_ly()
  for (sp_name in sp_order) {
    sub <- d_m %>% filter(spouse == sp_name)
    if (nrow(sub) > 0) {
      # Pre-compute hover strings to avoid plotly trace mixup
      hover_texts <- sprintf("<b>%s</b><br>%s, Men, %s<br>Estimated: %s<br>%.1f%%",
        sp_name, gen_label, sub$age_group, format(round(sub$n), big.mark = ","), sub$pct)
      p_men <- p_men %>% add_bars(data = sub, y = ~age_group, x = ~pct, name = sp_name,
        marker = list(color = colors[sp_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = sp_name, showlegend = TRUE, orientation = "h")
    }
  }
  p_men <- p_men %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(105, 0), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = TRUE, categoryorder = "array", categoryarray = age_levels))

  p_women <- plot_ly()
  for (sp_name in sp_order) {
    sub <- d_f %>% filter(spouse == sp_name)
    if (nrow(sub) > 0) {
      hover_texts <- sprintf("<b>%s</b><br>%s, Women, %s<br>Estimated: %s<br>%.1f%%",
        sp_name, gen_label, sub$age_group, format(round(sub$n), big.mark = ","), sub$pct)
      p_women <- p_women %>% add_bars(data = sub, y = ~age_group, x = ~pct, name = sp_name,
        marker = list(color = colors[sp_name]), textposition = "none",
        hovertext = hover_texts, hoverinfo = "text",
        legendgroup = sp_name, showlegend = FALSE, orientation = "h")
    }
  }
  p_women <- p_women %>% layout(barmode = "stack",
    xaxis = list(title = "", range = c(0, 105), ticksuffix = "%"),
    yaxis = list(title = "", showticklabels = FALSE, categoryorder = "array", categoryarray = age_levels))

  p <- subplot(p_men, p_women, shareY = TRUE, titleX = TRUE, margin = 0) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(showarrow = FALSE),
      annotations = list(
        list(text = "Men", x = 0.22, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555")),
        list(text = "Women", x = 0.78, y = 1.08, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14, family = "Montserrat", color = "#555"))
      ),
      margin = list(l = 60, r = 20, t = 50, b = 40),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)

  leg <- make_html_legend(colors, break_after = 3)
  plotly_div(id_prefix, plotly_to_json(p), height, source = SRC_MARRIAGE, legend_html = leg, highlight_hover = TRUE)
}

writeLines(page_template("Marriage", paste0(
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
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian-Americans in partnerships have an Iranian partner.</div>
  </div>', g1_pct),
      sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of second-generation Iranian-Americans in partnerships have an Iranian partner.</div>
    <ul style="margin:10px auto 0; padding-left:18px; max-width:400px; text-align:left; font-size:13.5px; color:#555; line-height:1.55;">
      <li>%d%% partner with White, non-Hispanic Americans</li>
      <li>Similar to <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC8112448/" target="_blank" style="color:#2774AE; text-decoration:none; border-bottom:1px solid rgba(39,116,174,0.4);">other second-generation Asian Americans</a></li>
    </ul>
  </div>', g2_iran, g2_white)
    )
  },
  '<div class="chart-card pc1">',
  '<div class="section-title">Spouse/Partner Ethnicity of Iranian-Americans: First Generation</div>',
  make_butterfly_marriage(sp, "1st gen", "1st Generation", FALSE, "500px", "mar1"),
  '</div>',
  '<div class="chart-card pc2">',
  '<div class="section-title">Spouse/Partner Ethnicity of Iranian-Americans: Second Generation</div>',
  make_butterfly_marriage(sp, "2nd gen", "2nd Generation", TRUE, "500px", "mar2"),
  '</div>',
  '</div>'
)), "docs/pages/us-marriage.html")
cat("  Done\n")


# =====================================================
# US INCOME — decile distribution by generation
# =====================================================
cat("Building us-income...\n")

iran <- read_csv(file.path(DATA_DIR, "iran_data.csv"), show_col_types = FALSE)

# National decile thresholds — household-level (see
# pull_acs_5yr_2020_2024.R). The reference distribution is one row per
# household whose reference person is aged 25-54, weighted by WGTP.
pctiles <- read_csv(file.path(DATA_DIR, "national_reference.csv"), show_col_types = FALSE)
breaks <- c(-Inf, pctiles$p10, pctiles$p20, pctiles$p30, pctiles$p40,
            pctiles$p50, pctiles$p60, pctiles$p70, pctiles$p80, pctiles$p90, Inf)

# Iranian side: one row per Iranian household (by SERIAL) with at least
# one prime-age adult (25-54) of compound-Iranian identification.
# Previously this filtered to all prime-age persons and summed PERWT,
# which multi-counted households with multiple prime-age adults. The
# household-level approach matches how the national reference is built
# and makes the comparison internally consistent. Second-generation
# children living with 1st-gen parents inherit the parent's household.
inc <- iran %>%
  filter(AGE >= 25 & AGE <= 54 & !is.na(HHINCOME)) %>%
  # Prefer a 1st-gen row when a household has multiple prime-age Iranian
  # members (the sort puts 1st-gen first so slice(1) picks it). Then dedupe.
  mutate(gen_rank = ifelse(generation == "1st gen", 0L, 1L)) %>%
  arrange(SERIAL, gen_rank, AGE) %>%
  group_by(SERIAL) %>%
  slice(1) %>%
  ungroup() %>%
  select(-gen_rank) %>%
  mutate(decile = cut(HHINCOME, breaks = breaks, labels = paste0("D", 1:10),
                      include.lowest = TRUE, right = TRUE))

make_income_chart <- function(df, gen_val, gen_label, id_prefix) {
  d <- df %>% filter(gen == gen_val) %>%
    group_by(decile) %>%
    summarize(n = n(), weighted = sum(HHWT, na.rm = TRUE), .groups = "drop") %>%
    mutate(share = round(weighted / sum(weighted) * 100, 1))

  decile_labels <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
  d$label <- decile_labels[as.numeric(gsub("D", "", d$decile))]
  d$label <- factor(d$label, levels = decile_labels)

  p <- plot_ly(data = d, x = ~label, y = ~share, type = "scatter", mode = "markers+lines",
    marker = list(color = "#4A90D9", size = 8),
    line = list(color = "#4A90D9", width = 1),
    text = ~sprintf("<b>Decile:</b> %s<br><b>Sample size (n):</b> %s<br><b>Share:</b> %.1f%%",
      label, format(n, big.mark = ","), share),
    hoverinfo = "text", textposition = "none") %>%
    # Shaded area
    add_trace(x = ~label, y = ~share, type = "scatter", mode = "none",
      fill = "tozeroy", fillcolor = "rgba(173,216,230,0.3)",
      marker = list(size = 0, opacity = 0),
      hoverinfo = "skip", showlegend = FALSE) %>%
    # 10% reference line (national baseline — if no difference, each decile = 10%)
    add_trace(x = decile_labels, y = rep(10, 10), type = "scatter", mode = "lines",
      line = list(color = "#cc0000", width = 1.5, dash = "dot"),
      marker = list(size = 0, opacity = 0),
      hoverinfo = "skip", showlegend = FALSE) %>%
    layout(
      title = list(text = sprintf("<b>Position in U.S.<br>Household Income Distribution:<br>%s (Ages 25-54)</b>", gen_label),
        font = list(size = 15, family = "Montserrat")),
      xaxis = list(title = "Income Decile (Lowest to Highest)", titlefont = list(size = 11),
        categoryorder = "array", categoryarray = decile_labels),
      yaxis = list(title = "", ticksuffix = "%", range = c(0, max(d$share) + 3)),
      showlegend = FALSE,
      margin = list(t = 75, b = 70),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(text = "10% =<br>national<br>baseline", x = decile_labels[5], y = 13,
          showarrow = FALSE, font = list(size = 8, color = "#cc0000"), xanchor = "center"))
    ) %>% config(displayModeBar = FALSE)

  # Add share labels — push away from 10% baseline to avoid overlap
  for (i in seq_len(nrow(d))) {
    val <- d$share[i]
    if (val >= 8 && val <= 12) {
      y_pos <- val - 3
    } else {
      y_pos <- val + 1
    }
    p <- p %>% add_annotations(
      x = d$label[i], y = y_pos,
      text = sprintf("%.1f%%", val),
      showarrow = FALSE, font = list(size = 10, color = "#4A90D9"))
  }

  plotly_div(id_prefix, plotly_to_json(p), "500px", source = SRC_INCOME)
}

# Pre-compute top- and bottom-decile shares so the text cards track the
# underlying data automatically instead of hard-coding literals that drift
# whenever the pipeline is re-run.
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

writeLines(page_template("Income", paste0(
  '<div class="page-content">',
  sprintf('<div class="text-card pt1" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of first-generation Iranian-American households (ages 25&ndash;54) fall in the top U.S. income decile &mdash; more than double the national baseline of 10%%.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Only %s%% fall in the lowest decile.</div>
  </div>', round(fg_d10), format(fg_d1, nsmall = 1)),
  sprintf('<div class="text-card pt2" style="text-align:center;">
    <div style="font-size:36px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em;">%d%%</div>
    <div style="font-size:15px; font-weight:500; color:#333; margin-top:12px; line-height:1.45;">of second-generation Iranian-American households (ages 25&ndash;54) fall in the top U.S. income decile &mdash; even more concentrated at the top than the first generation.</div>
    <div style="font-size:13.5px; color:#555; margin-top:14px; line-height:1.55; max-width:420px; margin-left:auto; margin-right:auto;">Just %s%% fall in the lowest decile.</div>
  </div>', round(sg_d10), format(sg_d1, nsmall = 1)),
  '<div class="chart-card pc1">', make_income_chart(inc, "1st gen", "First Generation", "inc1"), '</div>',
  '<div class="chart-card pc2">', make_income_chart(inc, "2nd gen", "Second Generation", "inc2"), '</div>',
  '</div>'
)), "docs/pages/us-income.html")
cat("  Done\n")


# =====================================================
# US POPULATION — headline, waterfall, region bar, maps
# =====================================================
cat("Building us-population...\n")

# Waterfall
wf <- read_csv(file.path(DATA_DIR, "waterfall_components.csv"), show_col_types = FALSE)
wf$ymin <- wf$cumulative - wf$weighted_n
wf$ymax <- wf$cumulative
wf$label <- format(wf$weighted_n, big.mark = ",")
# Clear x-axis labels
wf$short_label <- c("Birthplace +\nAncestry", "Birthplace\nonly",
                     "Ancestry\nonly", "Children in\nIranian households",
                     "Race\nonly")
# Longer hover labels with context
wf$hover_label <- c("Born in Iran +<br>Iranian ancestry",
                     "Born in Iran<br>(ancestry not reported)",
                     "Iranian ancestry<br>(U.S.-born)",
                     "Children in Iranian households<br>(ancestry not reported for them)",
                     "Reported Iranian<br>as race")

# Color scheme: blue gradient for core (1-3), then distinct colors for additions
wf_colors <- c("#1a4e72", "#2774AE", "#5a9bd5", "#4a8c6f", "#c4793a")

p_waterfall <- plot_ly() %>%
  add_bars(data = wf, x = ~short_label, y = ~weighted_n, base = ~ymin,
    marker = list(color = wf_colors),
    text = ~sprintf("<b>%s</b><br>%s (%.1f%%)<br>Cumulative: %s",
      wf$hover_label, format(weighted_n, big.mark = ","), pct, format(cumulative, big.mark = ",")),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Americans: How We Count</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = "", tickfont = list(size = 10), tickangle = 0,
      categoryorder = "array",
      categoryarray = wf$short_label),
    yaxis = list(title = "", tickformat = ","),
    showlegend = FALSE,
    margin = list(t = 50, b = 110, l = 60),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# Region bar chart — reuse iran_data already loaded above
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
p_region <- plot_ly(data = region_dat, x = ~REGION, y = ~pop, type = "bar",
    marker = list(color = c("#7b5ea7", "#d4a943", "#2ca089", "#e07b54")),
    text = ~sprintf("<b>%s</b><br>%s (%s%%)",
      REGION, format(pop, big.mark = ","), round(pop / total_pop * 100)),
    hoverinfo = "text", textposition = "none") %>%
  layout(
    title = list(text = "<b>Iranian-Americans by US Region</b>",
      font = list(size = 16, family = "Montserrat")),
    xaxis = list(title = ""), yaxis = list(title = "", tickformat = ","),
    margin = list(t = 60, b = 40),
    plot_bgcolor = "white", paper_bgcolor = "white"
  ) %>% config(displayModeBar = FALSE)

# --- Generate PMTiles for maps (freestiler) ---
# Skip map generation if freestiler not installed (maps already built)
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
# Title-case state names for display
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

# LA PUMA data — use official 2020 tigris boundaries, NOT the GeoPackage
library(tigris)
ca_pumas_official <- pumas(state = "06", year = 2020, cb = TRUE)
la_counties_fips <- c("037", "059", "111")  # LA, Orange, Ventura
la_pumas <- ca_pumas_official %>%
  filter(substr(GEOID20, 3, 5) %in% la_counties_fips) %>%
  st_transform(4326)

# Join Iranian population from CSV
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

# Population page — custom template with MapLibre GL JS
pop_page <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Population</title>
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap" rel="stylesheet">
<script src="lib/plotly-3.4.0.min.js"></script>
<link rel="stylesheet" href="https://unpkg.com/maplibre-gl@4.7.1/dist/maplibre-gl.css">
<script src="https://unpkg.com/maplibre-gl@4.7.1/dist/maplibre-gl.js"></script>
<script src="https://unpkg.com/pmtiles@3.2.1/dist/pmtiles.js"></script>
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
  <div class="label">Estimated Iranian-American Population</div>
  <div class="number">794,915</div>
  <div class="label" style="margin-top:6px; font-size:13px; color:#555;">Based on the <a href="https://www2.census.gov/programs-surveys/acs/methodology/questionnaires/2024/quest24.pdf" style="color:#2774AE;" target="_blank">2024 American Community Survey</a>, a nationwide survey by the U.S. Census Bureau</div>
  <div style="margin:14px auto 0; max-width:440px; font-size:13px; color:#444; text-align:left; line-height:1.7;">
    <p style="margin-bottom:8px;">A person is counted if they meet <em>at least one</em> of four survey questions:</p>
    <ul style="padding-left:20px; margin:0; line-height:2;">
      <li><strong>Place of birth</strong> <span style="color:#888;">&mdash; &ldquo;Where was this person born?&rdquo;</span></li>
      <li><strong>Ancestry</strong> <span style="color:#888;">&mdash; &ldquo;What is this person&rsquo;s ancestry or ethnic origin?&rdquo;</span></li>
      <li><strong>Race</strong> <span style="color:#888;">&mdash; &ldquo;Iranian&rdquo; written in under &ldquo;White&rdquo;</span></li>
      <li><strong>Parental origin</strong> <span style="color:#888;">&mdash; lives with a parent who meets any of the above</span></li>
    </ul>
  </div>
</div>
<div class="chart-card">', plotly_div("waterfall", plotly_to_json(p_waterfall), "400px", source = SRC_POP_1YR),
'<script>if(window.innerWidth<900){setTimeout(function(){var el=document.getElementById("waterfall");if(el&&window.Plotly)Plotly.relayout(el,{"xaxis.tickangle":-45,"margin.b":130});},400);}</script>',
'</div>
</div>

<!-- Bottom row: region bar + map -->
<div class="chart-row">
<div class="chart-card">', plotly_div("region", plotly_to_json(p_region), "400px", source = SRC_POP_5YR), '</div>
<div class="chart-card" style="margin-bottom:0; padding-bottom:8px;">
  <div class="section-title" style="margin-top:0;">Geographic Distribution of Iranian-Americans</div>
  <div class="tab-bar" style="margin-bottom:8px;">
    <button class="tab-btn active" data-btn-group="geo" onclick="switchGeoTab(\'geo-state\',this)">By State</button>
    <button class="tab-btn" data-btn-group="geo" onclick="switchGeoTab(\'geo-county\',this)">By California County</button>
    <button class="tab-btn" data-btn-group="geo" onclick="switchGeoTab(\'geo-la\',this)">Greater Los Angeles</button>
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
    <p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0;">Source: <a href="https://www.census.gov/programs-surveys/acs/microdata.html" target="_blank" style="color:#2774AE;">U.S. Census Bureau</a> &mdash; ACS 2020&ndash;2024 5-Year PUMS<br>Weighted population estimates by state. Includes birthplace, ancestry, and parental origin.</p>
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
    <p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0;">Source: ACS 2020&ndash;2024 5-Year<br>California counties only. Includes birthplace, ancestry, and parental origin, allocated from sub-county survey areas.</p>
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
    <p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0;">Source: ACS 2020&ndash;2024 5-Year PUMS (PUMA-level)<br>Greater Los Angeles CSA. Includes birthplace, ancestry, and parental origin.<br>Census survey areas, not exact neighborhoods.</p>
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
      .setHTML("<b>" + f.properties.state + "</b><br>" + pop.toLocaleString() + " Iranian-Americans<br>" + pct + "% of U.S. total")
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
          pop.toLocaleString() + " Iranian-Americans<br>" + pct + "% of CA total")
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
          pop.toLocaleString() + " Iranian-Americans<br>" + laPct + "% of CA total")
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

writeLines(pop_page, "docs/pages/us-population.html")
cat("  Done\n")


cat("\nAll pages rebuilt.\n")

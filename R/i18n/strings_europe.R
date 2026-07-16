# ============================================================================
# R/i18n/strings_europe.R  — Europe overview (eu-overview) string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_europe.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(), plus one display-name lookup used only by this page:
#   EU_COUNTRY_FA   English country name -> Persian country name (fa only; the
#                   en path never touches it). Covers the 23 mapped countries
#                   plus the synthetic "France (2019)" time-series label.
#
# RULES honored here (same as strings_france.R / strings_global.R):
#   * en values are the EXACT English strings from build_europe.R (source of
#     truth) so the English edition stays BYTE-IDENTICAL.
#   * Every %d / format() slot from the original is rewritten as %s; the builder
#     passes fa_num()/fmtv() so the number is language-formatted.
#   * HTML tags / entities (<b>, <br>, &mdash;, &amp;, –) mirror the en
#     structure; only the human text is swapped in fa. ASCII digits are KEPT in
#     fa BODY literals (source lines, cards, legend) — FA_NUM_SCRIPT Persian-izes
#     them at runtime. Numbers baked into PLOTLY (hover text, chart-title year
#     range) are NOT reached by that runtime pass, so the builder passes them
#     through fa_num()/fmtv()/htxt() to arrive already Persian.
#   * Official agency acronyms (Eurostat, Destatis, ONS, NRS, NISRA, BFS, INSEE)
#     stay Latin; in fa they are glossed with the acronym isolated in <bdi>
#     (glossary convention). French proper names (Recensement de la population,
#     immigré) stay Latin inline, as on the English page.
#
# Europe overview framing: Iran-BORN (متولد ایران) across Europe — headline
# title "Estimated Iran-Born Population in Europe". This page carries NO
# percentages, so no ٪ / «درصد» strings appear.
# ============================================================================

STR <- list(

  # --- page shell -------------------------------------------------------------
  eu_page_title = list(
    en = "Europe: Iran-Born Overview",
    fa = "اروپا: نمای کلی متولدان ایران"),

  # --- headline card ----------------------------------------------------------
  eu_headline_label = list(
    en = "Estimated Iran-Born Population in Europe",
    fa = "برآورد جمعیت متولدان ایران در اروپا"),
  eu_headline_sublabel = list(
    en = "Summed across %s European countries with published data",
    fa = "مجموع %s کشور اروپایی دارای داده منتشرشده"),

  # --- identification box ------------------------------------------------------
  eu_idbox_intro = list(
    en = "A person is counted if their country of birth is Iran, as recorded in each country\u2019s census or population register.",
    fa = "هر فرد در صورتی شمرده می‌شود که کشور محل تولدش ایران باشد، بر پایهٔ سرشماری یا سامانهٔ ثبت جمعیت هر کشور."),
  eu_idbox_sources = list(
    en = "Most figures come from Eurostat; Germany, the United Kingdom, and Switzerland use their national statistics.",
    fa = "بیشتر ارقام از Eurostat گرفته شده و برای آلمان، بریتانیا و سوئیس از آمار ملی آن کشورها استفاده شده است."),
  eu_idbox_b1_span = list(
    en = "&mdash; %s European countries (country-of-birth tables)",
    fa = "&mdash; %s کشور اروپایی (جدول‌های کشور محل تولد)"),
  eu_idbox_b2_span = list(
    en = "&mdash; Germany",
    fa = "&mdash; آلمان"),
  eu_idbox_b3_span = list(
    en = "&mdash; United Kingdom (combined)",
    fa = "&mdash; بریتانیا (ترکیبی)"),
  eu_idbox_b4_span = list(
    en = "&mdash; Switzerland (STATPOP register)",
    fa = "&mdash; سوئیس (سامانه ثبت STATPOP)"),

  # --- source link inner text (agency acronym stays Latin, glossed in fa) ------
  eu_link_eurostat = list(
    en = "Eurostat",
    fa = "اداره آمار اتحادیه اروپا (<bdi>Eurostat</bdi>)"),
  eu_link_mikro = list(
    en = "Destatis Mikrozensus 2025",
    fa = "پیمایش خانوار Mikrozensus آلمان، 2025 (<bdi>Destatis</bdi>)"),
  eu_link_ons = list(
    en = "ONS Census 2021",
    fa = "سرشماری 2021 اداره ملی آمار بریتانیا (<bdi>ONS</bdi>)"),
  eu_link_nrs = list(
    en = "NRS Scotland's Census 2022",
    fa = "سرشماری 2022 اسکاتلند، اداره ثبت ملی (<bdi>NRS</bdi>)"),
  eu_link_nisra = list(
    en = "NISRA NI Census 2021",
    fa = "سرشماری 2021 ایرلند شمالی (<bdi>NISRA</bdi>)"),
  eu_link_bfs = list(
    en = "Swiss Federal Statistical Office (BFS)",
    fa = "اداره فدرال آمار سوئیس (<bdi>BFS</bdi>)"),

  # --- source lines ------------------------------------------------------------
  eu_uk_combined = list(
    en = "%s (England &amp; Wales), %s, and %s",
    fa = "%s (انگلستان و ولز)، %s و %s"),
  eu_src_note = list(
    en = "Source: %s population by country of birth. Germany from %s. United Kingdom combined from %s.",
    fa = "منبع: %s جمعیت به تفکیک کشور محل تولد. آلمان از %s. بریتانیا به‌صورت ترکیبی از %s."),
  eu_bar_src_extra = list(
    en = " Countries with fewer than 10,000 Iran-born residents are shown on the map but omitted from this ranking.",
    fa = " کشورهایی با کمتر از 10,000 نفر متولد ایران روی نقشه نشان داده می‌شوند اما از این رتبه‌بندی کنار گذاشته شده‌اند."),
  eu_ts_src = list(
    en = "Source: %s for the seven solid lines (Sweden, Netherlands, Austria, Italy, Norway, Denmark, Finland). United Kingdom (dotted) is from ONS/NRS/NISRA census points 2001, 2011, and 2021/22. France (dashed) is from INSEE Recensement de la population — census snapshot 1999, then continuous 2006–2019. The INSEE “immigré” definition is narrower than Eurostat’s foreign-born. Switzerland (dashed) is from the BFS register, 2010–2024. Germany is omitted because Eurostat does not publish Iran-born counts for Germany; see the Germany pages.",
    fa = "منبع: %s برای هفت خط توپُر (سوئد، هلند، اتریش، ایتالیا، نروژ، دانمارک، فنلاند). بریتانیا (خط‌چین نقطه‌ای) از نقاط سرشماری ONS/NRS/NISRA در سال‌های 2001، 2011 و 2021/22. فرانسه (خط‌چین) از Recensement de la population اداره آمار فرانسه — دادهٔ سرشماری 1999 و سپس پیوسته 2006–2019. تعریف “immigré” اداره آمار فرانسه (INSEE) محدودتر از تعریف «متولد خارج» Eurostat است. سوئیس (خط‌چین) از سامانه ثبت BFS، 2010–2024. آلمان کنار گذاشته شده است زیرا Eurostat شمار متولدان ایران را برای آلمان منتشر نمی‌کند؛ صفحه‌های آلمان را ببینید."),

  # --- map legend swatch labels -----------------------------------------------
  eu_leg_bin1 = list(
    en = "Under 20,000",
    fa = "کمتر از 20,000"),
  eu_leg_bin2 = list(
    en = "20,000 – 35,000",
    fa = "20,000 تا 35,000"),
  eu_leg_bin3 = list(
    en = "35,000 – 70,000",
    fa = "35,000 تا 70,000"),
  eu_leg_bin4 = list(
    en = "70,000 – 120,000",
    fa = "70,000 تا 120,000"),
  eu_leg_bin5 = list(
    en = "120,000 or more",
    fa = "120,000 یا بیشتر"),
  eu_map_section_title = list(
    en = "Where Iran-Born Residents Live in Europe",
    fa = "ساکنان متولد ایران در اروپا کجا زندگی می‌کنند"),

  # --- chart titles -----------------------------------------------------------
  eu_bar_title = list(
    en = "<b>Iran-Born in Europe by Country,<br>Most Recent Year</b>",
    fa = "<b>متولدان ایران در اروپا به تفکیک کشور،<br>آخرین سال موجود</b>"),
  eu_ts_title = list(
    en = "<b>Iran-Born Population in Europe<br>Over Time, %s–%s</b>",
    fa = "<b>جمعیت متولدان ایران در اروپا<br>در گذر زمان، %s–%s</b>"),

  # --- hover text -------------------------------------------------------------
  # map + bar ranking share one template: (country, value, most-recent-year).
  eu_hover_count = list(
    en = "<b>%s</b><br>%s Iran-born residents<br>Most recent year: %s",
    fa = "<b>%s</b><br>%s نفر متولد ایران<br>آخرین سال موجود: %s"),
  # time series — Eurostat solid lines: (country, year, value).
  eu_ts_hover = list(
    en = "<b>%s</b> %s<br>%s Iran-born residents",
    fa = "<b>%s</b> %s<br>%s نفر متولد ایران"),
  eu_ts_hover_uk = list(
    en = "<b>United Kingdom</b> %s<br>%s Iran-born residents (census)",
    fa = "<b>بریتانیا</b> %s<br>%s نفر متولد ایران (سرشماری)"),
  eu_ts_hover_fr = list(
    en = "<b>France</b> %s<br>%s Iran-born (INSEE immigré definition)",
    fa = "<b>فرانسه</b> %s<br>%s نفر متولد ایران (بر پایه تعریف “immigré” اداره آمار فرانسه INSEE)"),
  eu_ts_hover_ch = list(
    en = "<b>Switzerland</b> %s<br>%s Iran-born",
    fa = "<b>سوئیس</b> %s<br>%s نفر متولد ایران")
)

# --- English country name -> Persian display name (fa hover/labels only) ------
# Keys are the exact `country` values in data/europe/iran_born_combined.csv
# (23 rows) plus the synthetic "France (2019)" time-series right-edge label.
# Missing keys fall back to the English name in the builder's clab() (never NA).
EU_COUNTRY_FA <- c(
  "Austria"        = "اتریش",
  "Belgium"        = "بلژیک",
  "Bulgaria"       = "بلغارستان",
  "Switzerland"    = "سوئیس",
  "Czechia"        = "جمهوری چک",
  "Germany"        = "آلمان",
  "Denmark"        = "دانمارک",
  "Estonia"        = "استونی",
  "Finland"        = "فنلاند",
  "France"         = "فرانسه",
  "France (2019)"  = "فرانسه \u2066(2019)\u2069",
  "Hungary"        = "مجارستان",
  "Iceland"        = "ایسلند",
  "Italy"          = "ایتالیا",
  "Lithuania"      = "لیتوانی",
  "Luxembourg"     = "لوکزامبورگ",
  "Latvia"         = "لتونی",
  "Netherlands"    = "هلند",
  "Norway"         = "نروژ",
  "Romania"        = "رومانی",
  "Sweden"         = "سوئد",
  "Slovenia"       = "اسلوونی",
  "Slovakia"       = "اسلواکی",
  "United Kingdom" = "بریتانیا"
)

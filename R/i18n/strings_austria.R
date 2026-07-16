# ============================================================================
# R/i18n/strings_austria.R  — Austria page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_austria.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R / strings_armenia.R):
#   * en values are the EXACT English strings from build_austria.R (source of
#     truth) so the English edition stays byte-identical.
#   * Every %d / %.1f / format() slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted. The
#     percent SIGN lives in the template (%% in en,  the WORD «درصد» in fa).
#   * HTML tags / entities (<strong>, <br>, &mdash;) and the U+2013 en-dash
#     mirror the en structure; only the human text is swapped in fa. Latin
#     digits in fa prose are converted at runtime by FA_NUM_SCRIPT.
#   * Official agency names ("Statistics Austria", "Eurostat", "UN DESA") stay
#     Latin in both editions (they arrive via the language-independent links).
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  at_pop_title = list(
    en = "Austria: Population",
    fa = "اتریش: جمعیت"),
  at_pop_headline_label = list(
    en = "Estimated Iran-Born Population in Austria",
    fa = "برآورد جمعیت متولدان ایران در اتریش"),
  at_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, %s",
    fa = "بر پایه سامانه ثبت جمعیت %s، %s"),

  # --- identification box ------------------------------------------------------
  at_pop_idbox_intro = list(
    en = "Austria uses population registers. A person is counted as Iran-born based on:",
    fa = "اتریش از سامانه ثبت جمعیت استفاده می‌کند. هر فرد بر این اساس متولد ایران به شمار می‌آید:"),
  at_pop_idbox_bullet1 = list(
    en = '<strong>Country of birth</strong> <span style="color:#6b6b6b;">&mdash; recorded in the Central Register of Residents</span>',
    fa = '<strong>کشور محل تولد</strong> <span style="color:#6b6b6b;">&mdash; ثبت‌شده در سامانه مرکزی ثبت ساکنان</span>'),
  at_pop_thirdgen_note = list(
    en = "Austrian-born children of Iran-born parents are not counted as Iran-born in population statistics. This count reflects first-generation residents only.",
    fa = "فرزندان متولد اتریش با والدین متولد ایران در آمار جمعیتی متولد ایران به شمار نمی‌آیند. این شمار تنها ساکنان نسل اول را دربرمی‌گیرد."),

  # --- sex breakdown boxes -----------------------------------------------------
  at_sex_box_title = list(
    en = "Iran-Born Population by Sex",
    fa = "جمعیت متولدان ایران به تفکیک جنسیت"),
  at_sex_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  at_sex_male_label = list(
    en = "Male",
    fa = "مرد"),
  at_sex_female_label = list(
    en = "Female",
    fa = "زن"),
  at_sex_male_sub = list(
    en = "Iran-born men",
    fa = "مردان متولد ایران"),
  at_sex_female_sub = list(
    en = "Iran-born women",
    fa = "زنان متولد ایران"),

  # --- map section -------------------------------------------------------------
  at_geo_section_title = list(
    en = "Geographic Distribution in Austria",
    fa = "توزیع جغرافیایی در اتریش"),

  # --- trend chart -------------------------------------------------------------
  at_hist_title = list(
    en = "<b>Iran-Born Population in Austria,<br>%s–%s</b>",
    fa = "<b>جمعیت متولدان ایران در اتریش،<br>%s–%s</b>"),
  at_hist_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br>%s نفر متولد ایران"),

  # --- map hover ---------------------------------------------------------------
  at_map_hover = list(
    en = "<b>%s</b><br>%s Iran-born<br>%s%% of total",
    fa = "<b>%s</b><br>%s نفر متولد ایران<br>%s درصد از کل"),

  # --- source lines ------------------------------------------------------------
  at_src_trend = list(
    en = "Source: %s &mdash; Iran-born population stock, 2000–2025; 1990 and 1995 from UN DESA",
    fa = "منبع: %s &mdash; جمعیت متولدان ایران، 2000–2025؛ داده‌های 1990 و 1995 از UN DESA"),
  at_src_census = list(
    en = "Source: %s &mdash; Census 2021, Iran-born by region",
    fa = "منبع: %s &mdash; سرشماری 2021، متولدان ایران به تفکیک منطقه")
)

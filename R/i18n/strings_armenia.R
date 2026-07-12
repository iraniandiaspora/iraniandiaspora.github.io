# ============================================================================
# R/i18n/strings_armenia.R  — Armenia page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_armenia.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R):
#   * en values are the EXACT English strings from build_armenia.R (source of
#     truth) so the English edition stays byte-identical.
#   * Every %d / format() slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted and
#     the percent SIGN would live in the template (%% en, ٪ fa) if used.
#   * HTML tags / entities (<strong>, <br>, &mdash;) mirror the en structure;
#     only the human text is swapped in fa. Latin digits in fa prose are
#     converted at runtime by FA_NUM_SCRIPT.
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  am_pop_title = list(
    en = "Armenia: Population",
    fa = "ارمنستان: جمعیت"),
  am_pop_headline_label = list(
    en = "Estimated Iran-Born Population in Armenia",
    fa = "برآورد جمعیت متولدان ایران در ارمنستان"),
  am_pop_headline_caption = list(
    en = "Based on the %s Population Census maintained by %s",
    fa = "بر پایه سرشماری جمعیت سال %s، انجام‌شده توسط کمیته آمار ارمنستان (%s)"),

  # --- identification box ------------------------------------------------------
  am_pop_idbox_intro = list(
    en = "Armenia counts Iran-born residents based on:",
    fa = "ارمنستان ساکنان متولد ایران را بر این اساس می‌شمارد:"),
  am_pop_idbox_bullet1 = list(
    en = '<strong>Place of birth</strong> <span style="color:#6b6b6b;">&mdash; recorded in the 2022 census short form</span>',
    fa = '<strong>محل تولد</strong> <span style="color:#6b6b6b;">&mdash; ثبت‌شده در پرسشنامه کوتاه سرشماری 2022</span>'),
  am_pop_thirdgen_note = list(
    en = "Armenia does not separately identify children of Iran-born parents.",
    fa = "ارمنستان فرزندان والدین متولد ایران را جداگانه شناسایی نمی‌کند."),

  # --- citizenship breakdown notes --------------------------------------------
  am_pop_cit_intro = list(
    en = "Of the %s Iran-born residents in 2022:",
    fa = "از مجموع %s ساکن متولد ایران در سال 2022:"),
  am_pop_cit_iranian = list(
    en = "%s still held an Iranian passport",
    fa = "%s نفر همچنان گذرنامه ایرانی داشتند"),
  am_pop_cit_dual = list(
    en = "249 held both Iranian and Armenian citizenship",
    fa = "249 نفر هم‌زمان تابعیت ایران و ارمنستان را داشتند"),
  am_pop_cit_naturalized = list(
    en = "The remaining %s had naturalized as Armenian citizens",
    fa = "%s نفر باقی‌مانده تابعیت ارمنستان را گرفته بودند"),
  am_pop_un_note = list(
    en = "The UN Migrant Stock series uses interpolation between census years and gives a slightly higher 2024 estimate of 6,793.",
    fa = "سری برآورد جمعیت مهاجران سازمان ملل میان سال‌های سرشماری درون‌یابی می‌کند و برآورد آن برای سال 2024 اندکی بالاتر است: 6,793 نفر."),

  # --- trend chart -------------------------------------------------------------
  am_hist_title = list(
    en = "<b>Iran-Born Population in Armenia,<br>1990–2024</b>",
    fa = "<b>جمعیت متولدان ایران در ارمنستان،<br>1990–2024</b>"),
  am_hist_hover_un = list(
    en = "<b>%s</b><br>%s Iran-born (UN estimate)",
    fa = "<b>%s</b><br>%s نفر متولد ایران (برآورد سازمان ملل)"),
  am_hist_hover_armstat = list(
    en = "<b>%s</b><br>%s Iran-born (Armstat census)",
    fa = "<b>%s</b><br>%s نفر متولد ایران (سرشماری ارمنستان)"),
  am_leg_un = list(
    en = "UN Migrant Stock",
    fa = "برآورد مهاجران سازمان ملل"),
  am_leg_armstat = list(
    en = "Armstat census",
    fa = "سرشماری ارمنستان"),

  # --- source line -------------------------------------------------------------
  am_src_trend = list(
    en = "Source: %s — International Migrant Stock 2024; %s — Population Censuses 2011 and 2022",
    fa = "منبع: %s — برآورد جمعیت مهاجران بین‌المللی 2024؛ %s — سرشماری‌های جمعیت 2011 و 2022")
)

# ============================================================================
# R/i18n/strings_norway.R  — Norway page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_norway.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R):
#   * en values are the EXACT English strings from build_norway.R (source of
#     truth) so the English edition stays BYTE-IDENTICAL.
#   * Every %d / %.1f / format() slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted and the
#     percent SIGN lives in the template (%% in en, درصد in fa prose/hover, ٪ on
#     chart axis ticks).
#   * HTML tags / entities (<strong>, <br>, &mdash;) mirror the en structure;
#     only the human text is swapped in fa. Latin digits in fa prose are
#     converted at runtime by FA_NUM_SCRIPT.
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  no_pop_title = list(
    en = "Norway: Population",
    fa = "نروژ: جمعیت"),
  no_pop_headline_label = list(
    en = "Estimated Iranian-Origin Population in Norway",
    fa = "برآورد جمعیت ایرانی‌تبار در نروژ"),
  no_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, %s",
    fa = "بر پایه سامانه ثبت جمعیت نگهداری‌شده توسط %s، %s"),

  # --- identification box ------------------------------------------------------
  no_pop_idbox_intro = list(
    en = "Norway uses population registers. A person is classified as Iranian-origin if they are:",
    fa = "نروژ از سامانه ثبت جمعیت استفاده می‌کند. هر فرد در صورتی ایرانی‌تبار طبقه‌بندی می‌شود که یکی از این موارد باشد:"),
  no_pop_idbox_bullet1 = list(
    en = "<strong>Immigrant</strong>: born in Iran with two foreign-born parents",
    fa = "<strong>مهاجر</strong>: متولد ایران با دو والدِ متولد خارج از کشور"),
  no_pop_idbox_bullet2 = list(
    en = "<strong>Norwegian-born to immigrant parents</strong>: born in Norway with two Iran-born parents",
    fa = "<strong>متولد نروژ از والدین مهاجر</strong>: متولد نروژ با دو والدِ متولد ایران"),
  no_pop_thirdgen_note = list(
    en = "Children with one Norwegian-born parent are not counted as immigrants or Norwegian-born to immigrant parents in SSB statistics.",
    fa = "فرزندانی که یکی از والدینشان متولد نروژ باشد، در آمار SSB نه مهاجر و نه متولد نروژ از والدین مهاجر به شمار می‌آیند."),

  # --- generation boxes --------------------------------------------------------
  no_gen_box_title = list(
    en = "Iranian-Origin Population by Generation",
    fa = "جمعیت ایرانی‌تبار به تفکیک نسل"),
  no_gen_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  no_gen1_sub = list(
    en = "Born in Iran",
    fa = "متولد ایران"),
  no_gen2_sub = list(
    en = "Born in Norway to immigrant parent(s)",
    fa = "متولد نروژ از والد(ین) مهاجر"),

  # --- tabs + section ----------------------------------------------------------
  no_tab_hist = list(
    en = "Population Over Time",
    fa = "جمعیت در طول زمان"),
  no_tab_gen = list(
    en = "By Generation",
    fa = "به تفکیک نسل"),
  no_tab_emp = list(
    en = "Employment Rate",
    fa = "نرخ اشتغال"),
  no_geo_section_title = list(
    en = "Geographic Distribution in Norway",
    fa = "پراکندگی جغرافیایی در نروژ"),

  # --- historical trend chart --------------------------------------------------
  no_hist_title = list(
    en = "<b>Iran-Born Population in Norway,<br>1970–%s</b>",
    fa = "<b>جمعیت متولدان ایران در نروژ،<br>1970–%s</b>"),
  no_hist_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br>%s نفر متولد ایران"),

  # --- generation split chart --------------------------------------------------
  no_gen_title = list(
    en = "<b>Iranian-Origin Population in Norway<br>by Generation, %s–%s</b>",
    fa = "<b>جمعیت ایرانی‌تبار در نروژ<br>به تفکیک نسل، %s–%s</b>"),
  no_gen_hover = list(
    en = "<b>%s</b><br>Immigrants: %s<br>Norwegian-born: %s<br>Total: %s",
    fa = "<b>%s</b><br>مهاجران: %s<br>متولد نروژ: %s<br>مجموع: %s"),
  no_leg_immigrants = list(
    en = "Immigrants",
    fa = "مهاجران"),
  no_leg_norwegian_born = list(
    en = "Norwegian-born",
    fa = "متولد نروژ"),

  # --- employment-rate chart ---------------------------------------------------
  no_emp_title = list(
    en = "<b>Employment Rate of Iranian Immigrants<br>(Ages 20–66), 2001–2025</b>",
    fa = "<b>نرخ اشتغال مهاجران ایرانی<br>(20–66 ساله)، 2001–2025</b>"),
  no_emp_hover_total = list(
    en = "<b>%s</b><br>Employment rate: %s%%<br>%s employed (20–66)",
    fa = "<b>%s</b><br>نرخ اشتغال: %s درصد<br>%s نفر شاغل (20–66 ساله)"),
  no_emp_hover_male = list(
    en = "<b>%s</b><br>Male: %s%%",
    fa = "<b>%s</b><br>مرد: %s درصد"),
  no_emp_hover_female = list(
    en = "<b>%s</b><br>Female: %s%%",
    fa = "<b>%s</b><br>زن: %s درصد"),
  no_name_total = list(
    en = "Total",
    fa = "مجموع"),
  no_name_male = list(
    en = "Male",
    fa = "مرد"),
  no_name_female = list(
    en = "Female",
    fa = "زن"),
  no_leg_total = list(
    en = "Total",
    fa = "مجموع"),
  no_leg_male_dashed = list(
    en = "Male (dashed)",
    fa = "مرد (خط‌چین)"),
  no_leg_female_dashed = list(
    en = "Female (dashed)",
    fa = "زن (خط‌چین)"),

  # --- county choropleth -------------------------------------------------------
  no_county_hover = list(
    en = "<b>%s</b><br>%s Iranian-origin<br>%s%% of total",
    fa = "<b>%s</b><br>%s نفر ایرانی‌تبار<br>%s درصد از کل"),

  # --- source lines ------------------------------------------------------------
  no_src_ssb = list(
    en = "Source: %s &mdash; Population Register, 2026",
    fa = "منبع: %s &mdash; ثبت جمعیت، 2026"),
  no_src_ssb_emp = list(
    en = "Source: %s &mdash; Employed immigrants, fourth quarter, 2001–2025",
    fa = "منبع: %s &mdash; مهاجران شاغل، فصل چهارم، 2001–2025"),

  # --- axis affix --------------------------------------------------------------
  no_axis_pct_suffix = list(
    en = "%",
    fa = "٪")
)

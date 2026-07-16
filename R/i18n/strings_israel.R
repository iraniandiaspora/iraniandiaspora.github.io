# ============================================================================
# R/i18n/strings_israel.R  — Israel page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_israel.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R / strings_armenia.R):
#   * en values are the EXACT English strings from build_israel.R (source of
#     truth) so the English edition stays byte-identical.
#   * Every %d / %.1f / format() slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted and the
#     percent SIGN lives in the template (%% in en, «درصد» word in fa prose /
#     hovers; the axis ٪ sign is handled in the builder, not here).
#   * HTML tags / entities (<strong>, <br>, &mdash;, &ldquo;, &rsquo;) mirror
#     the en structure; only the human text is swapped in fa. Latin digits in
#     fa prose are Persian-ized at runtime by FA_NUM_SCRIPT — keep ASCII digits
#     in the literals here.
#   * Israel is register-based paternal-line 2-gen -> "Iranian-Origin" =
#     ایرانی‌تبار; Iran-born = متولد ایران; Israeli-born = متولد اسرائیل.
#   * Agency names (CBS / Central Bureau of Statistics) stay Latin in BOTH
#     editions; the fa source lines gloss them in the surrounding prose.
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  il_pop_title = list(
    en = "Israel: Population",
    fa = "اسرائیل: جمعیت"),
  il_pop_headline_label = list(
    en = "Estimated Iranian-Origin Population in Israel",
    fa = "برآورد جمعیت ایرانی‌تبار در اسرائیل"),
  il_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, 2024",
    fa = "بر پایه سامانه ثبت جمعیت %s، 2024"),

  # --- identification box -----------------------------------------------------
  il_pop_idbox_intro = list(
    en = "Israel defines &ldquo;country of origin&rdquo; using a paternal-line, two-generation rule:",
    fa = "اسرائیل «کشور مبدأ» را با قاعده‌ای دونسلی و بر پایه خط پدری تعریف می‌کند:"),
  il_pop_idbox_bullet1 = list(
    en = "<strong>Born abroad</strong>: own country of birth is Iran",
    fa = "<strong>متولد خارج از کشور</strong>: کشور محل تولد خود فرد ایران است"),
  il_pop_idbox_bullet2 = list(
    en = "<strong>Born in Israel</strong>: father&rsquo;s country of birth is Iran",
    fa = "<strong>متولد اسرائیل</strong>: کشور محل تولد پدر ایران است"),
  il_pop_thirdgen_note = list(
    en = "Third-generation descendants (Israeli-born, father Israeli-born) and maternal-only lines are not counted.",
    fa = "نوادگان نسل سوم (متولد اسرائیل با پدر متولد اسرائیل) و تبارهای صرفاً مادری شمرده نمی‌شوند."),

  # --- generation boxes -------------------------------------------------------
  il_gen_box_title = list(
    en = "Iranian-Origin Population by Generation",
    fa = "جمعیت ایرانی‌تبار به تفکیک نسل"),
  il_gen_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  il_gen1_label = list(
    en = "Iran-born",
    fa = "متولد ایران"),
  il_gen1_sub = list(
    en = "Born in Iran",
    fa = "متولد ایران"),
  il_gen2_label = list(
    en = "Israeli-born",
    fa = "متولد اسرائیل"),
  il_gen2_sub = list(
    en = "Born in Israel, father born in Iran",
    fa = "متولد اسرائیل، با پدر متولد ایران"),

  # --- age chart (grouped 1st vs 2nd gen) -------------------------------------
  il_age_title = list(
    en = "<b>Age Distribution of Iranian-Origin<br>Population in Israel</b>",
    fa = "<b>توزیع سنی جمعیت ایرانی‌تبار<br>در اسرائیل</b>"),
  il_age_hover_gen1 = list(
    en = "<b>%s</b><br>Iran-born: %s",
    fa = "<b>%s</b><br>متولد ایران: %s"),
  il_age_hover_gen2 = list(
    en = "<b>%s</b><br>Israeli-born: %s",
    fa = "<b>%s</b><br>متولد اسرائیل: %s"),
  il_leg_gen1 = list(
    en = "Iran-born (1st gen)",
    fa = "متولد ایران (نسل اول)"),
  il_leg_gen2 = list(
    en = "Israeli-born (2nd gen)",
    fa = "متولد اسرائیل (نسل دوم)"),

  # --- Iran-born detail chart (8 age bins) ------------------------------------
  il_gen1_title = list(
    en = "<b>Iran-Born Population in Israel<br>by Age</b>",
    fa = "<b>جمعیت متولدان ایران در اسرائیل<br>به تفکیک سن</b>"),
  il_gen1_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s (%s درصد)"),

  # --- UN trend chart ---------------------------------------------------------
  il_trend_title = list(
    en = "<b>Iran-Born Population in Israel,<br>1990–2024</b>",
    fa = "<b>جمعیت متولدان ایران در اسرائیل،<br>1990–2024</b>"),
  il_trend_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br>%s نفر متولد ایران"),

  # --- comparison chart (Asian-origin groups) ---------------------------------
  il_comp_title = list(
    en = "<b>Asian-Origin Groups in Israel</b>",
    fa = "<b>گروه‌های آسیایی‌تبار در اسرائیل</b>"),
  il_comp_hover = list(
    en = "<b>%s</b><br>Total: %s<br>First generation: %s<br>Second generation: %s",
    fa = "<b>%s</b><br>مجموع: %s<br>نسل اول: %s<br>نسل دوم: %s"),

  # --- comparison-group country / region names --------------------------------
  country_iraq = list(
    en = "Iraq",
    fa = "عراق"),
  country_iran = list(
    en = "Iran",
    fa = "ایران"),
  country_yemen = list(
    en = "Yemen",
    fa = "یمن"),
  country_turkiye = list(
    en = "Türkiye",
    fa = "ترکیه"),
  country_india_pakistan = list(
    en = "India and Pakistan",
    fa = "هند و پاکستان"),
  country_syria_lebanon = list(
    en = "Syria and Lebanon",
    fa = "سوریه و لبنان"),

  # --- tab labels -------------------------------------------------------------
  il_tab_both = list(
    en = "Iran-Born + Israeli-Born",
    fa = "متولد ایران + متولد اسرائیل"),
  il_tab_detail = list(
    en = "Iran-Born Only",
    fa = "فقط متولدان ایران"),
  il_tab_trend = list(
    en = "Population Over Time",
    fa = "جمعیت در طول زمان"),
  il_tab_comp = list(
    en = "Comparative Context",
    fa = "مقایسه با گروه‌های دیگر"),

  # --- source lines -----------------------------------------------------------
  il_src_cbs = list(
    en = "Source: %s &mdash; Statistical Abstract of Israel, 2024",
    fa = "منبع: %s &mdash; سالنامه آماری اسرائیل، 2024"),
  il_src_un = list(
    en = "Source: %s — International Migrant Stock 2024",
    fa = "منبع: %s — برآورد جمعیت مهاجران بین‌المللی 2024")
)

# ============================================================================
# R/i18n/strings_switzerland.R  — Switzerland page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_switzerland.R AFTER R/_helpers_i18n.R. Defines the global
# STR list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_norway.R):
#   * en values are the EXACT English strings from build_switzerland.R (source
#     of truth) so the English edition stays BYTE-IDENTICAL.
#   * Every %d / %.1f / format() slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted and the
#     percent SIGN lives in the template (%% in en, درصد in fa prose/hover, ٪ on
#     chart axis ticks).
#   * HTML tags / entities (<strong>, <br>, &mdash;) mirror the en structure;
#     only the human text is swapped in fa. Latin digits in fa prose are
#     converted at runtime by FA_NUM_SCRIPT (so ASCII digits stay in literals).
#   * Official agency names (BFS, Federal Statistical Office, STATPOP) stay
#     Latin in both editions.
#
# NOTE — the arrivals chart is Switzerland's one hybrid: the BARS are an annual
# flow, but the cumulative LINE is a SEPARATE stock-ratio series with its own
# tooltip (ch_arr_hover_line), distinct from the bar tooltip (ch_arr_hover_bar).
# Both are translated faithfully.
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  ch_pop_title = list(
    en = "Switzerland: Population",
    fa = "سوئیس: جمعیت"),
  ch_pop_headline_label = list(
    en = "Estimated Iran-Born Population in Switzerland",
    fa = "برآورد جمعیت متولدان ایران در سوئیس"),
  ch_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, %s",
    fa = "بر پایه سامانه ثبت جمعیت %s، %s"),

  # --- identification box ------------------------------------------------------
  ch_pop_idbox_intro = list(
    en = "Switzerland uses population registers. A person is counted as Iran-born based on:",
    fa = "سوئیس از سامانه ثبت جمعیت استفاده می‌کند. هر فرد بر پایه معیار زیر متولد ایران شمرده می‌شود:"),
  ch_pop_idbox_bullet1 = list(
    en = "<strong>Country of birth</strong> &mdash; recorded in the population register",
    fa = "<strong>کشور محل تولد</strong> &mdash; ثبت‌شده در سامانه ثبت جمعیت"),
  ch_pop_cit_sentence = list(
    en = "Of the %s Iran-born residents, %s (%s%%) hold Swiss citizenship, %s hold Iranian citizenship, and %s hold other nationalities.",
    fa = "از میان %s ساکن متولد ایران، %s نفر (%s درصد) تابعیت سوئیس، %s نفر تابعیت ایران و %s نفر تابعیت‌های دیگر دارند."),
  ch_pop_thirdgen_note = list(
    en = "The register tracks country of birth but not parental birthplace. Swiss-born children of Iran-born parents are not counted.",
    fa = "این سامانه کشور محل تولد را ثبت می‌کند اما محل تولد والدین را ثبت نمی‌کند. فرزندان متولد سوئیس با والدین متولد ایران شمرده نمی‌شوند."),

  # --- map section -------------------------------------------------------------
  ch_geo_section_title = list(
    en = "Geographic Distribution in Switzerland",
    fa = "توزیع جغرافیایی در سوئیس"),

  # --- population trend chart --------------------------------------------------
  ch_trend_title = list(
    en = "<b>Iran-Born Population in Switzerland,<br>2010–2024</b>",
    fa = "<b>جمعیت متولدان ایران در سوئیس،<br>2010–2024</b>"),
  ch_trend_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br>%s نفر متولد ایران"),

  # --- annual arrivals chart (hybrid: flow bars + stock-ratio line) -----------
  ch_arr_title = list(
    en = "<b>Iran-Born Annual Immigration<br>to Switzerland, 2011–2024</b>",
    fa = "<b>مهاجرت سالانه متولدان ایران<br>به سوئیس، 2011–2024</b>"),
  ch_arr_hover_bar = list(
    en = "<b>%s</b><br>%s arrivals",
    fa = "<b>%s</b><br>%s نفر واردشده"),
  ch_arr_name_bar = list(
    en = "Arrivals",
    fa = "واردشدگان"),
  ch_arr_hover_line = list(
    en = "<b>By end of %s:</b> %s%% of the 2024 population already present",
    fa = "<b>تا پایان %s:</b> %s درصد از جمعیت سال 2024 در سوئیس ساکن شده بودند"),
  ch_arr_name_line = list(
    en = "Share of current stock",
    fa = "سهم از جمعیت کنونی"),

  # --- canton choropleth -------------------------------------------------------
  ch_map_hover = list(
    en = "<b>%s</b><br>%s Iran-born<br>%s%% of total",
    fa = "<b>%s</b><br>%s نفر متولد ایران<br>%s درصد از کل"),

  # --- source lines ------------------------------------------------------------
  ch_src_bfs = list(
    en = "Source: %s &mdash; Population Register, 2024",
    fa = "منبع: %s &mdash; ثبت جمعیت، 2024"),
  ch_src_bfs_imm = list(
    en = "Source: %s &mdash; Immigration of permanent resident population, 2011–2024",
    fa = "منبع: %s &mdash; مهاجرت جمعیت مقیم دائم، 2011–2024"),

  # --- axis affix --------------------------------------------------------------
  ch_axis_pct_suffix = list(
    en = "%",
    fa = "٪")
)

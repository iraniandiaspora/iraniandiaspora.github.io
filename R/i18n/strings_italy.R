# ============================================================================
# R/i18n/strings_italy.R  — Italy page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_italy.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R / strings_armenia.R):
#   * en values are the EXACT English strings from build_italy.R (source of
#     truth) so the English edition stays byte-identical.
#   * Every %d / %.1f / format() slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted and the
#     percent SIGN lives in the template (%% en, درصد fa).
#   * HTML tags / entities (<strong>, <br>, &mdash;) mirror the en structure;
#     only the human text is swapped in fa. Latin digits in fa prose are
#     converted at runtime by FA_NUM_SCRIPT; ASCII digits are KEPT in literals.
#   * Official agency names (ISTAT, Eurostat) stay Latin in both editions.
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  it_pop_title = list(
    en = "Italy: Population",
    fa = "ایتالیا: جمعیت"),
  it_pop_headline_label = list(
    en = "Estimated Iran-Born Population in Italy",
    fa = "برآورد جمعیت متولدان ایران در ایتالیا"),
  it_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, %s",
    fa = "بر پایه سامانه ثبت جمعیت نگهداری‌شده توسط %s، %s"),

  # --- identification box ------------------------------------------------------
  it_pop_idbox_intro = list(
    en = "Italy uses population registers. A person is counted as Iran-born based on:",
    fa = "ایتالیا از سامانه ثبت جمعیت استفاده می‌کند. هر فرد بر پایه معیار زیر متولد ایران به شمار می‌آید:"),
  it_pop_idbox_bullet1 = list(
    en = '<strong>Country of birth</strong> <span style="color:#6b6b6b;">&mdash; recorded in municipal civil registers</span>',
    fa = '<strong>کشور محل تولد</strong> <span style="color:#6b6b6b;">&mdash; ثبت‌شده در دفاتر ثبت‌احوال شهرداری</span>'),
  it_pop_thirdgen_note = list(
    en = "Italian-born children of Iran-born parents are not counted.",
    fa = "فرزندان متولد ایتالیا با والدین متولد ایران شمرده نمی‌شوند."),

  # --- sex breakdown boxes -----------------------------------------------------
  it_sexbox_title = list(
    en = "Iran-Born Population by Sex",
    fa = "جمعیت متولدان ایران به تفکیک جنسیت"),
  it_gen_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  it_male_label = list(
    en = "Male",
    fa = "مرد"),
  it_male_sub = list(
    en = "Iran-born men",
    fa = "مردان متولد ایران"),
  it_female_label = list(
    en = "Female",
    fa = "زن"),
  it_female_sub = list(
    en = "Iran-born women",
    fa = "زنان متولد ایران"),

  # --- map section -------------------------------------------------------------
  it_geo_section_title = list(
    en = "Geographic Distribution in Italy",
    fa = "پراکندگی جغرافیایی در ایتالیا"),
  it_map_unavailable = list(
    en = "Map unavailable (GeoJSON missing)",
    fa = "نقشه در دسترس نیست (فایل GeoJSON یافت نشد)"),
  it_map_hover = list(
    en = "<b>%s</b><br>%s Iran-born<br>%s%% of total",
    fa = "<b>%s</b><br>%s نفر متولد ایران<br>%s درصد از کل"),

  # --- trend chart -------------------------------------------------------------
  it_hist_title = list(
    en = "<b>Iran-Born Population in Italy, 2002–2025</b>",
    fa = "<b>جمعیت متولدان ایران در ایتالیا، 2002–2025</b>"),
  it_hist_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br>%s نفر متولد ایران"),

  # --- source lines ------------------------------------------------------------
  it_src_istat = list(
    en = "Source: %s &mdash; Resident population by country of birth, 2025",
    fa = "منبع: %s &mdash; جمعیت ساکن به تفکیک کشور محل تولد، 2025"),
  it_src_hist = list(
    en = "Source: %s &mdash; Iran-born population stock, 2002–2025",
    fa = "منبع: %s &mdash; جمعیت متولدان ایران، 2002–2025")
)

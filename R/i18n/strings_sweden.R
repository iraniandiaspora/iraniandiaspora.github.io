# ============================================================================
# R/i18n/strings_sweden.R  — Sweden page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_sweden.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R / strings_armenia.R):
#   * en values are the EXACT English strings from build_sweden.R (source of
#     truth) so the English edition stays byte-identical.
#   * Every %d / %.1f / format() slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted and the
#     percent SIGN lives in the template (%% in en, درصد in fa).
#   * HTML tags / entities (<strong>, <br>, &mdash;) mirror the en structure;
#     only the human text is swapped in fa. ASCII digits in fa prose are
#     Persian-ized at runtime by FA_NUM_SCRIPT / at build by htxt().
#   * Official agency name "Statistics Sweden (SCB)" stays Latin in both
#     editions (it is the link text, language-independent); Swedish county
#     names (Stockholm, ...) stay Latin place names.
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  se_pop_title = list(
    en = "Sweden: Population",
    fa = "سوئد: جمعیت"),
  se_pop_headline_label = list(
    en = "Estimated Iranian-Origin Population in Sweden",
    fa = "جمعیت تخمینی ایرانی‌تباران در سوئد"),
  se_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, %s",
    fa = "بر پایه ثبت جمعیت نگه‌داری‌شده توسط %s، %s"),

  # --- identification box -----------------------------------------------------
  se_pop_idbox_intro = list(
    en = "Sweden uses population registers. A person is classified as Iranian-origin if they meet at least one of:",
    fa = "سوئد از سامانه ثبت جمعیت استفاده می‌کند. هر فرد در صورت داشتن دست‌کم یکی از این ویژگی‌ها ایرانی‌تبار طبقه‌بندی می‌شود:"),
  se_pop_idbox_bullet1 = list(
    en = '<strong>Born in Iran</strong> <span style="color:#6b6b6b;">&mdash; first generation</span>',
    fa = '<strong>متولد ایران</strong> <span style="color:#6b6b6b;">&mdash; نسل اول</span>'),
  se_pop_idbox_bullet2 = list(
    en = '<strong>Born in Sweden</strong> with a mother or father born in Iran <span style="color:#6b6b6b;">&mdash; second generation</span>',
    fa = '<strong>متولد سوئد</strong> با پدر یا مادر متولد ایران <span style="color:#6b6b6b;">&mdash; نسل دوم</span>'),
  se_pop_thirdgen_note = list(
    en = "The second generation includes children with one Swedish-born and one Iran-born parent. Third-generation residents are not counted. Classification is from birth records, not self-reported ethnicity.",
    fa = "نسل دوم شامل فرزندانی می‌شود که یکی از والدینشان متولد سوئد و دیگری متولد ایران است. ساکنان نسل سوم شمرده نمی‌شوند. این طبقه‌بندی بر پایه اسناد تولد است، نه قومیت خوداظهارشده."),

  # --- generation boxes -------------------------------------------------------
  se_gen_box_title = list(
    en = "Iranian-Origin Population by Generation",
    fa = "جمعیت ایرانی‌تبار به تفکیک نسل"),
  se_gen_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  se_gen1_label = list(
    en = "First generation",
    fa = "نسل اول"),
  se_gen1_sub = list(
    en = "Born in Iran",
    fa = "متولد ایران"),
  se_gen2_label = list(
    en = "Second generation",
    fa = "نسل دوم"),
  se_gen2_sub = list(
    en = "Born in Sweden, parent(s) born in Iran",
    fa = "متولد سوئد، با پدر یا مادر متولد ایران"),

  # --- tabs + section ---------------------------------------------------------
  se_tab_hist = list(
    en = "Population Over Time",
    fa = "جمعیت در طول زمان"),
  se_tab_yrs = list(
    en = "Year of Arrival",
    fa = "سال ورود"),
  se_geo_section_title = list(
    en = "Geographic Distribution in Sweden",
    fa = "پراکندگی جغرافیایی در سوئد"),

  # --- charts -----------------------------------------------------------------
  se_hist_title = list(
    en = "<b>Iran-Born Population in Sweden,<br>1990–2025</b>",
    fa = "<b>جمعیت متولدان ایران در سوئد،<br>1990–2025</b>"),
  se_hist_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br>%s نفر متولد ایران"),
  se_yrs_title = list(
    en = "<b>Iran-Born Residents in Sweden<br>by Year of Arrival</b>",
    fa = "<b>ساکنان متولد ایران در سوئد<br>به تفکیک سال ورود</b>"),
  se_yrs_hover_period = list(
    en = "<b>%s</b><br>%s arrivals (%s/year avg)<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s نفر واردشده (میانگین %s نفر در سال)<br>تجمعی: %s درصد"),
  se_yrs_hover_single = list(
    en = "<b>%s</b><br>%s arrivals<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s نفر واردشده<br>تجمعی: %s درصد"),
  se_yrs_cum_hover = list(
    en = "<b>%s</b><br>%s%% of today's Iran-born population had arrived",
    fa = "<b>%s</b><br>%s درصد از جمعیت کنونی متولدان ایران تا این سال وارد شده بودند"),
  se_yrs_before_label = list(
    en = "Before 1975",
    fa = "پیش از 1975"),
  se_yrs_annotation = list(
    en = "Arrivals before 2015 are grouped into 5-year periods, shown as a per-year average.",
    fa = "ورودهای پیش از 2015 در دوره‌های پنج‌ساله گروه‌بندی شده و به صورت میانگین سالانه نمایش داده می‌شوند."),
  se_county_hover = list(
    en = "<b>%s</b><br>%s Iran-born<br>%s%% of total",
    fa = "<b>%s</b><br>%s نفر متولد ایران<br>%s درصد از کل"),

  # --- source lines -----------------------------------------------------------
  se_src_scb = list(
    en = "Source: %s &mdash; Population Register, 2025",
    fa = "منبع: %s &mdash; سامانه ثبت جمعیت، 2025"),
  se_src_scb_pop = list(
    en = "Source: %s &mdash; Population by country of birth, 2024",
    fa = "منبع: %s &mdash; جمعیت به تفکیک کشور محل تولد، 2024"),
  se_src_hist = list(
    en = "Source: %s &mdash; Iran-born population stock, 1999–2025; gaps filled from Statistics Sweden, 1990 and 1995 from UN DESA",
    fa = "منبع: %s &mdash; جمعیت متولدان ایران، 1999–2025؛ خلأها از Statistics Sweden پر شده، داده‌های 1990 و 1995 از UN DESA")
)

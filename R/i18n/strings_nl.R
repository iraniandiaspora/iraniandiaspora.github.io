# ============================================================================
# R/i18n/strings_nl.R  — Netherlands page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_nl.R AFTER R/_helpers_i18n.R. Defines the global STR list
# consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (see build_nl.R header):
#   * en values are the EXACT English strings from build_nl.R (source of truth)
#     so the English editions stay byte-identical.
#   * Every %d / %.1f from the original is rewritten as %s; the builder passes
#     fa_num()/fmtv() so the number is language-formatted and the percent SIGN
#     lives in the template (%% in en,  درصد in fa).
#   * HTML tags / entities (<strong>, <br>, &mdash;, &ndash;) mirror the en
#     structure; only the human text is swapped in fa.
# ============================================================================

STR <- list(

  # --- generic --------------------------------------------------------------
  src_prefix = list(
    en = "Source: ",
    fa = "منبع: "),

  # --- population: page + headline -----------------------------------------
  nl_pop_title = list(
    en = "Netherlands: Population",
    fa = "هلند: جمعیت"),
  nl_pop_headline_label = list(
    en = "Estimated Iranian-Origin Population in the Netherlands",
    fa = "جمعیت تخمینی ایرانی‌تباران در هلند"),
  nl_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, Statistics Netherlands, January 2026",
    fa = "بر پایه ثبت جمعیت %s، اداره آمار هلند، ژانویه 2026"),
  nl_pop_idbox_intro = list(
    en = "The Netherlands uses population registers rather than a traditional census. A person is classified as Iranian-origin if they meet at least one of:",
    fa = "هلند به‌جای سرشماری سنتی از ثبت جمعیت استفاده می‌کند. هر فرد در صورت داشتن دست‌کم یکی از این دو ویژگی ایرانی‌تبار به شمار می‌آید:"),
  nl_pop_idbox_bullet1 = list(
    en = '<strong>Born in Iran</strong> <span style="color:#6b6b6b;">&mdash; first generation</span>',
    fa = '<strong>متولد ایران</strong> <span style="color:#6b6b6b;">&mdash; نسل اول</span>'),
  nl_pop_idbox_bullet2 = list(
    en = '<strong>Born in the Netherlands</strong> with a mother or father born in Iran <span style="color:#6b6b6b;">&mdash; second generation</span>',
    fa = '<strong>متولد هلند</strong> با پدر یا مادر متولد ایران <span style="color:#6b6b6b;">&mdash; نسل دوم</span>'),
  nl_pop_thirdgen_note = list(
    en = "Third-generation residents (Dutch-born with Dutch-born parents) are not counted as Iranian-origin in CBS statistics. The classification is derived from birth records, not self-reported ethnicity or language.",
    fa = "ساکنان نسل سوم (متولد هلند با پدر و مادرِ متولد هلند) در آمار CBS ایرانی‌تبار به شمار نمی‌آیند. این طبقه‌بندی بر پایه اسناد تولد است، نه قومیت یا زبانِ خوداظهارشده."),

  # --- population: generation boxes ----------------------------------------
  nl_gen_box_title = list(
    en = "Iranian-Origin Population by Generation",
    fa = "جمعیت ایرانی‌تبار به تفکیک نسل"),
  nl_gen_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  nl_gen1_label = list(
    en = "First generation",
    fa = "نسل اول"),
  nl_gen1_sub = list(
    en = "Born in Iran",
    fa = "متولد ایران"),
  nl_gen2_label = list(
    en = "Second generation",
    fa = "نسل دوم"),
  nl_gen2_sub = list(
    en = "Born in the Netherlands, parent(s) born in Iran",
    fa = "متولد هلند، با پدر یا مادر متولد ایران"),

  # --- population: tabs + section ------------------------------------------
  nl_tab_hist = list(
    en = "Population Over Time",
    fa = "جمعیت در طول زمان"),
  nl_tab_arrival = list(
    en = "Year of Arrival",
    fa = "سال ورود"),
  nl_geo_section_title = list(
    en = "Geographic Distribution in the Netherlands",
    fa = "توزیع جغرافیایی در هلند"),

  # --- population: charts ---------------------------------------------------
  nl_hist_title = list(
    en = "<b>Iran-Born Population in the Netherlands,<br>1990–2025</b>",
    fa = "<b>جمعیت متولدان ایران در هلند،<br>1990–2025</b>"),
  nl_hist_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br><b>%s</b> نفر متولد ایران"),
  nl_arrival_title = list(
    en = "<b>Iran-Born Residents in the Netherlands<br>by Year of Arrival</b>",
    fa = "<b>ساکنان متولد ایران در هلند<br>به تفکیک سال ورود</b>"),
  nl_arrival_hover_bar = list(
    en = "<b>%s</b><br>%s arrivals<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s نفر واردشده<br>تجمعی: %s درصد"),
  nl_arrival_hover_cum = list(
    en = "<b>%s</b><br>%s%% of today's Iran-born population had arrived",
    fa = "<b>%s</b><br>%s درصد از جمعیت کنونی متولدان ایران تا این سال وارد شده بودند"),
  nl_arrival_pre1965_label = list(
    en = "≤1965",
    fa = "تا 1965"),
  nl_axis_pct_0   = list(en = "0%",   fa = "0٪"),
  nl_axis_pct_25  = list(en = "25%",  fa = "25٪"),
  nl_axis_pct_50  = list(en = "50%",  fa = "50٪"),
  nl_axis_pct_75  = list(en = "75%",  fa = "75٪"),
  nl_axis_pct_100 = list(en = "100%", fa = "100٪"),
  nl_prov_hover = list(
    en = "<b>%s</b><br>%s Iranian-origin<br>%s%% of total",
    fa = "<b>%s</b><br>%s نفر ایرانی‌تبار<br>%s درصد از کل"),

  # --- source lines ---------------------------------------------------------
  nl_src_cbs_pop2025 = list(
    en = "Source: %s &mdash; Statistics Netherlands, Population Register 2025",
    fa = "منبع: %s &mdash; اداره آمار هلند، ثبت جمعیت 2025"),
  nl_src_cbs_pop2026 = list(
    en = "Source: %s &mdash; Statistics Netherlands, Population Register, January 2026",
    fa = "منبع: %s &mdash; اداره آمار هلند، ثبت جمعیت، ژانویه 2026"),
  nl_src_cbs_lf = list(
    en = "Source: %s &mdash; Statistics Netherlands, Labour Force Survey 2021&ndash;2024",
    fa = "منبع: %s &mdash; اداره آمار هلند، پیمایش نیروی کار 2021&ndash;2024"),
  nl_src_hist = list(
    en = "Source: %s — Iran-born population stock, 1999–2025; 1990 and 1995 from UN DESA",
    fa = "منبع: %s — جمعیت متولدان ایران، 1999–2025؛ داده‌های 1990 و 1995 از UN DESA"),
  nl_src_cbs_duration = list(
    en = "Source: %s &mdash; Statistics Netherlands, residence duration, January 2025",
    fa = "منبع: %s &mdash; اداره آمار هلند، مدت اقامت، ژانویه 2025"),
  nl_src_cbs_income = list(
    en = "Source: %s &mdash; Statistics Netherlands, household income and wealth, 2011&ndash;2024",
    fa = "منبع: %s &mdash; اداره آمار هلند، درآمد و دارایی خانوار، 2011&ndash;2024"),

  # --- workinc: page + text cards ------------------------------------------
  nl_workinc_title = list(
    en = "Netherlands: Work & Income",
    fa = "هلند: کار و درآمد"),
  nl_card_bignum = list(
    en = "%s%%",
    fa = "%s درصد"),
  nl_card1_primary = list(
    en = "of Iranian-origin residents aged 15&ndash;74 in the Netherlands are in the labour force (%s).",
    fa = "از ساکنان ایرانی‌تبار 15 تا 74 ساله هلند در نیروی کار مشارکت دارند (%s)."),
  nl_card1_bullet_perm = list(
    en = "%s%% of those employed hold permanent contracts",
    fa = "%s درصد از شاغلان قرارداد دائم دارند"),
  nl_card1_bullet_flex = list(
    en = "%s%% hold flexible contracts",
    fa = "%s درصد قرارداد موقت دارند"),
  nl_card1_bullet_self = list(
    en = "%s%% are self-employed",
    fa = "%s درصد خوداشتغال هستند"),
  nl_card2_primary = list(
    en = "of the Dutch national average household disposable income, for Iranian-origin households (%s).",
    fa = "از میانگین ملی درآمد قابل تصرف خانوار در هلند، برای خانوارهای ایرانی‌تبار (%s)."),
  nl_card2_secondary = list(
    en = "The share of Iranian-origin households below the low-income threshold has fallen from %s%% in %s to %s%% in %s.",
    fa = "سهم خانوارهای ایرانی‌تبارِ زیر آستانه کم‌درآمدی از %s درصد در سال %s به %s درصد در سال %s کاهش یافته است."),

  # --- workinc: charts ------------------------------------------------------
  nl_part_title = list(
    en = "<b>Labour Force Participation<br>in the Netherlands</b>",
    fa = "<b>مشارکت در نیروی کار<br>در هلند</b>"),
  nl_part_hover = list(
    en = "<b>%s</b><br>Participation: %s%%<br>%s employed / %s total (15–74)",
    fa = "<b>%s</b><br>مشارکت: %s درصد<br>%s شاغل از مجموع %s نفر (15–74 ساله)"),
  nl_emp_title = list(
    en = "<b>Employment Type in the Netherlands</b>",
    fa = "<b>نوع اشتغال در هلند</b>"),
  nl_emp_hover_perm = list(
    en = "<b>%s</b><br>Permanent: %s%%",
    fa = "<b>%s</b><br>قرارداد دائم: %s درصد"),
  nl_emp_hover_flex = list(
    en = "<b>%s</b><br>Flexible: %s%%",
    fa = "<b>%s</b><br>قرارداد موقت: %s درصد"),
  nl_emp_hover_self = list(
    en = "<b>%s</b><br>Self-employed: %s%%",
    fa = "<b>%s</b><br>خوداشتغال: %s درصد"),
  nl_leg_permanent = list(
    en = "Permanent",
    fa = "قرارداد دائم"),
  nl_leg_flexible = list(
    en = "Flexible",
    fa = "قرارداد موقت"),
  nl_leg_selfemp = list(
    en = "Self-employed",
    fa = "خوداشتغال"),
  nl_inc_title = list(
    en = "<b>Disposable Household Income<br>in the Netherlands (nominal € thousands)</b>",
    fa = "<b>درآمد قابل تصرف خانوار<br>در هلند (هزار یورو، به قیمت اسمی)</b>"),
  nl_inc_hover_nl = list(
    en = "<b>%s</b><br>NL average: €%s",
    fa = "<b>%s</b><br>میانگین هلند: %s یورو"),
  nl_inc_hover_iran = list(
    en = "<b>%s</b><br>Iranian-origin: €%s<br>%s%% of NL average",
    fa = "<b>%s</b><br>ایرانی‌تبار: %s یورو<br>معادل %s درصد میانگین هلند"),
  nl_leg_iranian_origin = list(
    en = "Iranian-origin",
    fa = "ایرانی‌تبار"),
  nl_leg_nl_avg = list(
    en = "NL average",
    fa = "میانگین هلند"),
  nl_lowinc_title = list(
    en = "<b>Households with Low Income<br>in the Netherlands</b>",
    fa = "<b>خانوارهای کم‌درآمد<br>در هلند</b>"),
  nl_lowinc_hover_nl = list(
    en = "<b>%s</b><br>NL average: %s%%",
    fa = "<b>%s</b><br>میانگین هلند: %s درصد"),
  nl_lowinc_hover_iran = list(
    en = "<b>%s</b><br>Iranian-origin: %s%%",
    fa = "<b>%s</b><br>ایرانی‌تبار: %s درصد"),

  # --- workinc: tabs --------------------------------------------------------
  nl_tab_participation = list(
    en = "Participation",
    fa = "مشارکت"),
  nl_tab_emptype = list(
    en = "Employment Type",
    fa = "نوع اشتغال"),
  nl_tab_income = list(
    en = "Disposable Income",
    fa = "درآمد قابل تصرف"),
  nl_tab_lowinc = list(
    en = "Low Income Share",
    fa = "سهم خانوارهای کم‌درآمد"),

  # --- axis affixes ---------------------------------------------------------
  # euro prefix fa is intentionally empty (€ becomes a يورو suffix), so the
  # builder reads it inline rather than via tr() (tr() fails loud on empty).
  nl_axis_pct_suffix  = list(en = "%",       fa = "٪"),
  nl_axis_euro_prefix = list(en = "€",  fa = ""),
  nl_axis_k_suffix    = list(en = "k",       fa = "")
)

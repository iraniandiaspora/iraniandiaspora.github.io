# ============================================================================
# R/i18n/strings_finland.R  — Finland page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_finland.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R / strings_armenia.R):
#   * en values are the EXACT English strings from build_finland.R (source of
#     truth) so the English edition stays BYTE-IDENTICAL.
#   * Every %d / format() slot from the original is rewritten as %s; the builder
#     passes fa_num()/fmtv() so the number is language-formatted.
#   * HTML tags / entities (<strong>, <br>, &mdash;, &ldquo;, en-dash) mirror
#     the en structure; only the human text is swapped in fa. Latin digits in
#     fa prose are converted at runtime by FA_NUM_SCRIPT.
#   * Official agency names (Statistics Finland, StatFin, DVV) stay Latin in
#     both editions.
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  fi_pop_title = list(
    en = "Finland: Population",
    fa = "فنلاند: جمعیت"),
  fi_pop_headline_label = list(
    en = "Estimated Iranian-Origin Population in Finland",
    fa = "جمعیت تخمینی ایرانی‌تباران در فنلاند"),
  fi_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, %s",
    fa = "بر پایه سامانه ثبت جمعیت %s، %s"),

  # --- identification box -----------------------------------------------------
  fi_pop_idbox_intro = list(
    en = "Finland uses a continuous population register. A person is classified as Iranian-origin if they meet at least one of:",
    fa = "فنلاند از یک سامانه ثبت جمعیت پیوسته استفاده می‌کند. هر فرد در صورت داشتن دست‌کم یکی از این ویژگی‌ها ایرانی‌تبار به شمار می‌آید:"),
  fi_pop_idbox_bullet1 = list(
    en = '<strong>Born in Iran</strong> <span style="color:#6b6b6b;">&mdash; recorded in the Digital and Population Data Services Agency (DVV) register</span>',
    fa = '<strong>متولد ایران</strong> <span style="color:#6b6b6b;">&mdash; ثبت‌شده در سامانه اداره خدمات دیجیتال و داده‌های جمعیتی (DVV)</span>'),
  fi_pop_idbox_bullet2 = list(
    en = '<strong>Born in Finland with at least one Iran-born parent</strong> <span style="color:#6b6b6b;">&mdash; from StatFin\'s &ldquo;background country&rdquo; classification</span>',
    fa = '<strong>متولد فنلاند با دست‌کم یکی از والدین متولد ایران</strong> <span style="color:#6b6b6b;">&mdash; بر پایه طبقه‌بندی «کشور خاستگاه» در StatFin</span>'),
  fi_pop_thirdgen_note = list(
    en = "Third-generation Finnish-born grandchildren of Iran-born grandparents are not counted as Iranian-origin in the StatFin classification.",
    fa = "نوه‌های نسل‌سوم متولد فنلاند که پدربزرگ یا مادربزرگشان متولد ایران بوده‌اند، در طبقه‌بندی StatFin ایرانی‌تبار به شمار نمی‌آیند."),

  # --- generation boxes -------------------------------------------------------
  fi_gen_box_title = list(
    en = "Iranian-Origin Population by Generation",
    fa = "جمعیت ایرانی‌تبار به تفکیک نسل"),
  fi_gen_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  fi_gen1_label = list(
    en = "First generation",
    fa = "نسل اول"),
  fi_gen1_sub = list(
    en = "Born in Iran",
    fa = "متولد ایران"),
  fi_gen2_label = list(
    en = "Second generation",
    fa = "نسل دوم"),
  fi_gen2_sub = list(
    en = "Born in Finland with Iran-born parent(s)",
    fa = "متولد فنلاند، با پدر یا مادر متولد ایران"),

  # --- trend chart ------------------------------------------------------------
  fi_hist_title = list(
    en = "<b>Iranian-Origin Population in Finland,<br>%s–%s</b>",
    fa = "<b>جمعیت ایرانی‌تباران در فنلاند،<br>%s–%s</b>"),
  fi_hist_hover = list(
    en = "<b>%s</b><br>First generation: %s<br>Second generation: %s<br>Total: %s",
    fa = "<b>%s</b><br>نسل اول: %s<br>نسل دوم: %s<br>مجموع: %s"),

  # --- region map -------------------------------------------------------------
  fi_map_hover = list(
    en = "<b>%s</b><br>%s Iran-born (%s%% of 1st gen)",
    fa = "<b>%s</b><br>%s نفر متولد ایران (%s درصد از نسل اول)"),
  fi_geo_section_title = list(
    en = "Geographic Distribution in Finland",
    fa = "توزیع جغرافیایی در فنلاند"),

  # --- source lines -----------------------------------------------------------
  fi_src_box = list(
    en = "Source: %s &mdash; Population register, %s",
    fa = "منبع: %s &mdash; سامانه ثبت جمعیت، %s"),
  fi_src_trend = list(
    en = "Source: %s &mdash; Iranian-origin population by generation, %s–%s",
    fa = "منبع: %s &mdash; جمعیت ایرانی‌تباران به تفکیک نسل، %s–%s"),
  fi_src_map = list(
    en = "Source: %s &mdash; Iran-born population by region, %s",
    fa = "منبع: %s &mdash; جمعیت متولدان ایران به تفکیک منطقه، %s")
)

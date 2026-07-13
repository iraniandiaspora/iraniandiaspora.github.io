# ============================================================================
# R/i18n/strings_turkey.R  — Türkiye pages string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_turkey.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_switzerland.R / strings_armenia.R):
#   * en values are the EXACT English strings from build_turkey.R (source of
#     truth) so the English edition stays BYTE-IDENTICAL.
#   * Every %d / format() number slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted. The
#     percent SIGN lives in the template: %% in en, the WORD «درصد» in fa
#     prose/hover (Türkiye has NO percent-axis chart, so no ٪ suffix here).
#   * HTML tags / entities (<strong>, <br>, <span>, &mdash;, –) mirror the
#     en structure; only the human text is swapped in fa. ASCII digits stay in
#     literals — FA_NUM_SCRIPT Persian-izes them in the DOM at runtime, and
#     ASCII "," thousands are kept (Iranian economic-press convention).
#   * Official agency names (Eurostat, TÜİK, UNHCR, ADNKS) stay Latin in both
#     editions. Per dashboard rules, no Eurostat table codes appear anywhere —
#     the plain terms "Iran-born" / "Iranian citizens" (متولد ایران /
#     شهروندان ایرانی) are used instead.
# ============================================================================

STR <- list(

  # --- page titles ------------------------------------------------------------
  tr_pop_title = list(
    en = "Türkiye: Population",
    fa = "ترکیه: جمعیت"),
  tr_imm_title = list(
    en = "Türkiye: Immigration",
    fa = "ترکیه: مهاجرت"),

  # --- population headline ----------------------------------------------------
  tr_pop_headline_label = list(
    en = "Estimated Iran-Born Population in Türkiye",
    fa = "برآورد جمعیت متولدان ایران در ترکیه"),
  tr_pop_headline_caption = list(
    en = "Based on the address-based population register maintained by %s, %s",
    fa = "بر پایه سامانه ثبت جمعیت مبتنی بر نشانی که توسط %s نگهداری می‌شود، %s"),

  # --- identification box ------------------------------------------------------
  tr_pop_idbox_intro = list(
    en = "Türkiye uses an address-based population register. A person is counted as Iran-born based on:",
    fa = "ترکیه از سامانه ثبت جمعیت مبتنی بر نشانی استفاده می‌کند. هر فرد بر پایه معیار زیر متولد ایران شمرده می‌شود:"),
  tr_pop_idbox_bullet1 = list(
    en = "<strong>Country of birth</strong> <span style=\"color:#6b6b6b;\">&mdash; recorded in the address-based register (ADNKS)</span>",
    fa = "<strong>کشور محل تولد</strong> <span style=\"color:#6b6b6b;\">&mdash; ثبت‌شده در سامانه ثبت مبتنی بر نشانی (ADNKS)</span>"),
  tr_pop_thirdgen_note = list(
    en = "Türkiye-born children of Iran-born parents are not counted as Iran-born in these statistics. The figure reflects first-generation residents only.",
    fa = "فرزندان متولد ترکیهِ والدین متولد ایران در این آمار متولد ایران شمرده نمی‌شوند. این رقم تنها ساکنان نسل اول را نشان می‌دهد."),
  tr_pop_un_note = list(
    en = "The UN International Migrant Stock reports about 200,000 Iran-born residents of Türkiye in 2024, higher than the figure above because UN numbers add UNHCR-registered refugees and asylum seekers to the national register and are extrapolated from 2020.",
    fa = "برآورد جمعیت مهاجران سازمان ملل شمار ساکنان متولد ایران ترکیه را در سال 2024 حدود 200,000 نفر گزارش می‌کند که از رقم بالا بیشتر است، زیرا آمار سازمان ملل پناهندگان و پناهجویان ثبت‌شده نزد UNHCR را به سامانه ثبت ملی می‌افزاید و از سال 2020 برون‌یابی شده است."),

  # --- sex breakdown boxes -----------------------------------------------------
  tr_sex_box_title = list(
    en = "Iran-Born Population by Sex",
    fa = "جمعیت متولدان ایران به تفکیک جنسیت"),
  tr_sex_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  tr_label_male = list(
    en = "Male",
    fa = "مرد"),
  tr_label_female = list(
    en = "Female",
    fa = "زن"),
  tr_sublabel_men = list(
    en = "Iran-born men",
    fa = "مردان متولد ایران"),
  tr_sublabel_women = list(
    en = "Iran-born women",
    fa = "زنان متولد ایران"),

  # --- Iran-born population trend chart ---------------------------------------
  tr_hist_title = list(
    en = "<b>Iran-Born Population in Türkiye,<br>%s–%s</b>",
    fa = "<b>جمعیت متولدان ایران در ترکیه،<br>%s–%s</b>"),
  tr_hist_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br>%s نفر متولد ایران"),

  # --- age x sex chart ---------------------------------------------------------
  tr_age_title = list(
    en = "<b>Iran-Born in Türkiye by Age and Sex</b>",
    fa = "<b>متولدان ایران در ترکیه به تفکیک سن و جنسیت</b>"),
  tr_age_under5 = list(
    en = "Under 5",
    fa = "زیر 5"),
  tr_age_hover_female = list(
    en = "<b>%s</b><br>Female: %s",
    fa = "<b>%s</b><br>زن: %s"),
  tr_age_hover_male = list(
    en = "<b>%s</b><br>Male: %s",
    fa = "<b>%s</b><br>مرد: %s"),

  # --- Iranian citizens trend chart -------------------------------------------
  tr_cit_title = list(
    en = "<b>Iranian Citizens Resident in Türkiye,<br>2014–2025</b>",
    fa = "<b>شهروندان ایرانی مقیم ترکیه،<br>2014–2025</b>"),
  tr_cit_hover = list(
    en = "<b>%s</b><br>%s Iranian citizens",
    fa = "<b>%s</b><br>%s شهروند ایرانی"),

  # --- residence-permit chart --------------------------------------------------
  tr_permits_title = list(
    en = "<b>Iranian Residence Permits in Türkiye,<br>by Type</b>",
    fa = "<b>پروانه‌های اقامت ایرانیان در ترکیه،<br>به تفکیک نوع</b>"),
  tr_permit_short = list(
    en = "Short-term",
    fa = "کوتاه‌مدت"),
  tr_permit_student = list(
    en = "Student",
    fa = "دانشجویی"),
  tr_permit_family = list(
    en = "Family",
    fa = "خانوادگی"),
  tr_permit_other = list(
    en = "Other / long-term",
    fa = "سایر / بلندمدت"),
  tr_permits_hover = list(
    en = "<b>%s</b><br>%s permits (%s%% of total)",
    fa = "<b>%s</b><br>%s پروانه (%s درصد از کل)"),

  # --- immigration text cards --------------------------------------------------
  tr_imm_cit_sentence = list(
    en = "Iranian citizens resident in Türkiye grew %s-fold between 2014 (%s) and %s (%s), their highest year.",
    fa = "شهروندان ایرانی مقیم ترکیه %s برابر شدند: از %s نفر در سال 2014 به بالاترین رقم خود در سال %s (%s نفر) رسیدند."),
  tr_imm_permits_sentence = list(
    en = "Iranian residence permits in Türkiye in May 2026 — the fourth-largest foreign group after Turkmen, Azerbaijani, and Syrian nationals.",
    fa = "پروانه‌های اقامت ایرانیان در ترکیه در مه 2026 — چهارمین گروه خارجی بزرگ پس از اتباع ترکمن، آذربایجانی و سوری."),
  tr_imm_permits_bullet1 = list(
    en = "About %s%% are short-term permits",
    fa = "حدود %s درصد پروانه‌های کوتاه‌مدت‌اند"),
  tr_imm_permits_bullet2 = list(
    en = "About %s%% are student permits",
    fa = "حدود %s درصد پروانه‌های دانشجویی‌اند"),
  tr_imm_permits_bullet3 = list(
    en = "Iranian students are the second-largest foreign-student group in Türkiye",
    fa = "دانشجویان ایرانی دومین گروه بزرگ دانشجویان خارجی در ترکیه‌اند"),

  # --- source lines ------------------------------------------------------------
  tr_src_headline = list(
    en = "Source: %s &mdash; Iran-born residents of Türkiye, 2025",
    fa = "منبع: %s &mdash; ساکنان متولد ایران ترکیه، 2025"),
  tr_src_trend = list(
    en = "Source: %s and %s &mdash; Iran-born residents of Türkiye, 2015–2025",
    fa = "منبع: %s و %s &mdash; ساکنان متولد ایران ترکیه، 2015–2025"),
  tr_src_age = list(
    en = "Source: %s &mdash; Iran-born by age and sex, 2025",
    fa = "منبع: %s &mdash; متولدان ایران به تفکیک سن و جنسیت، 2025"),
  tr_src_citizens = list(
    en = "Source: %s and %s &mdash; Iranian citizens in Türkiye, 2014–2025",
    fa = "منبع: %s و %s &mdash; شهروندان ایرانی در ترکیه، 2014–2025"),
  tr_src_permits = list(
    en = "Source: %s &mdash; Residence permits by type and nationality, 7 May 2026",
    fa = "منبع: %s &mdash; پروانه‌های اقامت به تفکیک نوع و تابعیت، 7 مه 2026")
)

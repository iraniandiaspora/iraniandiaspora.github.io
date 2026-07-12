# ============================================================================
# R/i18n/strings_global.R  — Global (world) page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_global.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(), plus two display-name lookups used only by this page:
#   GLOBAL_GROUP_KEYS  streamgraph group -> STR key (en value == the group key
#                      itself, so the English edition stays byte-identical)
#   COUNTRY_FA         choropleth destination -> Persian country name (fa only;
#                      the en path never touches it)
#
# RULES honored here (see build_nl.R / strings_nl.R):
#   * en values are the EXACT English strings from build_global.R (source of
#     truth) so the English edition stays byte-identical.
#   * Every %d / %.2f from the original is rewritten as %s; the builder passes
#     fa_num()/fmtv() so the number is language-formatted and the percent SIGN
#     lives in the template (%% in en, ٪ in fa).
#   * HTML tags / entities (<b>, <br>, &mdash;, &ndash;, &rsquo;) mirror the en
#     structure; only the human text is swapped in fa.
#   * fa hover templates bake Persian digits directly (hover labels are created
#     after the FA_NUM_SCRIPT page pass, so they must arrive already Persian).
#   * Latin agency acronyms inside fa source lines stay Latin, isolated in
#     <bdi> per the glossary convention.
# ============================================================================

STR <- list(

  # --- page shell -------------------------------------------------------------
  gl_page_title = list(
    en = "Global Overview",
    fa = "نمای جهانی"),

  # --- headline text card -----------------------------------------------------
  gl_total_label = list(
    en = "%s million",
    fa = "%s میلیون"),
  gl_card1_primary = list(
    en = "Iran-born people live outside Iran as of 2024 &mdash; nearly three times the 1990 total.",
    fa = "نفر متولد ایران تا سال ۲۰۲۴ در خارج از ایران زندگی می‌کنند &mdash; نزدیک به سه برابر رقم سال ۱۹۹۰."),

  # --- methodology text card ----------------------------------------------------
  gl_card2_heading = list(
    en = "How the United Nations counts Iranians abroad",
    fa = "سازمان ملل متحد ایرانیان خارج از کشور را چگونه می‌شمارد"),
  gl_card2_bullet1 = list(
    en = "Each country&rsquo;s total includes only people born in Iran",
    fa = "رقم هر کشور تنها افراد متولد ایران را در بر می‌گیرد"),
  gl_card2_bullet2 = list(
    en = "Drawn from each country&rsquo;s census or population register, so definitions and reference years vary",
    fa = "داده‌ها از سرشماری یا سامانه ثبت جمعیت هر کشور گرفته شده‌اند و از این رو تعریف‌ها و سال‌های مرجع یکسان نیستند"),
  gl_card2_bullet3 = list(
    en = "Second-generation Iranians (children born abroad to Iran-born parents) are not counted",
    fa = "ایرانیان نسل دوم (فرزندانی که خارج از ایران از پدر و مادر متولد ایران به دنیا آمده‌اند) شمرده نمی‌شوند"),
  gl_card2_bullet4 = list(
    en = "Some countries (notably the Persian Gulf states) do not publish country-of-birth statistics, so their Iranian populations are not reflected here",
    fa = "برخی کشورها (به‌ویژه کشورهای حاشیه خلیج فارس) آمار کشور محل تولد را منتشر نمی‌کنند و بنابراین جمعیت ایرانیان آنها در اینجا بازتاب نیافته است"),
  gl_card2_note = list(
    en = "Other pages on this site count Iranians more broadly &mdash; by ancestry, or by a parent&rsquo;s birthplace.",
    fa = "صفحات دیگر این سایت ایرانیان را با تعریفی گسترده‌تر می‌شمارند &mdash; بر پایه اصالت ایرانی یا محل تولد والدین."),

  # --- streamgraph --------------------------------------------------------------
  gl_stream_title = list(
    en = "<b>Iran-Born Migrant Population,<br>Total by Country</b>",
    fa = "<b>جمعیت مهاجران متولد ایران،<br>مجموع به تفکیک کشور</b>"),
  gl_stream_hover = list(
    en = "<b>%s</b> %s<br>%s Iran-born migrants",
    fa = "<b>%s</b> %s<br>%s مهاجر متولد ایران"),

  # --- streamgraph group display names (en == the internal group key) ----------
  gl_grp_us = list(
    en = "United States of America",
    fa = "ایالات متحده آمریکا"),
  gl_grp_ca = list(
    en = "Canada",
    fa = "کانادا"),
  gl_grp_de = list(
    en = "Germany",
    fa = "آلمان"),
  gl_grp_uk = list(
    en = "United Kingdom",
    fa = "بریتانیا"),
  gl_grp_se = list(
    en = "Sweden",
    fa = "سوئد"),
  gl_grp_nl = list(
    en = "Netherlands",
    fa = "هلند"),
  gl_grp_other_eu = list(
    en = "Other (Europe)",
    fa = "سایر کشورهای اروپا"),
  gl_grp_tr = list(
    en = "T\u00fcrkiye",
    fa = "ترکیه"),
  gl_grp_il = list(
    en = "Israel",
    fa = "اسرائیل"),
  gl_grp_iq = list(
    en = "Iraq",
    fa = "عراق"),
  gl_grp_au = list(
    en = "Australia",
    fa = "استرالیا"),
  gl_grp_other_noneu = list(
    en = "Other (Non-Europe)",
    fa = "سایر کشورهای غیراروپایی"),

  # --- world choropleth ----------------------------------------------------------
  gl_map_section_title = list(
    en = "Iranian Migrant Stock Worldwide (2024)",
    fa = "جمعیت مهاجران ایرانی در سراسر جهان (۲۰۲۴)"),
  gl_map_hover_iran = list(
    en = "<b>Iran</b><br>Estimated population: 93 million (2026)<br>Source: UN World Population Prospects 2024",
    fa = "<b>ایران</b><br>برآورد جمعیت: ۹۳ میلیون نفر (۲۰۲۶)<br>منبع: چشم‌انداز جمعیت جهان سازمان ملل ۲۰۲۴"),
  gl_map_hover = list(
    en = "<b>%s</b><br>%s Iran-born migrants",
    fa = "<b>%s</b><br>%s مهاجر متولد ایران"),

  # --- map legend swatch labels ----------------------------------------------------
  gl_leg_iran = list(
    en = "Iran",
    fa = "ایران"),
  gl_leg_bin1 = list(
    en = "1 – 1,000",
    fa = "۱ تا ۱٬۰۰۰"),
  gl_leg_bin2 = list(
    en = "1,000 – 10,000",
    fa = "۱٬۰۰۰ تا ۱۰٬۰۰۰"),
  gl_leg_bin3 = list(
    en = "10,000 – 100,000",
    fa = "۱۰٬۰۰۰ تا ۱۰۰٬۰۰۰"),
  gl_leg_bin4 = list(
    en = "100,000 – 450,000",
    fa = "۱۰۰٬۰۰۰ تا ۴۵۰٬۰۰۰"),
  gl_leg_nodata = list(
    en = "No data",
    fa = "بدون داده"),

  # --- source lines ------------------------------------------------------------
  gl_un_link_text = list(
    en = "UN International Migrant Stock (2024)",
    fa = "برآورد جمعیت مهاجران سازمان ملل (۲۰۲۴)"),
  gl_euro_link_text = list(
    en = "Eurostat",
    fa = "اداره آمار اتحادیه اروپا (<bdi>Eurostat</bdi>)"),
  gl_src_area = list(
    en = "Source: %s, supplemented by %s. Data reported at 5-year intervals (1990&ndash;2020) and 2024.<br>Based on foreign-born population data from national censuses and population registers.",
    fa = "منبع: %s، با داده‌های تکمیلی از %s. داده‌ها در بازه‌های پنج‌ساله (۱۹۹۰&ndash;۲۰۲۰) و سال ۲۰۲۴ گزارش شده‌اند.<br>بر پایه داده‌های جمعیت متولد خارج از کشور از سرشماری‌ها و سامانه‌های ثبت جمعیت ملی."),
  gl_src_map = list(
    en = "Source: %s, supplemented by %s<br>Based on foreign-born population data from national censuses and population registers.",
    fa = "منبع: %s، با داده‌های تکمیلی از %s<br>بر پایه داده‌های جمعیت متولد خارج از کشور از سرشماری‌ها و سامانه‌های ثبت جمعیت ملی.")
)

# --- streamgraph group -> STR key lookup (used by grp_label() in the builder) --
GLOBAL_GROUP_KEYS <- c(
  "United States of America" = "gl_grp_us",
  "Canada"                   = "gl_grp_ca",
  "Germany"                  = "gl_grp_de",
  "United Kingdom"           = "gl_grp_uk",
  "Sweden"                   = "gl_grp_se",
  "Netherlands"              = "gl_grp_nl",
  "Other (Europe)"           = "gl_grp_other_eu",
  "T\u00fcrkiye"             = "gl_grp_tr",
  "Israel"                   = "gl_grp_il",
  "Iraq"                     = "gl_grp_iq",
  "Australia"                = "gl_grp_au",
  "Other (Non-Europe)"       = "gl_grp_other_noneu"
)

# --- choropleth destination -> Persian country name (fa hover only) -----------
# Keys are the cleaned UN destination names (iso_map keys in build_global.R)
# plus the synthetic "Iran" row. Missing keys fall back to the English name in
# dest_label() (never NA).
COUNTRY_FA <- c(
  "United States of America"            = "ایالات متحده آمریکا",
  "Canada"                              = "کانادا",
  "Germany"                             = "آلمان",
  "United Kingdom"                      = "بریتانیا",
  "Sweden"                              = "سوئد",
  "Netherlands"                         = "هلند",
  "T\u00fcrkiye"                        = "ترکیه",
  "Israel"                              = "اسرائیل",
  "Iraq"                                = "عراق",
  "Australia"                           = "استرالیا",
  "France"                              = "فرانسه",
  "Austria"                             = "اتریش",
  "Italy"                               = "ایتالیا",
  "Norway"                              = "نروژ",
  "Denmark"                             = "دانمارک",
  "Spain"                               = "اسپانیا",
  "Belgium"                             = "بلژیک",
  "Switzerland"                         = "سوئیس",
  "Finland"                             = "فنلاند",
  "Greece"                              = "یونان",
  "Japan"                               = "ژاپن",
  "India"                               = "هند",
  "Pakistan"                            = "پاکستان",
  "Afghanistan"                         = "افغانستان",
  "Kuwait"                              = "کویت",
  "United Arab Emirates"                = "امارات متحده عربی",
  "Qatar"                               = "قطر",
  "Bahrain"                             = "بحرین",
  "Oman"                                = "عمان",
  "Saudi Arabia"                        = "عربستان سعودی",
  "Jordan"                              = "اردن",
  "Lebanon"                             = "لبنان",
  "Syrian Arab Republic"                = "سوریه",
  "Egypt"                               = "مصر",
  "Libya"                               = "لیبی",
  "South Africa"                        = "آفریقای جنوبی",
  "Brazil"                              = "برزیل",
  "Argentina"                           = "آرژانتین",
  "New Zealand"                         = "نیوزیلند",
  "Russian Federation"                  = "روسیه",
  "Ukraine"                             = "اوکراین",
  "Georgia"                             = "گرجستان",
  "Armenia"                             = "ارمنستان",
  "Azerbaijan"                          = "جمهوری آذربایجان",
  "Tajikistan"                          = "تاجیکستان",
  "Turkmenistan"                        = "ترکمنستان",
  "Uzbekistan"                          = "ازبکستان",
  "Kazakhstan"                          = "قزاقستان",
  "Kyrgyzstan"                          = "قرقیزستان",
  "China"                               = "چین",
  "Republic of Korea"                   = "کره جنوبی",
  "Malaysia"                            = "مالزی",
  "Thailand"                            = "تایلند",
  "Namibia"                             = "نامیبیا",
  "Czechia"                             = "جمهوری چک",
  "Hungary"                             = "مجارستان",
  "Poland"                              = "لهستان",
  "Romania"                             = "رومانی",
  "Bulgaria"                            = "بلغارستان",
  "Ireland"                             = "ایرلند",
  "Portugal"                            = "پرتغال",
  "Luxembourg"                          = "لوکزامبورگ",
  "Iceland"                             = "ایسلند",
  "Cyprus"                              = "قبرس",
  "State of Palestine"                  = "فلسطین",
  "Iran (Islamic Republic of)"          = "ایران",
  "Iran"                                = "ایران",
  "Mexico"                              = "مکزیک",
  "Ecuador"                             = "اکوادور",
  "Estonia"                             = "استونی",
  "Indonesia"                           = "اندونزی",
  "Slovakia"                            = "اسلواکی",
  "Slovenia"                            = "اسلوونی",
  "Malta"                               = "مالت",
  "Panama"                              = "پاناما",
  "Costa Rica"                          = "کاستاریکا",
  "Sri Lanka"                           = "سری‌لانکا",
  "Venezuela (Bolivarian Republic of)"  = "ونزوئلا",
  "Bolivia (Plurinational State of)"    = "بولیوی",
  "Lithuania"                           = "لیتوانی",
  "Belarus"                             = "بلاروس",
  "Croatia"                             = "کرواسی",
  "Latvia"                              = "لتونی",
  "Dominican Republic"                  = "جمهوری دومینیکن",
  "Guatemala"                           = "گواتمالا",
  "El Salvador"                         = "السالوادور",
  "Liechtenstein"                       = "لیختن‌اشتاین",
  "Yemen"                               = "یمن",
  "Nauru"                               = "نائورو"
)

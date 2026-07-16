# ============================================================================
# R/i18n/strings_denmark.R  — Denmark page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_denmark.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R):
#   * en values are the EXACT English strings from build_denmark.R (source of
#     truth) so the English editions stay byte-identical.
#   * Every %d / %.1f / %.0f slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted and the
#     percent SIGN lives in the template (%% in en,  درصد in fa prose).
#   * HTML tags / entities (<strong>, <br>, &mdash;) mirror the en structure;
#     only the human text is swapped in fa. Latin digits in fa prose are
#     converted at runtime by FA_NUM_SCRIPT; digits inside chart JSON (titles,
#     hover) are converted at BUILD time via htxt()/fa_num()/fmtv().
#   * Danish official product/agency names (Statistics Denmark (DST),
#     Register-based Labour Force Statistics (RAS)) are kept as-is in BOTH
#     editions — not translated.
#   * Danish region names are proper place names — kept as-is (Latin) in fa,
#     like the Netherlands province names.
# ============================================================================

STR <- list(

  # --- population: page + headline -----------------------------------------
  dk_pop_title = list(
    en = "Denmark: Population",
    fa = "دانمارک: جمعیت"),
  dk_pop_headline_label = list(
    en = "Estimated Iranian-Origin Population in Denmark",
    fa = "برآورد جمعیت ایرانی‌تبار در دانمارک"),
  dk_pop_headline_caption = list(
    en = "Based on population registers maintained by %s, %s",
    fa = "بر پایه ثبت جمعیت %s، %s"),
  dk_pop_idbox_intro = list(
    en = "Denmark uses population registers. A person is classified as Iranian-origin if they are:",
    fa = "دانمارک از ثبت جمعیت استفاده می‌کند. هر فرد در صورت داشتن یکی از این ویژگی‌ها ایرانی‌تبار طبقه‌بندی می‌شود:"),
  dk_pop_idbox_bullet1 = list(
    en = '<strong>Immigrant</strong>: born in Iran, neither parent a Danish citizen born in Denmark',
    fa = '<strong>مهاجر</strong>: متولد ایران، که هیچ‌یک از والدینش شهروند دانمارکی متولد دانمارک نیست'),
  dk_pop_idbox_bullet2 = list(
    en = '<strong>Descendant</strong>: born in Denmark, neither parent a Danish citizen born in Denmark',
    fa = '<strong>فرزند مهاجر</strong>: متولد دانمارک، که هیچ‌یک از والدینش شهروند دانمارکی متولد دانمارک نیست'),
  dk_pop_thirdgen_note = list(
    en = "The descendant category requires that neither parent is a Danish citizen born in Denmark. Third-generation (both parents Danish-born citizens) are not counted as Iranian-origin.",
    fa = "طبقه‌بندی «فرزند مهاجر» مستلزم آن است که هیچ‌یک از والدین شهروند دانمارکی متولد دانمارک نباشد. نسل سوم (که هر دو والد شهروند دانمارکی متولد دانمارک‌اند) ایرانی‌تبار به شمار نمی‌آید."),

  # --- population: generation boxes ----------------------------------------
  dk_gen_box_title = list(
    en = "Iranian-Origin Population by Generation",
    fa = "جمعیت ایرانی‌تبار به تفکیک نسل"),
  dk_gen_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  dk_gen1_label = list(
    en = "Immigrants",
    fa = "مهاجران"),
  dk_gen1_sub = list(
    en = "Born in Iran",
    fa = "متولد ایران"),
  dk_gen2_label = list(
    en = "Descendants",
    fa = "فرزندان مهاجران"),
  dk_gen2_sub = list(
    en = "Born in Denmark to Iranian parent(s)",
    fa = "متولد دانمارک با پدر یا مادر ایرانی"),

  # --- population: section title -------------------------------------------
  dk_geo_section_title = list(
    en = "Geographic Distribution in Denmark",
    fa = "توزیع جغرافیایی در دانمارک"),

  # --- population: charts ---------------------------------------------------
  dk_hist_title = list(
    en = "<b>Iranian-Origin Population in Denmark,<br>1980–2026</b>",
    fa = "<b>جمعیت ایرانی‌تبار در دانمارک،<br>1980–2026</b>"),
  dk_hist_hover_gen1 = list(
    en = "<b>%s</b><br>Immigrants: %s<br>Descendants: %s<br>Total: %s",
    fa = "<b>%s</b><br>مهاجران: %s<br>فرزندان مهاجران: %s<br>کل: %s"),
  dk_hist_hover_gen2 = list(
    en = "<b>%s</b><br>Descendants: %s",
    fa = "<b>%s</b><br>فرزندان مهاجران: %s"),
  dk_region_hover = list(
    en = "<b>%s</b><br>%s Iranian-origin<br>%s%% of total",
    fa = "<b>%s</b><br>%s نفر ایرانی‌تبار<br>%s درصد از کل"),

  # --- source lines ---------------------------------------------------------
  dk_src_pop = list(
    en = "Source: %s &mdash; Population Register, 2026",
    fa = "منبع: %s &mdash; ثبت جمعیت، 2026"),
  dk_src_emp = list(
    en = "Source: %s &mdash; Full-time employees, fourth quarter 2024",
    fa = "منبع: %s &mdash; کارکنان تمام‌وقت، سه‌ماهه چهارم 2024"),

  # --- workinc: page + industry chart --------------------------------------
  dk_workinc_title = list(
    en = "Denmark: Work & Income",
    fa = "دانمارک: کار و درآمد"),
  dk_industry_title = list(
    en = "<b>Iranian-Origin Full-Time Employees<br>by Industry in Denmark</b>",
    fa = "<b>کارکنان تمام‌وقت ایرانی‌تبار<br>به تفکیک بخش اقتصادی در دانمارک</b>"),
  dk_industry_hover = list(
    en = "<b>%s</b><br>%s employees (%s%%)",
    fa = "<b>%s</b><br>%s نفر شاغل (%s درصد)"),

  # --- workinc: text card ---------------------------------------------------
  dk_wi_bignum = list(
    en = "%s%%",
    fa = "%s درصد"),
  dk_wi_primary = list(
    en = "of Iranian-origin full-time equivalent employees in Denmark work in %s.",
    fa = "از کارکنان تمام‌وقت ایرانی‌تبار در دانمارک در بخش %s کار می‌کنند."),
  dk_wi_next = list(
    en = "Next largest sectors:",
    fa = "بخش‌های بزرگ بعدی:"),
  dk_wi_bullet = list(
    en = "%s (%s%%)",
    fa = "%s (%s درصد)"),
  dk_wi_footnote = list(
    en = "Based on Register-based Labour Force Statistics (RAS). Self-employed are not included.",
    fa = "بر پایه آمار نیروی کار مبتنی بر ثبت (Register-based Labour Force Statistics, RAS). خوداشتغالان در این آمار لحاظ نشده‌اند.")
)

# --- Industry sector display labels (fa) --------------------------------------
# The industry chart's category labels + hover + text-card sectors are common-
# noun ECONOMIC-SECTOR names (not proper nouns), so they are translated for the
# Persian edition. Keyed on the English sector label produced by build_denmark.R
# (the industry_en Danish->English map). The builder looks each row up in the
# active-language display vector; en uses the English label unchanged (byte-
# identical), fa uses the value below. "Industry" here follows the glossary's
# بخش اقتصادی (sector, incl. services) rather than صنعت (manufacturing only).
DK_SECTOR_FA <- c(
  "Agriculture & Fishing"  = "کشاورزی و ماهیگیری",
  "Mining & Quarrying"     = "معدن و استخراج",
  "Manufacturing"          = "صنعت و تولید",
  "Energy Supply"          = "تأمین انرژی",
  "Water & Waste"          = "آب و پسماند",
  "Construction"           = "ساخت‌وساز",
  "Trade & Retail"         = "بازرگانی و خرده‌فروشی",
  "Transport"              = "حمل‌ونقل",
  "Hotels & Restaurants"   = "هتل و رستوران",
  "IT & Telecom"           = "فناوری اطلاعات و ارتباطات",
  "Finance & Insurance"    = "مالی و بیمه",
  "Real Estate"            = "املاک و مستغلات",
  "Professional Services"  = "خدمات تخصصی",
  "Admin & Support"        = "خدمات اداری و پشتیبانی",
  "Public Admin & Defense" = "اداره عمومی و دفاع",
  "Education"              = "آموزش",
  "Health & Social Work"   = "بهداشت و مددکاری اجتماعی",
  "Arts & Recreation"      = "هنر و اوقات فراغت",
  "Other Services"         = "سایر خدمات"
)

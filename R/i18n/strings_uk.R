# ============================================================================
# R/i18n/strings_uk.R  — United Kingdom page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_uk.R AFTER R/_helpers_i18n.R. Defines the global STR list
# consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R / strings_denmark.R):
#   * en values are the EXACT English strings from build_uk.R (source of truth)
#     so the English editions stay byte-identical.
#   * Every %d / %.1f / %.0f from the original is rewritten as %s; the builder
#     passes fa_num()/fmtv() so the number is language-formatted. In prose,
#     cards and hovers the percent is the WORD «درصد»; on chart AXES it is the
#     sign ٪ (uk_axis_pct_suffix); the label-above qualification bars carry ٪
#     value labels (built in the builder via pct_lab()).
#   * HTML tags / entities (<strong>, <br>, &mdash;, &ldquo;) mirror the en
#     structure; only the human text is swapped in fa. Latin digits in fa PROSE
#     are converted at runtime by FA_NUM_SCRIPT; digits baked into Plotly text
#     (chart categories/hovers/annotations) are Persian-ized at build time via
#     fa_digits() in the builder.
#   * UK is an Iran-born-framing country (متولد ایران). Country/nation scope
#     names ARE translated (United Kingdom -> بریتانیا; England and Wales ->
#     انگلستان و ولز), but the 11 region-map category labels (London, North
#     West, Scotland, ...) come from the data and stay Latin. Official agency
#     names (ONS, NRS, NISRA, Scotland's Census 2022) stay Latin in the links.
# ============================================================================

STR <- list(

  # --- source lines -----------------------------------------------------------
  # ONS_CUSTOM in the original: paste0("Source: ", ONS_LINK, " — England and Wales")
  uk_src_ew = list(
    en = "Source: %s — England and Wales",
    fa = "منبع: %s — انگلستان و ولز"),
  # Map source in the original: "Source: <ONS> — Census 2021 (England and Wales); <SCOT>"
  uk_src_map = list(
    en = "Source: %s — Census 2021 (England and Wales); %s",
    fa = "منبع: %s — سرشماری 2021 (انگلستان و ولز)؛ %s"),

  # --- population: page + headline --------------------------------------------
  uk_pop_title = list(
    en = "United Kingdom: Population",
    fa = "بریتانیا: جمعیت"),
  uk_pop_headline_label = list(
    en = "Estimated Iran-Born Population in the United Kingdom",
    fa = "برآورد جمعیت متولدان ایران در بریتانیا"),
  uk_pop_headline_caption = list(
    en = "Based on the most recent %s, %s, and %s",
    fa = "بر پایه جدیدترین سرشماری‌های %s، %s و %s"),
  uk_pop_idbox_intro = list(
    en = "The UK uses a decennial census. A person is counted as Iran-born based on:",
    fa = "بریتانیا هر ده سال یک‌بار سرشماری انجام می‌دهد. هر فرد بر این اساس متولد ایران به شمار می‌آید:"),
  uk_pop_idbox_bullet1 = list(
    en = '<strong>Country of birth</strong> <span style="color:#6b6b6b;">&mdash; &ldquo;What is your country of birth?&rdquo;</span>',
    fa = '<strong>کشور محل تولد</strong> <span style="color:#6b6b6b;">&mdash; «کشور محل تولد شما کجاست؟»</span>'),
  uk_pop_thirdgen_note = list(
    en = "The UK census does not ask about ancestry or parental origin, so British-born children of Iran-born parents are not counted.",
    fa = "سرشماری بریتانیا درباره تبار یا خاستگاه والدین نمی‌پرسد، بنابراین فرزندان متولد بریتانیا با والدینِ متولد ایران شمرده نمی‌شوند."),

  # --- population: tabs -------------------------------------------------------
  uk_tab_age = list(
    en = "Age &amp; Sex",
    fa = "سن و جنسیت"),
  uk_tab_arrival = list(
    en = "Year of Arrival",
    fa = "سال ورود"),

  # --- population: age & sex chart --------------------------------------------
  uk_age_title = list(
    en = "<b>Iran-Born in England and Wales<br>by Age and Sex</b>",
    fa = "<b>متولدان ایران در انگلستان و ولز<br>به تفکیک سن و جنسیت</b>"),
  uk_age_hover_male = list(
    en = "<b>%s</b><br>Male: %s",
    fa = "<b>%s</b><br>مرد: %s"),
  uk_age_hover_female = list(
    en = "<b>%s</b><br>Female: %s",
    fa = "<b>%s</b><br>زن: %s"),
  uk_leg_male = list(
    en = "Male",
    fa = "مرد"),
  uk_leg_female = list(
    en = "Female",
    fa = "زن"),

  # --- population: year-of-arrival chart --------------------------------------
  uk_arrival_title = list(
    en = "<b>Iran-Born Residents in the UK<br>by Year of Arrival</b>",
    fa = "<b>ساکنان متولد ایران در بریتانیا<br>به تفکیک سال ورود</b>"),
  # slots: cohort label, arrivals count, per-year avg, cumulative %
  uk_arrival_hover_bar = list(
    en = "<b>%s</b><br>%s arrivals (%s/year avg)<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s نفر واردشده (میانگین %s در سال)<br>تجمعی: %s درصد"),
  uk_arrival_hover_cum = list(
    en = "<b>%s</b><br>%s%% of today's Iran-born population had arrived",
    fa = "<b>%s</b><br>%s درصد از جمعیت کنونی متولدان ایران تا این سال وارد شده بودند"),
  uk_arrival_footnote = list(
    en = "Census cohorts span different numbers of years; bars show arrivals averaged per year.",
    fa = "دوره‌های سرشماری بازه‌های زمانی متفاوتی را دربر می‌گیرند؛ ستون‌ها میانگین سالانه ورود را نشان می‌دهند."),

  # --- axis affix (chart AXES use the sign ٪, not the word) -------------------
  uk_axis_pct_suffix = list(
    en = "%",
    fa = "٪"),

  # --- population: religion chart ---------------------------------------------
  uk_relig_title = list(
    en = "<b>Religion of Iran-Born Residents<br>in England and Wales</b>",
    fa = "<b>دین ساکنان متولد ایران<br>در انگلستان و ولز</b>"),

  # --- shared horizontal-bar hover (religion / economic activity / qualification)
  # slots: category, count, share%
  uk_bar_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s (%s درصد)"),

  # --- population: geographic distribution ------------------------------------
  uk_geo_section_title = list(
    en = "Geographic Distribution in the United Kingdom",
    fa = "پراکندگی جغرافیایی در بریتانیا"),
  # slots: region name (Latin), count, share of UK total%
  uk_map_hover = list(
    en = "<b>%s</b><br>%s Iran-born residents<br>%s%% of UK total",
    fa = "<b>%s</b><br>%s نفر متولد ایران<br>%s درصد از کل بریتانیا"),

  # --- workedu: page + charts -------------------------------------------------
  uk_workedu_title = list(
    en = "United Kingdom: Work & Education",
    fa = "بریتانیا: کار و آموزش"),
  uk_econ_title = list(
    en = "<b>Economic Activity of Iran-Born<br>Residents in England and Wales</b>",
    fa = "<b>وضعیت فعالیت اقتصادی ساکنان<br>متولد ایران در انگلستان و ولز</b>"),
  uk_qual_title = list(
    en = "<b>Highest Qualification of Iran-Born<br>Residents in England and Wales</b>",
    fa = "<b>بالاترین مدرک تحصیلی ساکنان<br>متولد ایران در انگلستان و ولز</b>"),

  # --- workedu: text cards ----------------------------------------------------
  uk_card_bignum = list(
    en = "%s%%",
    fa = "%s درصد"),
  uk_card1_primary = list(
    en = "of economically active Iran-born residents aged 16+ in England and Wales are employed.",
    fa = "از ساکنان متولد ایرانِ فعال اقتصادی 16 سال به بالا در انگلستان و ولز شاغل‌اند."),
  uk_card1_secondary = list(
    en = "Another %s%% of the Iran-born 16+ population are economically inactive (including %s%% students).",
    fa = "%s درصد دیگر از جمعیت متولد ایرانِ 16 سال به بالا از نظر اقتصادی غیرفعال‌اند (از جمله %s درصد دانشجو)."),
  uk_card2_primary = list(
    en = "of Iran-born residents aged 16+ in England and Wales hold a bachelor&rsquo;s degree or higher.",
    fa = "از ساکنان متولد ایران 16 سال به بالا در انگلستان و ولز لیسانس یا مدرک بالاتر دارند."),
  uk_card2_secondary = list(
    en = "About %s%% report no formal qualifications.",
    fa = "حدود %s درصد هیچ مدرک تحصیلی رسمی ندارند.")
)

# ============================================================================
# Category-label display maps (fa). Keyed on the EXACT English category value
# in the source CSVs; the builder looks each row up in the active-language
# display vector (en uses the English label unchanged -> byte-identical, fa
# uses the value below). Labels destined for Plotly text carry ASCII digits
# here; the builder wraps them in fa_digits() so the digits are Persian at
# build time (FA_NUM_SCRIPT can't reach text baked inside <script>).
# UK exam names (GCSE, O level, A level) stay Latin — no Persian equivalent.
# ============================================================================

UK_AGE_FA <- c(
  "15 years and under" = "15 سال و کمتر",
  "16 to 24 years"     = "16 تا 24 سال",
  "25 to 34 years"     = "25 تا 34 سال",
  "35 to 49 years"     = "35 تا 49 سال",
  "50 to 64 years"     = "50 تا 64 سال",
  "65 years and over"  = "65 سال و بیشتر"
)

UK_REL_FA <- c(
  "No religion"    = "بدون دین",
  "Christian"      = "مسیحی",
  "Buddhist"       = "بودایی",
  "Hindu"          = "هندو",
  "Jewish"         = "یهودی",
  "Muslim"         = "مسلمان",
  "Sikh"           = "سیک",
  "Other religion" = "دین دیگر",
  "Not answered"   = "بی‌پاسخ"
)

UK_ECON_FA <- c(
  "Employed"   = "شاغل",
  "Unemployed" = "بیکار",
  "Inactive"   = "غیرفعال اقتصادی",
  "Student"    = "دانشجو"
)

UK_QUAL_FA <- c(
  "No qualifications"            = "بدون مدرک تحصیلی",
  "Level 1 (GCSEs)"              = "سطح 1 (GCSE)",
  "Level 2 (5+ GCSEs / O levels)" = "سطح 2 (پنج GCSE یا بیشتر / O level)",
  "Level 3 (A levels)"           = "سطح 3 (A level)",
  "Level 4+ (Degree or higher)"  = "سطح 4 به بالا (لیسانس یا بالاتر)",
  "Other qualifications"         = "مدارک دیگر"
)

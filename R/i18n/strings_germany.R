# ============================================================================
# R/i18n/strings_germany.R  — Germany page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_germany.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(), plus a set of fa-only display lookup vectors (keyed on
# the English category label the builder produces) for the repeated category
# sets — motive, school/vocational qualification, labour-force status,
# citizenship, industry sector, income bracket, language category, generation.
#
# RULES honored here (same as strings_nl.R / strings_denmark.R):
#   * en values are the EXACT English strings from build_germany.R (source of
#     truth) so the four English editions stay BYTE-IDENTICAL.
#   * Every %d / %.1f / %.0f slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted, and
#     the percent SIGN lives in the template (%% in en,  درصد in fa).
#   * A literal "%" in the original prose (e.g. "an annual 1% household survey",
#     "do not sum to 100%") is escaped as "%%" in the en TEMPLATE because the
#     builder now runs it through sprintf(); the fa value uses the word درصد so
#     it needs no escape.
#   * HTML tags / entities (<strong>, <em>, <br>, &mdash;, &ldquo;, &rdquo;)
#     mirror the en structure; only the human text is swapped in fa. ASCII
#     digits baked into fa literals are Persian-ized at runtime by FA_NUM_SCRIPT
#     (and by htxt() where the builder pre-digits assembled prose/hover).
#   * Official agency / programme names stay Latin (Destatis, BAMF,
#     Mikrozensus, Migrationsberichte, Zuzüge, Abitur, Bundesland place names).
#   * Prose / cards / hover use the WORD «درصد»; axis tick suffixes use ٪
#     (handled in the builder, not here).
#
# TERMS: Iranian-origin = ایرانی‌تبار; Iran-born = متولد ایران; first/second
# generation = نسل اول / نسل دوم; Germany = آلمان.
# ============================================================================

# --- fa-only category display vectors (keyed on the English label) -----------
# Migration motive (de-immigration motive chart).
DE_MOTIVE_FA <- c(
  "Flight / asylum"      = "فرار و پناهندگی",
  "Family reunification" = "پیوستن به خانواده",
  "Family formation"     = "تشکیل خانواده",
  "Study / training"     = "تحصیل و آموزش",
  "Work"                 = "کار",
  "Other"                = "سایر"
)

# Highest school qualification (de-langedu school chart). Line breaks kept.
DE_SCHOOL_FA <- c(
  "No school<br>certificate"       = "بدون مدرک<br>تحصیلی",
  "Basic<br>secondary"             = "متوسطه<br>پایه",
  "Intermediate<br>secondary"      = "متوسطه<br>میانی",
  "Applied-university<br>entrance" = "ورود به دانشگاه<br>کاربردی",
  "University<br>entrance"         = "ورود به<br>دانشگاه"
)

# Vocational / academic qualification (de-langedu prof chart).
DE_PROF_FA <- c(
  "No vocational qualification" = "بدون مدرک حرفه‌ای",
  "Vocational (non-academic)"   = "حرفه‌ای (غیردانشگاهی)",
  "Academic degree"             = "مدرک دانشگاهی"
)

# Labour-force status (de-workinc status chart).
DE_STATUS_FA <- c(
  "Employed"           = "شاغل",
  "Unemployed"         = "بیکار",
  "Not in labor force" = "خارج از نیروی کار"
)

# Citizenship status (de-immigration citizenship chart).
DE_CITIZEN_FA <- c(
  "German citizens"   = "شهروندان آلمان",
  "Iranian nationals" = "اتباع ایران"
)

# Industry sector (de-workinc industry chart). Sector = بخش اقتصادی (incl.
# services), following the glossary, not صنعت (manufacturing only).
DE_INDUSTRY_FA <- c(
  "Agriculture & fishing"          = "کشاورزی و ماهیگیری",
  "Industry & construction"        = "صنعت و ساخت‌وساز",
  "Trade, hospitality & transport" = "بازرگانی، پذیرایی و حمل‌ونقل",
  "Public administration"          = "اداره عمومی",
  "Other services"                 = "سایر خدمات"
)

# Monthly net income bracket (de-workinc income chart). € becomes a یورو
# suffix; ASCII digits stay (runtime Persian-ized); en-dash range -> "تا".
DE_INCBRACKET_FA <- c(
  "Under €500"                        = "زیر 500 یورو",
  "€500–€1,000"             = "500 تا 1,000 یورو",
  "€1,000–€1,500"           = "1,000 تا 1,500 یورو",
  "€1,500–€2,000"           = "1,500 تا 2,000 یورو",
  "€2,000–€2,500"           = "2,000 تا 2,500 یورو",
  "€2,500–€3,000"           = "2,500 تا 3,000 یورو",
  "€3,000–€3,500"           = "3,000 تا 3,500 یورو",
  "€3,500+"                           = "بیش از 3,500 یورو"
)

# Main home language category (de-langedu language chart).
DE_LANGCAT_FA <- c(
  "German only"              = "فقط آلمانی",
  "Mostly German"            = "بیشتر آلمانی",
  "Mostly Persian"           = "بیشتر فارسی",
  "Mostly other non-German"  = "بیشتر زبان‌های غیرآلمانی دیگر"
)

# Generation split label used on the language chart's y-axis + hover.
DE_GENLABEL_FA <- c(
  "All Iranian-origin"     = "همه ایرانی‌تباران",
  "First generation only"  = "فقط نسل اول"
)

STR <- list(

  # ===========================================================================
  # SHARED SOURCE LINES
  # ===========================================================================
  de_src_mz = list(
    en = "Source: %s — Mikrozensus 2025 Erstergebnisse<br>German household survey. Counts residents with migration background (first or second generation), Iran.",
    fa = "منبع: %s — Mikrozensus 2025 Erstergebnisse<br>پیمایش خانوار آلمان. ساکنان دارای پیشینه مهاجرت (نسل اول یا دوم)، ایران، را می‌شمارد."),
  de_src_mz_simple = list(
    en = "Source: %s — Mikrozensus 2025",
    fa = "منبع: %s — Mikrozensus 2025"),

  # ===========================================================================
  # DE-POPULATION
  # ===========================================================================
  de_pop_title = list(
    en = "Germany: Population",
    fa = "آلمان: جمعیت"),
  de_pop_headline_label = list(
    en = "Estimated Iranian-Origin Population in Germany",
    fa = "برآورد جمعیت ایرانی‌تبار در آلمان"),
  de_pop_headline_caption = list(
    en = "Based on the %s, an annual 1%% household survey by Destatis, the German Federal Statistical Office",
    fa = "بر پایه %s، پیمایشی سالانه که حدود 1 درصد خانوارها را دربر می‌گیرد و توسط Destatis، اداره فدرال آمار آلمان، انجام می‌شود"),
  de_pop_idbox_intro = list(
    en = "A person is counted if they meet <em>at least one</em> of two survey questions:",
    fa = "هر فرد در صورت داشتن <em>دست‌کم یکی</em> از این دو پاسخ در پرسش‌های سرشماری شمرده می‌شود:"),
  de_pop_idbox_bullet1 = list(
    en = '<strong>Place of birth</strong> <span style="color:#6b6b6b;">&mdash; &ldquo;In which country were you born?&rdquo; (Iran)</span>',
    fa = '<strong>محل تولد</strong> <span style="color:#6b6b6b;">&mdash; «شما در کدام کشور متولد شده‌اید؟» (ایران)</span>'),
  de_pop_idbox_bullet2 = list(
    en = '<strong>Parental origin</strong> <span style="color:#6b6b6b;">&mdash; &ldquo;In which country was your mother / father born?&rdquo; (Iran, for at least one parent)</span>',
    fa = '<strong>پیشینه والدین</strong> <span style="color:#6b6b6b;">&mdash; «مادر / پدر شما در کدام کشور متولد شده است؟» (ایران، برای دست‌کم یکی از والدین)</span>'),

  # --- generation boxes ------------------------------------------------------
  de_gen_box_title = list(
    en = "Iranian-Origin Population by Generation",
    fa = "جمعیت ایرانی‌تبار به تفکیک نسل"),
  de_gen_pct_of_total = list(
    en = "%s%% of total",
    fa = "%s درصد از کل"),
  de_gen1_label = list(
    en = "First generation",
    fa = "نسل اول"),
  de_gen1_sub = list(
    en = "Born in Iran",
    fa = "متولد ایران"),
  de_gen2_label = list(
    en = "Second generation",
    fa = "نسل دوم"),
  de_gen2_sub = list(
    en = "Born in Germany with at least one Iran-born parent",
    fa = "متولد آلمان با دست‌کم یک والد متولد ایران"),

  # --- bar + map charts ------------------------------------------------------
  de_pop_bar_title = list(
    en = "<b>Iranian-Origin Population<br>by German State</b>",
    fa = "<b>جمعیت ایرانی‌تبار<br>به تفکیک ایالت آلمان</b>"),
  de_pop_map_section = list(
    en = "Geographic Distribution in Germany",
    fa = "توزیع جغرافیایی در آلمان"),
  de_hover_suppressed = list(
    en = "<b>%s</b><br>Suppressed (<5,000)<br>Destatis does not publish counts below 5,000",
    fa = "<b>%s</b><br>منتشرنشده (کمتر از 5,000)<br>Destatis ارقام کمتر از 5,000 را منتشر نمی‌کند"),
  de_hover_cat_pct = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s (%s درصد)"),
  de_hover_map = list(
    en = "<b>%s</b><br>%s Iranian-origin residents<br>%s%% of Germany total",
    fa = "<b>%s</b><br>%s ساکن ایرانی‌تبار<br>%s درصد از کل آلمان"),
  de_src_bundbar_note = list(
    en = "Six states with fewer than 5,000 Iranian-origin residents are suppressed by Destatis (shown in gray).",
    fa = "شش ایالت با کمتر از 5,000 ساکن ایرانی‌تبار توسط Destatis منتشر نشده‌اند (به رنگ خاکستری)."),
  de_src_map_note = list(
    en = "Gray states: suppressed (<5,000). Published state counts sum to ~308,000 of the 334,000 national total.",
    fa = "ایالت‌های خاکستری: منتشرنشده (کمتر از 5,000). مجموع ارقام منتشرشده ایالتی حدود 308,000 از کل ملی 334,000 است."),

  # ===========================================================================
  # DE-IMMIGRATION & CITIZENSHIP
  # ===========================================================================
  de_immig_title = list(
    en = "Germany: Immigration & Citizenship",
    fa = "آلمان: مهاجرت و تابعیت"),

  # --- charts ----------------------------------------------------------------
  de_motive_title = list(
    en = "<b>Main Reason Iran-Born Immigrants<br>Came to Germany</b>",
    fa = "<b>دلیل اصلی مهاجرت متولدان ایران<br>به آلمان</b>"),
  de_duration_title = list(
    en = "<b>Iran-Born Residents by<br>Length of Residence in Germany</b>",
    fa = "<b>ساکنان متولد ایران<br>به تفکیک مدت اقامت در آلمان</b>"),
  de_duration_xaxis = list(
    en = "Years Lived in Germany",
    fa = "سال‌های اقامت در آلمان"),
  de_duration_annot = list(
    en = "Bars run from earliest arrivals (left) to most recent (right).",
    fa = "میله‌ها از قدیمی‌ترین ورودها (چپ) تا جدیدترین (راست) چیده شده‌اند."),
  de_hover_duration = list(
    en = "<b>%s years</b><br>%s (%s%%)",
    fa = "<b>%s سال</b><br>%s (%s درصد)"),
  de_annual_title = list(
    en = "<b>Annual Iranian Arrivals to Germany,<br>1991–2024</b>",
    fa = "<b>ورود سالانه ایرانیان به آلمان،<br>1991–2024</b>"),
  de_hover_annual = list(
    en = "<b>%s</b><br>%s Iranian arrivals",
    fa = "<b>%s</b><br>%s ورودِ ایرانی"),
  de_citizen_title = list(
    en = "<b>Citizenship of Iranian-Origin<br>Residents in Germany</b>",
    fa = "<b>تابعیت ساکنان ایرانی‌تبار<br>در آلمان</b>"),

  # --- text cards ------------------------------------------------------------
  de_bignum = list(
    en = "%s%%",
    fa = "%s درصد"),
  de_immig_c1_primary = list(
    en = "of Iranian-origin residents in Germany hold German citizenship &mdash; about %s people.",
    fa = "از ساکنان ایرانی‌تبار در آلمان تابعیت آلمان را دارند &mdash; حدود %s نفر."),
  de_immig_c1_b1 = list(
    en = "About %s hold Iranian citizenship",
    fa = "حدود %s نفر تابعیت ایران را دارند"),
  de_immig_c1_b2 = list(
    en = "7,840 Iranian naturalizations were recorded in 2024",
    fa = "در سال 2024، 7,840 مورد کسب تابعیت آلمان توسط ایرانیان ثبت شد"),
  de_immig_c2_primary = list(
    en = "of first-generation Iranian residents have lived in Germany for a decade or more.",
    fa = "از ساکنان ایرانیِ نسل اول یک دهه یا بیشتر در آلمان زندگی کرده‌اند."),
  de_immig_c2_b1 = list(
    en = "Roughly %s%% have lived in Germany for 30 or more years",
    fa = "حدود %s درصد سی سال یا بیشتر در آلمان زندگی کرده‌اند"),

  # --- tabs ------------------------------------------------------------------
  de_tab_annual = list(
    en = "Annual Arrivals",
    fa = "ورود سالانه"),
  de_tab_motive = list(
    en = "Main Reason for Coming",
    fa = "دلیل اصلی مهاجرت"),
  de_tab_duration = list(
    en = "Length of Residence",
    fa = "مدت اقامت"),

  # --- source lines ----------------------------------------------------------
  de_src_annual = list(
    en = "Source: BAMF Migrationsberichte (2005, 2015, 2020, 2023, 2024 editions). Annual Iranian Zuz&#252;ge (arrivals to Germany) as reported in the official migration flow statistics. Pre-1991 data exists only for West Germany and is not included here.",
    fa = "منبع: BAMF Migrationsberichte (ویرایش‌های 2005، 2015، 2020، 2023، 2024). ورودی‌های سالانه ایرانیان (Zuz&#252;ge، ورود به آلمان) بر پایه آمار رسمی جریان مهاجرت. داده‌های پیش از 1991 تنها برای آلمان غربی موجود است و اینجا لحاظ نشده است."),
  de_src_motive = list(
    en = "Source: %s — Mikrozensus 2025. First generation only (born in Iran).",
    fa = "منبع: %s — Mikrozensus 2025. تنها نسل اول (متولد ایران)."),
  de_src_duration = list(
    en = "Source: %s — Mikrozensus 2025. Iran-born residents only.",
    fa = "منبع: %s — Mikrozensus 2025. تنها ساکنان متولد ایران."),

  # ===========================================================================
  # DE-WORK & INCOME
  # ===========================================================================
  de_workinc_title = list(
    en = "Germany: Work & Income",
    fa = "آلمان: کار و درآمد"),

  # --- charts ----------------------------------------------------------------
  de_empstatus_title = list(
    en = "<b>Labor Force Status in Germany</b>",
    fa = "<b>وضعیت اشتغال در آلمان</b>"),
  de_industry_title = list(
    en = "<b>Iranian-Origin Employment<br>by Industry in Germany</b>",
    fa = "<b>اشتغال ایرانی‌تباران<br>به تفکیک بخش اقتصادی در آلمان</b>"),
  de_hover_industry = list(
    en = "<b>%s</b><br>%s (%s%% of all employed)",
    fa = "<b>%s</b><br>%s (%s درصد از کل شاغلان)"),
  de_income_title = list(
    en = "<b>Monthly Net Income of<br>Iranian-Origin Workers in Germany</b>",
    fa = "<b>درآمد خالص ماهانه<br>شاغلان ایرانی‌تبار در آلمان</b>"),
  de_hover_income = list(
    en = "<b>%s per month</b><br>%s workers (%s%%)",
    fa = "<b>%s در ماه</b><br>%s شاغل (%s درصد)"),
  de_hover_income_suppressed = list(
    en = "<b>%s per month</b><br>Suppressed (<5,000)<br>Destatis does not publish counts below 5,000",
    fa = "<b>%s در ماه</b><br>منتشرنشده (کمتر از 5,000)<br>Destatis ارقام کمتر از 5,000 را منتشر نمی‌کند"),

  # --- text cards ------------------------------------------------------------
  de_wi_c1_primary = list(
    en = "of employed Iranian-origin residents work in the broad services sector (trade, hospitality, transport, and other services).",
    fa = "از ساکنان شاغل ایرانی‌تبار در بخش گسترده خدمات (بازرگانی، پذیرایی، حمل‌ونقل و سایر خدمات) کار می‌کنند."),
  de_wi_c1_b1 = list(
    en = "%s%% of all Iranian-origin residents aged 15+ are employed",
    fa = "%s درصد از همه ساکنان ایرانی‌تبار 15 سال به بالا شاغل‌اند"),
  de_wi_c1_b2 = list(
    en = "%s%% are unemployed",
    fa = "%s درصد بیکارند"),
  de_wi_c1_b3 = list(
    en = "%s%% are not in the labor force (students, retirees, caregivers)",
    fa = "%s درصد خارج از نیروی کار هستند (دانشجویان، بازنشستگان، مراقبان)"),
  de_wi_c2_primary = list(
    en = "of employed Iranian-origin residents earn between €1,000 and €2,500 net per month.",
    fa = "از ساکنان شاغل ایرانی‌تبار، درآمد خالص ماهانه‌ای بین 1,000 تا 2,500 یورو دارند."),
  de_wi_c2_b1 = list(
    en = "About %s%% earn €3,000 or more per month",
    fa = "حدود %s درصد ماهانه 3,000 یورو یا بیشتر درآمد دارند"),
  de_wi_c2_b2 = list(
    en = "About %s%% earn under €1,000",
    fa = "حدود %s درصد کمتر از 1,000 یورو درآمد دارند"),

  # --- tabs ------------------------------------------------------------------
  de_tab_status = list(
    en = "Labor Force Status",
    fa = "وضعیت اشتغال"),
  de_tab_industry = list(
    en = "Employment by Industry",
    fa = "اشتغال به تفکیک بخش"),

  # --- source lines ----------------------------------------------------------
  de_src_industry = list(
    en = "Source: %s — Mikrozensus 2025. Iranian-origin residents employed across five broad sectors. Agriculture and Public administration are suppressed by Destatis (shown in gray). Percentages are computed against the full published employed total (~175,000), so visible bars do not sum to 100%%.",
    fa = "منبع: %s — Mikrozensus 2025. ساکنان ایرانی‌تبارِ شاغل در پنج بخش گسترده اقتصادی. بخش‌های کشاورزی و اداره عمومی توسط Destatis منتشر نشده‌اند (به رنگ خاکستری). درصدها نسبت به کل شاغلان منتشرشده (حدود 175,000 نفر) محاسبه شده‌اند، بنابراین میله‌های نمایان جمعاً 100 درصد نمی‌شوند."),
  de_src_income = list(
    en = "Source: %s — Mikrozensus 2025. Net monthly personal income of employed Iranian-origin residents, all generations combined. The under-€500 bracket is suppressed by Destatis (<5,000), shown in gray.",
    fa = "منبع: %s — Mikrozensus 2025. درآمد خالص شخصیِ ماهانه ساکنان شاغل ایرانی‌تبار، همه نسل‌ها با هم. طبقه زیر 500 یورو توسط Destatis منتشر نشده است (کمتر از 5,000)، به رنگ خاکستری."),

  # ===========================================================================
  # DE-LANGUAGE & EDUCATION
  # ===========================================================================
  de_langedu_title = list(
    en = "Germany: Language & Education",
    fa = "آلمان: زبان و تحصیلات"),

  # --- charts ----------------------------------------------------------------
  de_lang_title = list(
    en = "<b>Main Language Spoken at Home in Germany</b>",
    fa = "<b>زبان اصلی گفت‌وگو در خانه در آلمان</b>"),
  de_hover_lang = list(
    en = "<b>%s</b><br>%s<br>%s residents (%s%%)",
    fa = "<b>%s</b><br>%s<br>%s ساکن (%s درصد)"),
  de_school_title = list(
    en = "<b>Highest School Qualification in Germany</b>",
    fa = "<b>بالاترین مدرک تحصیلی مدرسه در آلمان</b>"),
  de_prof_title = list(
    en = "<b>Highest Vocational or Academic<br>Qualification in Germany</b>",
    fa = "<b>بالاترین مدرک حرفه‌ای یا دانشگاهی<br>در آلمان</b>"),

  # --- text cards ------------------------------------------------------------
  de_le_c1_primary = list(
    en = "of Iranian-origin residents speak Persian as the main language at home &mdash; about %s people.",
    fa = "از ساکنان ایرانی‌تبار، فارسی را زبان اصلی خانه می‌دانند &mdash; حدود %s نفر."),
  de_le_c1_b1 = list(
    en = "%s%% speak only German at home",
    fa = "%s درصد در خانه فقط آلمانی صحبت می‌کنند"),
  de_le_c1_b2 = list(
    en = "%s%% speak mainly German with some other languages",
    fa = "%s درصد بیشتر آلمانی همراه با برخی زبان‌های دیگر صحبت می‌کنند"),
  de_le_c1_b3 = list(
    en = "%s%% speak another non-German, non-Persian language at home",
    fa = "%s درصد در خانه زبانی دیگر، غیر از آلمانی و فارسی، صحبت می‌کنند"),
  de_le_c1_b4 = list(
    en = "Including Kurdish, English, and Other Asian, each about %s%%",
    fa = "از جمله کُردی، انگلیسی و سایر زبان‌های آسیایی، هرکدام حدود %s درصد"),
  de_le_c2_primary = list(
    en = "of Iranian-origin residents in Germany hold the Abitur.",
    fa = "از ساکنان ایرانی‌تبار در آلمان دارای دیپلم آبیتور (Abitur) هستند."),
  de_le_c2_b1 = list(
    en = "The Abitur is the secondary-school qualification that grants university entry",
    fa = "آبیتور (Abitur) مدرک پایان دبیرستان است که اجازه ورود به دانشگاه را می‌دهد"),
  de_le_c2_b2 = list(
    en = "About %s%% (~%s) hold an academic degree",
    fa = "حدود %s درصد (~%s نفر) مدرک دانشگاهی دارند"),

  # --- tabs ------------------------------------------------------------------
  de_tab_school = list(
    en = "Highest School Qualification",
    fa = "بالاترین مدرک مدرسه"),
  de_tab_prof = list(
    en = "Vocational / Academic Qualification",
    fa = "مدرک حرفه‌ای / دانشگاهی")
)

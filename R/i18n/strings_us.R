# ============================================================================
# R/i18n/strings_us.R  — United States page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_us.R AFTER R/_helpers_i18n.R. Defines the global STR list
# consumed by tr(), plus fa-only display lookup vectors (keyed on the English
# category label the builder produces) for the repeated category sets —
# waterfall components, citizenship, admissions category, education level, work
# class, spouse ethnicity, language category, self-employment origin, occupation
# group, business sector, top-other language.
#
# RULES honored here (same as strings_germany.R / strings_nl.R):
#   * en values are the EXACT English strings from build_us.R (source of truth)
#     so the eight English editions stay BYTE-IDENTICAL.
#   * Every %d / %.1f / %.0f slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted, and
#     the percent SIGN lives in the template (%% in en,  درصد in fa).
#   * A literal "%" in the original prose (e.g. "10%") is escaped as "%%" in the
#     en TEMPLATE because the builder runs it through sprintf(); the fa value
#     uses the word درصد so it needs no escape.
#   * HTML tags / entities (<strong>, <em>, <br>, &mdash;, &ldquo;, &rdquo;,
#     &ndash;, &rsquo;) mirror the en structure; only the human text is swapped.
#     ASCII digits baked into fa literals are Persian-ized at runtime by
#     FA_NUM_SCRIPT (and by htxt() where the builder pre-digits assembled prose).
#   * Official agency / dataset names stay Latin (U.S. Census Bureau, ACS,
#     IPUMS, DHS, INS, PUMS, IRCA). US state / Census-region place names stay
#     Latin. Language names are Persian.
#   * Prose / cards / hover use the WORD «درصد»; axis tick suffixes + on-bar %
#     value labels use ٪ (handled in the builder, not here).
#
# TERMS: Iranian-American = ایرانی‌آمریکایی; Iranian ancestry = اصالت ایرانی;
# Iran-born = متولد ایران; first/second generation = نسل اول / نسل دوم;
# race write-in = خوداظهاری نژادی; United States / US = آمریکا.
# ============================================================================

# --- fa-only category display vectors (keyed on the English label) -----------

# US-POPULATION: waterfall component names (hover only).
US_WATERFALL_FA <- c(
  "Born in Iran + Ancestry + Race write-in" = "متولد ایران + اصالت + خوداظهاری نژادی",
  "Born in Iran + Ancestry"                 = "متولد ایران + اصالت",
  "Born in Iran + Race write-in"            = "متولد ایران + خوداظهاری نژادی",
  "Ancestry + Race write-in"                = "اصالت + خوداظهاری نژادی",
  "Born in Iran only"                       = "فقط متولد ایران",
  "Ancestry only"                           = "فقط اصالت",
  "Race write-in only"                      = "فقط خوداظهاری نژادی",
  "US-born child of Iranian parent"         = "فرزند متولد آمریکا با والد ایرانی"
)

# US-IMMIGRATION: citizenship status (chart x-axis + hover).
US_CITIZEN_FA <- c(
  "Naturalized citizen" = "شهروند با تابعیت اکتسابی",
  "Born in the US"      = "متولد آمریکا",
  "Not a citizen"       = "غیرشهروند"
)

# US-ADMISSIONS: green-card category (legend + hover).
US_ADMCAT_FA <- c(
  "Family"         = "خانوادگی",
  "Employment"     = "کاری",
  "Refugee/Asylee" = "پناهنده/پناهجو",
  "Diversity"      = "تنوع (لاتاری)",
  "Other"          = "سایر"
)

# US-EDUCATION: education level (butterfly legend + hover).
US_EDUC_FA <- c(
  "Less than BA degree" = "کمتر از لیسانس",
  "BA degree"           = "لیسانس",
  "Graduate degree"     = "تحصیلات تکمیلی"
)

# US-WORK: employment class (butterfly legend + hover).
US_WORKCLASS_FA <- c(
  "Private sector employee" = "کارمند بخش خصوصی",
  "Public sector employee"  = "کارمند بخش دولتی",
  "Non-profit employee"     = "کارمند بخش غیرانتفاعی",
  "Self-employed"           = "خوداشتغال",
  "No work in last 5 years" = "بدون کار در 5 سال گذشته"
)

# US-MARRIAGE: spouse / partner ethnicity (butterfly legend + hover).
US_SPOUSE_FA <- c(
  "Iranian"                            = "ایرانی",
  "Middle Eastern (Non-Iranian)"       = "خاورمیانه‌ای (غیرایرانی)",
  "White (Non-Iranian, Non-Hispanic)"  = "سفیدپوست (غیرایرانی، غیرلاتین‌تبار)",
  "Hispanic"                           = "لاتین‌تبار",
  "Asian, Black, Native American"      = "آسیایی، سیاه‌پوست، بومی آمریکا"
)

# US-LANGUAGE: language-at-home category (stacked-bar legend + hover).
US_LANGCAT_FA <- c(
  "Persian"        = "فارسی",
  "Other language" = "زبان‌های دیگر",
  "English only"   = "فقط انگلیسی"
)

# US-LANGUAGE: top non-Persian, non-English languages (factoid list).
US_LANGOTHER_FA <- c(
  "Armenian" = "ارمنی",
  "Assyrian" = "آشوری",
  "Arabic"   = "عربی",
  "Turkish"  = "ترکی",
  "Kurdish"  = "کُردی"
)

# US-WORK: self-employment by country of birth (label-above bars + hover).
# Country/region names are given in Persian (like the site's country nav);
# "All immigrants" is a benchmark label, not a place.
US_ORIGIN_FA <- c(
  "Israel/Palestine" = "اسرائیل/فلسطین",
  "Armenia"          = "ارمنستان",
  "Iran"             = "ایران",
  "Lebanon/Syria"    = "لبنان/سوریه",
  "Korea"            = "کره",
  "Vietnam"          = "ویتنام",
  "Turkey"           = "ترکیه",
  "Pakistan"         = "پاکستان",
  "Mexico"           = "مکزیک",
  "All immigrants"   = "همه مهاجران",
  "China"            = "چین",
  "India"            = "هند",
  "Philippines"      = "فیلیپین"
)

# US-WORK: occupation group (label-above bars + hover), post-recode names.
US_OCC_FA <- c(
  "Management & finance"   = "مدیریت و مالی",
  "Sales & office"         = "فروش و اداری",
  "Education, law & arts"  = "آموزش، حقوق و هنر",
  "Computer & engineering" = "رایانه و مهندسی",
  "Health care"            = "بهداشت و درمان",
  "Trades & transport"     = "فنی و حمل‌ونقل",
  "Service"                = "خدمات",
  "Science"                = "علوم"
)

# US-WORK: Iranian-owned business sector (label-above bars + hover), post-recode.
US_BIZIND_FA <- c(
  "Arts, food & hospitality" = "هنر، غذا و پذیرایی",
  "Education & health"       = "آموزش و بهداشت",
  "Manufacturing"            = "تولید صنعتی",
  "Finance & real estate"    = "مالی و املاک",
  "Professional & scientific"= "حرفه‌ای و علمی",
  "Other services"           = "سایر خدمات",
  "Transport & utilities"    = "حمل‌ونقل و خدمات عمومی",
  "Wholesale & retail"       = "عمده‌فروشی و خرده‌فروشی"
)

STR <- list(

  # ===========================================================================
  # SHARED SOURCE LINES
  # ===========================================================================
  us_src_pop_1yr = list(
    en = "Source: %s — ACS 2024 1-Year<br>Weighted estimates produced by the Census Bureau from an annual survey of about 3.5 million U.S. households.",
    fa = "منبع: %s — ACS 2024 1-Year<br>برآوردهای وزنی که اداره سرشماری از پیمایشی سالانه در حدود 3.5 میلیون خانوار آمریکا تهیه می‌کند."),
  us_src_pop_5yr = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Pools five annual surveys for more reliable state- and metro-level estimates.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>پنج پیمایش سالانه را برای برآوردهای مطمئن‌تر در سطح ایالت و کلان‌شهر ترکیب می‌کند."),
  us_src_immig = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Iran-born respondents only.<br>Year of immigration is self-reported and may reflect most recent entry.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>تنها پاسخ‌دهندگان متولد ایران.<br>سال مهاجرت خوداظهاری است و ممکن است به آخرین ورود اشاره داشته باشد."),
  us_src_citizen = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Includes birthplace, ancestry, and parental origin.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>شامل محل تولد، اصالت و پیشینه والدین."),
  us_src_marriage = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Currently married or partnered Iranian-Americans.<br>Spouse ethnicity based on the spouse’s own census responses.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>ایرانی‌آمریکایی‌های دارای همسر یا شریک زندگی.<br>قومیت همسر بر پایه پاسخ‌های سرشماری خود همسر است."),
  us_src_educ = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Ages 25+, when most have completed their education.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>25 سال به بالا، سنی که بیشتر افراد تحصیل خود را تمام کرده‌اند."),
  us_src_work = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Employment type reflects primary job held in the past year.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>نوع اشتغال بازتاب شغل اصلی سال گذشته است."),
  us_src_lang = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Language spoken at home, ages 5+.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>زبان رایج در خانه، 5 سال به بالا."),
  us_src_income = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Ages 25–54 (prime working years).<br>Each decile holds 10%% of all U.S. households, ranked by pre-tax household income.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>25 تا 54 سال (سال‌های اصلی کار).<br>هر دهک شامل 10 درصد از همه خانوارهای آمریکاست که بر پایه درآمد خانوار پیش از مالیات رتبه‌بندی شده‌اند."),
  us_src_lpr = list(
    en = "Source: INS Annual Reports (1970–1977); INS Statistical Yearbooks (1978–2004); DHS Yearbook of Immigration Statistics (2005–2024)",
    fa = "منبع: INS Annual Reports (1970–1977)؛ INS Statistical Yearbooks (1978–2004)؛ DHS Yearbook of Immigration Statistics (2005–2024)"),
  us_src_biz = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Self-employed (class of worker), Iran-born residents aged 20+.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>خوداشتغال (طبقه شغلی)، ساکنان متولد ایران 20 سال به بالا."),
  us_src_occ = list(
    en = "Source: %s — ACS 2020–2024 5-Year PUMS<br>Employed Iranian-American residents (first and second generation) aged 16+.",
    fa = "منبع: %s — ACS 2020–2024 5-Year PUMS<br>ساکنان شاغل ایرانی‌آمریکایی (نسل اول و دوم) 16 سال به بالا."),

  # shared "Men" / "Women" panel labels (butterfly annotations)
  us_men = list(en = "Men", fa = "مردان"),
  us_women = list(en = "Women", fa = "زنان"),

  # shared big-number percent template for factoid cards (36px navy number)
  us_bignum_pct = list(en = "%s%%", fa = "%s درصد"),

  # ===========================================================================
  # US-IMMIGRATION & CITIZENSHIP
  # ===========================================================================
  us_immig_title = list(
    en = "Immigration & Citizenship",
    fa = "آمریکا: مهاجرت و تابعیت"),

  # --- charts ----------------------------------------------------------------
  us_immig_chart_title = list(
    en = "<b>Iranian Migration to the US:<br>Annual Arrivals and Cumulative Trends</b>",
    fa = "<b>مهاجرت ایرانیان به آمریکا:<br>ورود سالانه و روند تجمعی</b>"),
  us_immig_hover_bar = list(
    en = "<b>%s</b><br>%s arrivals<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s ورود<br>تجمعی: %s درصد"),
  us_immig_hover_line = list(
    en = "<b>%s</b><br>%s%% of today's Iran-born population had arrived",
    fa = "<b>%s</b><br>%s درصد از جمعیت کنونی متولدان ایران تا این سال وارد شده بودند"),
  us_citizen_title = list(
    en = "<b>Iranian-Americans by<br>Citizenship Status</b>",
    fa = "<b>ایرانی‌آمریکایی‌ها به تفکیک<br>وضعیت تابعیت</b>"),
  us_citizen_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s (%s درصد)"),

  # --- text cards ------------------------------------------------------------
  us_immig_c1_primary = list(
    en = "Half of first-generation Iranian-Americans in the U.S. arrived in %s or later.",
    fa = "نیمی از ایرانی‌آمریکایی‌های نسل اول در آمریکا در سال %s یا پس از آن وارد شده‌اند."),
  us_immig_c2_big = list(
    en = "Nearly 9 in 10",
    fa = "تقریباً 9 از هر 10"),
  us_immig_c2_primary = list(
    en = "Iranian-Americans are U.S. citizens &mdash; %s%% naturalized and %s%% born in the United States.",
    fa = "ایرانی‌آمریکایی‌ها شهروند آمریکا هستند &mdash; %s درصد با تابعیت اکتسابی و %s درصد متولد آمریکا."),

  # ===========================================================================
  # US-ADMISSIONS (INS/DHS history)
  # ===========================================================================
  us_adm_title = list(
    en = "US: Immigration History",
    fa = "آمریکا: تاریخچه مهاجرت"),
  us_lpr_total_title = list(
    en = "<b>Iranians Granted<br>US Permanent Resident Status,<br>1970–2024</b>",
    fa = "<b>ایرانیان دریافت‌کننده<br>اقامت دائم آمریکا،<br>1970–2024</b>"),
  us_lpr_total_hover = list(
    en = "<b>%s</b><br>Granted: %s",
    fa = "<b>%s</b><br>اعطاشده: %s"),
  us_lpr_cat_title = list(
    en = "<b>Iranian US Permanent Residence Grants<br>by Category, 1970–2024</b>",
    fa = "<b>اعطای اقامت دائم آمریکا به ایرانیان<br>به تفکیک نوع، 1970–2024</b>"),
  us_lpr_cat_annot = list(
    en = "1970–1977: total grants only (no category breakdown in source). 1978–1991: family and employment combined in published tabulations.",
    fa = "1970–1977: تنها مجموع اعطاها (بدون تفکیک نوع در منبع). 1978–1991: مهاجرت خانوادگی و کاری در جدول‌های منتشرشده با هم آمده‌اند."),
  us_lpr_hover_total_pre78 = list(
    en = "<b>Total</b> (categories not reported separately): %s",
    fa = "<b>مجموع</b> (انواع جداگانه گزارش نشده): %s"),
  us_lpr_hover_famemp_pre92 = list(
    en = "<b>Family + Employment</b> (combined in source): %s",
    fa = "<b>خانوادگی + کاری</b> (در منبع ترکیب شده): %s"),
  us_lpr_hover_cat = list(
    en = "<b>%s</b>: %s",
    fa = "<b>%s</b>: %s"),

  # --- text cards ------------------------------------------------------------
  us_adm_c1_primary = list(
    en = "Iranians granted U.S. permanent residence between 1970 and 2024.",
    fa = "ایرانیانی که میان سال‌های 1970 و 2024 اقامت دائم آمریکا دریافت کرده‌اند."),
  us_adm_c1_b1 = list(
    en = "Counts green cards granted, not the current Iran-born population",
    fa = "شمار گرین‌کارت‌های اعطاشده را نشان می‌دهد، نه جمعیت کنونی متولدان ایران"),
  us_adm_c1_b2 = list(
    en = "Includes people who later died, returned, or moved elsewhere",
    fa = "شامل کسانی که بعدها درگذشته، بازگشته یا به جای دیگری رفته‌اند"),
  us_adm_c1_b3 = list(
    en = "Many recipients first entered on temporary visas (student, work) before receiving permanent residence in a subsequent year",
    fa = "بسیاری از دریافت‌کنندگان نخست با ویزای موقت (تحصیلی، کاری) وارد شده و در سالی بعد اقامت دائم گرفته‌اند"),
  us_adm_c2_head = list(
    en = "Green card pathways",
    fa = "مسیرهای گرین‌کارت"),
  us_adm_c2_b1 = list(
    en = "<b>Family</b> — immediate relatives or family sponsorship",
    fa = "<b>خانوادگی</b> — بستگان درجه‌یک یا اسپانسر خانوادگی"),
  us_adm_c2_b2 = list(
    en = "<b>Employment</b> — employer or skill-based admission",
    fa = "<b>کاری</b> — پذیرش بر پایه کارفرما یا مهارت"),
  us_adm_c2_b3 = list(
    en = "<b>Refugee/Asylee</b> — prior refugee or asylee status",
    fa = "<b>پناهنده/پناهجو</b> — وضعیت پیشین پناهندگی یا پناهجویی"),
  us_adm_c2_b4 = list(
    en = "<b>Diversity</b> — annual visa lottery for countries with low U.S. immigration",
    fa = "<b>تنوع (لاتاری)</b> — قرعه‌کشی سالانه ویزا برای کشورهایی که مهاجرت کمی به آمریکا دارند"),
  us_adm_c2_b5 = list(
    en = "Family has been the largest category throughout",
    fa = "مهاجرت خانوادگی در سراسر این دوره بزرگ‌ترین نوع بوده است"),
  us_adm_c2_b6 = list(
    en = "The 1989–91 surge includes a one-time program from a 1986 law (IRCA) that granted permanent residence to many previously undocumented immigrants",
    fa = "جهش 1989–91 شامل برنامه‌ای یک‌باره از قانون 1986 (IRCA) است که به بسیاری از مهاجران پیش‌تر بدون‌مدرک اقامت دائم داد"),

  # ===========================================================================
  # US-EDUCATION
  # ===========================================================================
  us_educ_title = list(
    en = "Education",
    fa = "آمریکا: تحصیلات"),
  us_educ_sec1 = list(
    en = "Educational Attainment of Iranian-Americans: First Generation",
    fa = "سطح تحصیلات ایرانی‌آمریکایی‌ها: نسل اول"),
  us_educ_sec2 = list(
    en = "Educational Attainment of Iranian-Americans: Second Generation",
    fa = "سطح تحصیلات ایرانی‌آمریکایی‌ها: نسل دوم"),
  us_educ_hover_m = list(
    en = "<b>%s</b><br>%s, Men<br>%s%%",
    fa = "<b>%s</b><br>%s، مردان<br>%s درصد"),
  us_educ_hover_w = list(
    en = "<b>%s</b><br>%s, Women<br>%s%%",
    fa = "<b>%s</b><br>%s، زنان<br>%s درصد"),
  us_gen1_label = list(en = "1st Generation", fa = "نسل اول"),
  us_gen2_label = list(en = "2nd Generation", fa = "نسل دوم"),

  # --- text cards ------------------------------------------------------------
  us_educ_c1_primary = list(
    en = "of first-generation Iranian-American women ages 25&ndash;34 hold a bachelor&rsquo;s degree or higher &mdash; now ahead of men (%s%%).",
    fa = "از زنان ایرانی‌آمریکایی نسل اول در گروه سنی 25&ndash;34 دارای مدرک لیسانس یا بالاتر هستند &mdash; اکنون از مردان (%s درصد) پیشی گرفته‌اند."),
  us_educ_c1_b1 = list(
    en = "Among older first-generation (ages 75&ndash;84), %s%% of men hold a bachelor&rsquo;s degree or higher, vs only %s%% of women",
    fa = "در میان نسل اول مسن‌تر (گروه سنی 75&ndash;84)، %s درصد مردان مدرک لیسانس یا بالاتر دارند، در برابر تنها %s درصد زنان"),
  us_educ_c2_primary = list(
    en = "of second-generation Iranian-American women ages 35&ndash;44 hold a bachelor&rsquo;s degree or higher &mdash; vs %s%% of men.",
    fa = "از زنان ایرانی‌آمریکایی نسل دوم در گروه سنی 35&ndash;44 دارای مدرک لیسانس یا بالاتر هستند &mdash; در برابر %s درصد مردان."),
  us_educ_c2_b1 = list(
    en = "Both generations now show women outpacing men in higher education",
    fa = "در هر دو نسل اکنون زنان در آموزش عالی از مردان پیشی گرفته‌اند"),

  # ===========================================================================
  # US-WORK
  # ===========================================================================
  us_work_title = list(
    en = "Work",
    fa = "آمریکا: کار"),
  us_work_sec1 = list(
    en = "Employment of Iranian-Americans: First Generation",
    fa = "اشتغال ایرانی‌آمریکایی‌ها: نسل اول"),
  us_work_sec2 = list(
    en = "Employment of Iranian-Americans: Second Generation",
    fa = "اشتغال ایرانی‌آمریکایی‌ها: نسل دوم"),
  us_work_hover_m = list(
    en = "<b>%s</b><br>%s, Men<br>%s%%",
    fa = "<b>%s</b><br>%s، مردان<br>%s درصد"),
  us_work_hover_w = list(
    en = "<b>%s</b><br>%s, Women<br>%s%%",
    fa = "<b>%s</b><br>%s، زنان<br>%s درصد"),

  # --- self-employment / occupation / sector charts --------------------------
  us_bizrate_title = list(
    en = "<b>Self-Employment by<br>Country of Birth</b>",
    fa = "<b>خوداشتغالی به تفکیک<br>کشور محل تولد</b>"),
  us_bizrate_hover = list(
    en = "<b>%s</b><br>%s%% self-employed",
    fa = "<b>%s</b><br>%s درصد خوداشتغال"),
  us_bizind_title = list(
    en = "<b>Industries of<br>Iranian-Owned Businesses</b>",
    fa = "<b>بخش‌های فعالیت<br>کسب‌وکارهای ایرانیان</b>"),
  us_bizind_hover = list(
    en = "<b>%s</b><br>%s%% of Iranian-owned businesses",
    fa = "<b>%s</b><br>%s درصد از کسب‌وکارهای متعلق به ایرانیان"),
  us_occ_title = list(
    en = "<b>Occupations of Employed<br>Iranian-Americans</b>",
    fa = "<b>مشاغل ایرانی‌آمریکایی‌های<br>شاغل</b>"),
  us_occ_hover = list(
    en = "<b>%s</b><br>%s%% of employed Iranian-Americans",
    fa = "<b>%s</b><br>%s درصد از ایرانی‌آمریکایی‌های شاغل"),

  # --- tabs ------------------------------------------------------------------
  us_tab_1gen = list(en = "First Generation", fa = "نسل اول"),
  us_tab_2gen = list(en = "Second Generation", fa = "نسل دوم"),
  us_tab_occ = list(en = "Occupations", fa = "مشاغل"),
  us_tab_selfemp = list(en = "Self-Employment", fa = "خوداشتغالی"),
  us_tab_sectors = list(en = "Sectors", fa = "بخش‌ها"),

  # --- text cards ------------------------------------------------------------
  us_work_c1_primary = list(
    en = "of employed Iranian-Americans work in the private sector &mdash; the most common employment type for both men and women.",
    fa = "از ایرانی‌آمریکایی‌های شاغل در بخش خصوصی کار می‌کنند &mdash; رایج‌ترین نوع اشتغال برای هم مردان و هم زنان."),
  us_work_c1_b1 = list(
    en = "Women are more likely than men to work in the public sector (%s%% vs %s%%) or non-profits (%s%% vs %s%%)",
    fa = "زنان بیش از مردان در بخش دولتی (%s درصد در برابر %s درصد) یا بخش غیرانتفاعی (%s درصد در برابر %s درصد) کار می‌کنند"),
  us_work_c1_b2 = list(
    en = "Self-employment is more common among men (%s%% vs %s%%) and in the first generation than the second (%s%% vs %s%%)",
    fa = "خوداشتغالی در میان مردان (%s درصد در برابر %s درصد) و در نسل اول بیش از نسل دوم (%s درصد در برابر %s درصد) رایج است"),
  us_work_c2_primary = list(
    en = "of employed Iranian-Americans work in professional or managerial occupations.",
    fa = "از ایرانی‌آمریکایی‌های شاغل در مشاغل حرفه‌ای یا مدیریتی کار می‌کنند."),
  us_work_c2_b1 = list(
    en = "Iranian immigrants are self-employed at %s%%, above the %s%% for all U.S. immigrants",
    fa = "مهاجران ایرانی با نرخ %s درصد خوداشتغال هستند، بالاتر از %s درصد برای همه مهاجران آمریکا"),
  us_work_c2_b2 = list(
    en = "Arts, food and hospitality is their most common business sector (%s%%)",
    fa = "هنر، غذا و پذیرایی رایج‌ترین بخش کسب‌وکار آنهاست (%s درصد)"),

  # ===========================================================================
  # US-MARRIAGE
  # ===========================================================================
  us_marriage_title = list(
    en = "Marriage",
    fa = "آمریکا: ازدواج"),
  us_marriage_sec1 = list(
    en = "Spouse/Partner Ethnicity of Iranian-Americans: First Generation",
    fa = "قومیت همسر/شریک ایرانی‌آمریکایی‌ها: نسل اول"),
  us_marriage_sec2 = list(
    en = "Spouse/Partner Ethnicity of Iranian-Americans: Second Generation",
    fa = "قومیت همسر/شریک ایرانی‌آمریکایی‌ها: نسل دوم"),
  us_marriage_hover_m = list(
    en = "<b>%s</b><br>%s, Men, %s<br>Estimated: %s<br>%s%%",
    fa = "<b>%s</b><br>%s، مردان، %s<br>برآورد: %s<br>%s درصد"),
  us_marriage_hover_w = list(
    en = "<b>%s</b><br>%s, Women, %s<br>Estimated: %s<br>%s%%",
    fa = "<b>%s</b><br>%s، زنان، %s<br>برآورد: %s<br>%s درصد"),

  # --- text cards ------------------------------------------------------------
  us_marriage_c1_primary = list(
    en = "of first-generation Iranian-Americans in partnerships have an Iranian partner.",
    fa = "از ایرانی‌آمریکایی‌های نسل اول دارای شریک زندگی، شریکی ایرانی دارند."),
  us_marriage_c2_primary = list(
    en = "of second-generation Iranian-Americans in partnerships have an Iranian partner.",
    fa = "از ایرانی‌آمریکایی‌های نسل دوم دارای شریک زندگی، شریکی ایرانی دارند."),
  us_marriage_c2_b1 = list(
    en = "%s%% partner with White, non-Hispanic Americans",
    fa = "%s درصد با آمریکایی‌های سفیدپوست غیرلاتین‌تبار ازدواج یا زندگی مشترک دارند"),
  us_marriage_c2_b2 = list(
    en = "Similar to <a href=\"https://pmc.ncbi.nlm.nih.gov/articles/PMC8112448/\" target=\"_blank\" style=\"color:#2774AE; text-decoration:none; border-bottom:1px solid rgba(39,116,174,0.4);\">other second-generation Asian Americans</a>",
    fa = "مشابه <a href=\"https://pmc.ncbi.nlm.nih.gov/articles/PMC8112448/\" target=\"_blank\" style=\"color:#2774AE; text-decoration:none; border-bottom:1px solid rgba(39,116,174,0.4);\">دیگر آمریکایی‌های آسیایی‌تبار نسل دوم</a>"),

  # ===========================================================================
  # US-INCOME
  # ===========================================================================
  us_income_title = list(
    en = "Income",
    fa = "آمریکا: درآمد"),
  us_income_chart_title = list(
    en = "<b>Position in US<br>Household Income Distribution:<br>%s (Ages 25–54)</b>",
    fa = "<b>جایگاه در توزیع درآمد<br>خانوار در آمریکا:<br>%s (25 تا 54 سال)</b>"),
  us_income_xaxis = list(
    en = "Income Decile (Lowest to Highest)",
    fa = "دهک درآمدی (کمترین تا بیشترین)"),
  us_income_baseline_annot = list(
    en = "10% =<br>national<br>baseline",
    fa = "10 درصد =<br>سهم پایه<br>هر دهک"),
  us_income_hover = list(
    en = "<b>Decile:</b> %s<br><b>Households in sample:</b> %s<br><b>Share:</b> %s%%",
    fa = "<b>دهک:</b> %s<br><b>خانوارهای نمونه:</b> %s<br><b>سهم:</b> %s درصد"),
  us_income_gen1 = list(en = "First Generation", fa = "نسل اول"),
  us_income_gen2 = list(en = "Second Generation", fa = "نسل دوم"),

  # --- text cards ------------------------------------------------------------
  us_income_c1_primary = list(
    en = "of first-generation Iranian-American households (ages 25&ndash;54) fall in the top U.S. income decile.",
    fa = "از خانوارهای ایرانی‌آمریکایی نسل اول (25 تا 54 سال) در بالاترین دهک درآمدی آمریکا قرار می‌گیرند."),
  us_income_c1_b1 = list(
    en = "More than double the national baseline of 10%",
    fa = "بیش از دو برابر سهم پایه هر دهک (10 درصد)"),
  us_income_c1_b2 = list(
    en = "Only %s%% fall in the lowest decile",
    fa = "تنها %s درصد در پایین‌ترین دهک قرار می‌گیرند"),
  us_income_c2_primary = list(
    en = "of second-generation Iranian-American households (ages 25&ndash;54) fall in the top U.S. income decile.",
    fa = "از خانوارهای ایرانی‌آمریکایی نسل دوم (25 تا 54 سال) در بالاترین دهک درآمدی آمریکا قرار می‌گیرند."),
  us_income_c2_b1 = list(
    en = "Even more concentrated at the top than the first generation",
    fa = "تمرکز آنها در دهک‌های بالایی حتی از نسل اول بیشتر است"),
  us_income_c2_b2 = list(
    en = "Just %s%% fall in the lowest decile",
    fa = "تنها %s درصد در پایین‌ترین دهک قرار می‌گیرند"),

  # ===========================================================================
  # US-LANGUAGE
  # ===========================================================================
  us_lang_title = list(
    en = "US: Language",
    fa = "آمریکا: زبان"),
  us_lang_chart_title = list(
    en = "<b>Language at Home of Iranian-Americans:<br>%s</b>",
    fa = "<b>زبان رایج در خانه ایرانی‌آمریکایی‌ها:<br>%s</b>"),
  us_lang_hover = list(
    en = "<b>%s</b><br>%s, Ages %s<br>%s%%",
    fa = "<b>%s</b><br>%s، سن %s<br>%s درصد"),
  us_lang_gen1 = list(en = "First Generation", fa = "نسل اول"),
  us_lang_gen2 = list(en = "Second Generation", fa = "نسل دوم"),

  # --- text cards ------------------------------------------------------------
  us_lang_c1_primary = list(
    en = "of first-generation Iranian-Americans speak Persian at home.",
    fa = "از ایرانی‌آمریکایی‌های نسل اول در خانه فارسی صحبت می‌کنند."),
  us_lang_c1_b1 = list(
    en = "%s%% speak a different non-English language at home",
    fa = "%s درصد در خانه زبانی دیگر غیر از انگلیسی صحبت می‌کنند"),
  us_lang_c1_b2 = list(
    en = "Largest groups: %s",
    fa = "بزرگ‌ترین گروه‌ها: %s"),
  us_lang_c1_b3 = list(
    en = "%s%% speak only English at home",
    fa = "%s درصد در خانه فقط انگلیسی صحبت می‌کنند"),
  us_lang_c2_primary = list(
    en = "of second-generation Iranian-Americans speak Persian at home.",
    fa = "از ایرانی‌آمریکایی‌های نسل دوم در خانه فارسی صحبت می‌کنند."),
  us_lang_c2_b1 = list(
    en = "%s%% speak only English at home",
    fa = "%s درصد در خانه فقط انگلیسی صحبت می‌کنند"),
  us_lang_c2_b2 = list(
    en = "%s%% speak another non-English language at home",
    fa = "%s درصد در خانه زبانی دیگر غیر از انگلیسی صحبت می‌کنند"),
  # top-other language list fragments (built in the builder)
  us_lang_other_lt1 = list(
    en = "%s &lt;1%%",
    fa = "%s کمتر از 1 درصد"),
  us_lang_other_pct = list(
    en = "%s %s%%",
    fa = "%s %s درصد"),

  # ===========================================================================
  # US-POPULATION
  # ===========================================================================
  us_pop_title = list(
    en = "Population",
    fa = "آمریکا: جمعیت"),
  us_pop_headline_label = list(
    en = "Estimated Iranian-American Population",
    fa = "برآورد جمعیت ایرانی‌آمریکایی"),
  us_pop_caption = list(
    en = "Based on the <a href=\"https://www2.census.gov/programs-surveys/acs/methodology/questionnaires/2024/quest24.pdf\" style=\"color:#2774AE;\" target=\"_blank\">2024 American Community Survey</a>, a nationwide survey by the U.S. Census Bureau",
    fa = "بر پایه <a href=\"https://www2.census.gov/programs-surveys/acs/methodology/questionnaires/2024/quest24.pdf\" style=\"color:#2774AE;\" target=\"_blank\">پیمایش جامعه آمریکا 2024</a>، پیمایشی سراسری از سوی اداره سرشماری آمریکا"),
  us_pop_idbox_intro = list(
    en = "A person is counted if they meet <em>at least one</em> of four survey questions:",
    fa = "هر فرد در صورتی شمرده می‌شود که <em>دست‌کم یکی</em> از این چهار پرسش سرشماری در موردش صدق کند:"),
  us_pop_idbox_b1 = list(
    en = "<strong>Place of birth</strong> <span style=\"color:#6b6b6b;\">&mdash; &ldquo;Where was this person born?&rdquo;</span>",
    fa = "<strong>محل تولد</strong> <span style=\"color:#6b6b6b;\">&mdash; «این فرد کجا متولد شده است؟»</span>"),
  us_pop_idbox_b2 = list(
    en = "<strong>Ancestry</strong> <span style=\"color:#6b6b6b;\">&mdash; &ldquo;What is this person&rsquo;s ancestry or ethnic origin?&rdquo;</span>",
    fa = "<strong>اصالت</strong> <span style=\"color:#6b6b6b;\">&mdash; «اصالت یا خاستگاه قومی این فرد چیست؟»</span>"),
  us_pop_idbox_b3 = list(
    en = "<strong>Race</strong> <span style=\"color:#6b6b6b;\">&mdash; &ldquo;Iranian&rdquo; written in under &ldquo;White&rdquo;</span>",
    fa = "<strong>نژاد</strong> <span style=\"color:#6b6b6b;\">&mdash; «ایرانی» که ذیل گزینه «سفید» نوشته شده است</span>"),
  us_pop_idbox_b4 = list(
    en = "<strong>Parental origin</strong> <span style=\"color:#6b6b6b;\">&mdash; lives with a parent who meets any of the above</span>",
    fa = "<strong>پیشینه والدین</strong> <span style=\"color:#6b6b6b;\">&mdash; با والدی زندگی می‌کند که هر یک از موارد بالا را دارد</span>"),

  us_waterfall_title = list(
    en = "<b>Iranian-Americans: How We Count</b>",
    fa = "<b>ایرانی‌آمریکایی‌ها: روش شمارش</b>"),
  us_waterfall_hover = list(
    en = "<b>%s</b><br>%s Born in Iran<br>%s Iranian ancestry<br>%s Iranian race write-in<br>%s Iranian parent<br><br>%s people · %s%% of total<br>Running total: %s",
    fa = "<b>%s</b><br>%s متولد ایران<br>%s اصالت ایرانی<br>%s خوداظهاری نژادی ایرانی<br>%s والد ایرانی<br><br>%s نفر · %s درصد از کل<br>مجموع تجمعی: %s"),
  us_crit_birth = list(en = "Born in<br>Iran", fa = "متولد<br>ایران"),
  us_crit_ancestry = list(en = "Iranian<br>ancestry", fa = "اصالت<br>ایرانی"),
  us_crit_race = list(en = "Iranian race<br>write-in", fa = "خوداظهاری<br>نژادی ایرانی"),
  us_crit_parent = list(en = "Iranian<br>parent", fa = "والد<br>ایرانی"),

  us_region_title = list(
    en = "<b>Iranian-Americans by US Region</b>",
    fa = "<b>ایرانی‌آمریکایی‌ها به تفکیک منطقه آمریکا</b>"),
  us_region_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s (%s درصد)"),

  us_geo_section = list(
    en = "Geographic Distribution of Iranian-Americans",
    fa = "توزیع جغرافیایی ایرانی‌آمریکایی‌ها"),
  us_geo_tab_state = list(en = "By State", fa = "به تفکیک ایالت"),
  us_geo_tab_county = list(en = "By California County", fa = "به تفکیک کانتی کالیفرنیا"),
  us_geo_tab_la = list(en = "Greater Los Angeles", fa = "کلان‌شهر لس‌آنجلس"),

  us_geo_src_state = list(
    en = "Source: <a href=\"https://www.census.gov/programs-surveys/acs/microdata.html\" target=\"_blank\" style=\"color:#2774AE;\">U.S. Census Bureau</a> &mdash; ACS 2020&ndash;2024 5-Year PUMS<br>Weighted population estimates by state. Includes birthplace, ancestry, and parental origin.",
    fa = "منبع: <a href=\"https://www.census.gov/programs-surveys/acs/microdata.html\" target=\"_blank\" style=\"color:#2774AE;\">اداره سرشماری آمریکا</a> &mdash; ACS 2020&ndash;2024 5-Year PUMS<br>برآوردهای وزنی جمعیت به تفکیک ایالت. شامل محل تولد، اصالت و پیشینه والدین."),
  us_geo_src_county = list(
    en = "Source: ACS 2020&ndash;2024 5-Year<br>California counties only. Includes birthplace, ancestry, and parental origin, allocated from sub-county survey areas.",
    fa = "منبع: ACS 2020&ndash;2024 5-Year<br>تنها کانتی‌های کالیفرنیا. شامل محل تولد، اصالت و پیشینه والدین، توزیع‌شده از حوزه‌های پیمایشی زیرکانتی."),
  us_geo_src_la = list(
    en = "Source: ACS 2020&ndash;2024 5-Year PUMS (PUMA-level)<br>Greater Los Angeles CSA. Includes birthplace, ancestry, and parental origin.<br>Census survey areas, not exact neighborhoods.",
    fa = "منبع: ACS 2020&ndash;2024 5-Year PUMS (سطح PUMA)<br>کلان‌شهر لس‌آنجلس (CSA). شامل محل تولد، اصالت و پیشینه والدین.<br>حوزه‌های پیمایشی سرشماری، نه محله‌های دقیق."),

  # MapLibre popup fragments (spliced into the map <script>; the leading/
  # trailing markup is part of the byte-identical JS literal — do not trim).
  us_map_label_ia = list(
    en = " Iranian-Americans<br>",
    fa = " ایرانی‌آمریکایی<br>"),
  us_map_pct_us = list(
    en = "% of U.S. total",
    fa = " درصد از کل آمریکا"),
  us_map_pct_ca = list(
    en = "% of CA total",
    fa = " درصد از کل کالیفرنیا")
)

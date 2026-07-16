# ============================================================================
# R/i18n/strings_canada.R  — Canada page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_canada.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(), plus fa-only category display lookup vectors (keyed on
# the English category label the builder produces) for the repeated category
# sets — region, language, religion, immigration type, citizenship, education
# level, field of study, employment category, industry sector, income band,
# generation.
#
# RULES honored here (same as strings_germany.R / strings_nl.R):
#   * en values are the EXACT English strings from build_canada.R (source of
#     truth) so the six English editions stay BYTE-IDENTICAL.
#   * Every %d / %.1f / format() slot from the original is rewritten as %s; the
#     builder passes fa_num()/fmtv() so the number is language-formatted, and
#     the percent SIGN lives in the template (%% in en,  درصد in fa) for strings
#     that pass through sprintf(). A literal "%" in prose that is NOT re-run
#     through sprintf() stays a single "%".
#   * HTML tags / entities (<strong>, <em>, <br>, &mdash;, &ndash;, &amp;,
#     &ldquo;, &rdquo;, &rsquo;) mirror the en structure; only the human text is
#     swapped in fa. ASCII digits in fa literals are Persian-ized at runtime by
#     FA_NUM_SCRIPT (and by htxt() where the builder pre-digits hover/prose).
#   * Prose / cards / hover use the WORD «درصد»; axis + chart-furniture tick
#     suffixes use ٪ (handled in the builder, not here).
#   * Official agency / dataset names stay Latin (Statistics Canada, StatCan,
#     Census PUMF, cancensus). Canadian place names (Ontario, Toronto, province
#     names) stay Latin.
#
# TERMS: Iranian-Canadian = ایرانی‌کانادایی; Iranian-origin = ایرانی‌تبار;
# Iran-born = متولد ایران; first / second / third generation = نسل اول / دوم /
# سوم; Canada = کانادا.
# ============================================================================

# --- fa-only category display vectors (keyed on the English label) -----------

# Region of residence (ca-population region bar).
CA_REGION_FA <- c(
  "Central"  = "مرکزی",
  "Western"  = "غربی",
  "Atlantic" = "آتلانتیک",
  "Northern" = "شمالی"
)

# Language pattern by generation (ca-langrelig language chart).
CA_LANGCAT_FA <- c(
  "Persian (mother tongue), Persian (at home)"  = "فارسی (زبان مادری)، فارسی (در خانه)",
  "Persian (mother tongue), Eng/Fr (at home)"   = "فارسی (زبان مادری)، انگلیسی/فرانسه (در خانه)",
  "Minority (mother tongue), same (at home)"    = "زبان اقلیت (زبان مادری)، همان (در خانه)",
  "Minority (mother tongue), Eng/Fr (at home)"  = "زبان اقلیت (زبان مادری)، انگلیسی/فرانسه (در خانه)",
  "English (mother tongue), English (at home)"  = "انگلیسی (زبان مادری)، انگلیسی (در خانه)",
  "Other languages"                             = "زبان‌های دیگر"
)

# Religious identification by generation (ca-langrelig religion chart).
CA_RELIGCAT_FA <- c(
  "Muslim"              = "مسلمان",
  "No religion/Secular" = "بی‌دین/سکولار",
  "Christian"           = "مسیحی",
  "Other religions"     = "سایر ادیان",
  "Not stated"          = "اظهارنشده"
)

# Generation split used on the language / religion chart y-axis + hover.
CA_GENLR_FA <- c(
  "1st Generation"  = "نسل اول",
  "2nd+ Generation" = "نسل دوم و بالاتر"
)

# Immigration type (ca-immigration immigration-type chart).
CA_IMMTYPE_FA <- c(
  "Economic immigrants"         = "مهاجران اقتصادی",
  "Family-sponsored immigrants" = "مهاجران خانوادگی",
  "Refugees"                    = "پناهندگان"
)

# Citizenship status — full label (ca-immigration citizenship hover).
CA_CITSTATUS_FA <- c(
  "Canadian citizen by birth"          = "شهروند کانادا از بدو تولد",
  "Canadian citizen by naturalization" = "شهروند کانادا با تابعیت اکتسابی",
  "Not a Canadian citizen"             = "بدون تابعیت کانادا"
)

# Citizenship status — short two-line axis label (\n line break kept).
CA_CITSHORT_FA <- c(
  "Naturalized\ncitizen" = "تابعیت\nاکتسابی",
  "Born in\nCanada"      = "متولد\nکانادا",
  "Not a\ncitizen"       = "بدون\nتابعیت"
)

# Highest education level (ca-education attainment butterfly).
CA_EDUC_FA <- c(
  "Less than BA degree" = "کمتر از لیسانس",
  "BA degree"           = "لیسانس",
  "Graduate degree"     = "تحصیلات تکمیلی"
)

# Field of study (ca-education field-of-study butterfly).
CA_FOS_FA <- c(
  "Science, Technology, Engineering & Math" = "علوم، فناوری، مهندسی و ریاضی",
  "Business & Management"                   = "بازرگانی و مدیریت",
  "Health"                                  = "بهداشت و درمان",
  "Social Sciences & Humanities"            = "علوم اجتماعی و انسانی",
  "Other"                                   = "سایر"
)

# Employment category (ca-work employment butterfly).
CA_EMPCAT_FA <- c(
  "Private sector"          = "بخش خصوصی",
  "Public sector"           = "بخش دولتی",
  "Self-employed"           = "خوداشتغال",
  "No work in last 5 years" = "بدون کار در 5 سال گذشته"
)

# Industry sector (ca-work industry butterfly).
CA_INDCAT_FA <- c(
  "Professional & Technical"     = "تخصصی و فنی",
  "Health & Education"           = "بهداشت و آموزش",
  "Trade & Services"             = "بازرگانی و خدمات",
  "Manufacturing & Construction" = "تولید و ساخت‌وساز",
  "Public & Other"               = "دولتی و سایر"
)

# Personal-income band (ca-income by-age chart). CA$ + k kept Latin.
CA_INCBAND_FA <- c(
  "Under CA$20k"     = "زیر CA$20k",
  "CA$20k-CA$40k"    = "CA$20k تا CA$40k",
  "CA$40k-CA$60k"    = "CA$40k تا CA$60k",
  "CA$60k-CA$100k"   = "CA$60k تا CA$100k",
  "CA$100k+"         = "CA$100k به بالا"
)

STR <- list(

  # ===========================================================================
  # SHARED
  # ===========================================================================
  ca_src_prefix = list(
    en = "Source: ",
    fa = "منبع: "),

  # generation / gender words reused across pages
  ca_gen1_short = list(
    en = "1st Gen",
    fa = "نسل اول"),
  ca_gen2_short = list(
    en = "2nd Gen",
    fa = "نسل دوم"),
  ca_men = list(
    en = "Men",
    fa = "مرد"),
  ca_women = list(
    en = "Women",
    fa = "زن"),
  ca_ages_25plus = list(
    en = "Ages 25+",
    fa = "۲۵ سال به بالا"),

  # ===========================================================================
  # CA-POPULATION
  # ===========================================================================
  ca_pop_title = list(
    en = "Canada: Population",
    fa = "کانادا: جمعیت"),
  ca_pop_headline_label = list(
    en = "Estimated Iranian-Canadian Population",
    fa = "برآورد جمعیت ایرانی‌کانادایی"),
  ca_pop_headline_caption = list(
    en = "Based on the %s Public Use Microdata File",
    fa = "بر پایه فایل ریزداده عمومی %s"),
  ca_pop_idbox_intro = list(
    en = "A person is counted if they meet <em>at least one</em> of three census questions:",
    fa = "هر فرد در صورتی شمرده می‌شود که <em>دست‌کم یکی</em> از این سه پرسش سرشماری در موردش صدق کند:"),
  ca_pop_idbox_bullet1 = list(
    en = '<strong>Place of birth</strong> <span style="color:#6b6b6b;">&mdash; &ldquo;Where was this person born?&rdquo;</span>',
    fa = '<strong>محل تولد</strong> <span style="color:#6b6b6b;">&mdash; «این فرد در کجا متولد شده است؟»</span>'),
  ca_pop_idbox_bullet2 = list(
    en = '<strong>Ethnic origin</strong> <span style="color:#6b6b6b;">&mdash; &ldquo;What are the ethnic or cultural origins of this person&rsquo;s ancestors?&rdquo;</span>',
    fa = '<strong>خاستگاه قومی</strong> <span style="color:#6b6b6b;">&mdash; «خاستگاه قومی یا فرهنگی نیاکان این فرد چیست؟»</span>'),
  ca_pop_idbox_bullet3 = list(
    en = '<strong>Mother tongue or home language</strong> <span style="color:#6b6b6b;">&mdash; reports Persian as the language first learned in childhood, or as the language spoken most often at home today</span>',
    fa = '<strong>زبان مادری یا زبان خانه</strong> <span style="color:#6b6b6b;">&mdash; فارسی را نخستین زبان آموخته‌شده در کودکی، یا زبانی که امروز بیشتر در خانه صحبت می‌شود، اعلام کند</span>'),

  # --- waterfall + criteria grid ---------------------------------------------
  ca_wf_title = list(
    en = "<b>Iranian-Canadians: How We Count</b>",
    fa = "<b>ایرانی‌کانادایی‌ها: روش شمارش</b>"),
  ca_wf_gen1 = list(
    en = "First generation",
    fa = "نسل اول"),
  ca_wf_gen2 = list(
    en = "Second generation",
    fa = "نسل دوم"),
  ca_wf_hover = list(
    en = "<b>%s</b><br>%s Born in Iran<br>%s Iranian ethnic origin<br>%s Persian language<br><br>%s people · %s%% of total<br>Running total: %s",
    fa = "<b>%s</b><br>%s متولد ایران<br>%s خاستگاه قومی ایرانی<br>%s زبان فارسی<br><br>%s نفر · %s درصد از کل<br>مجموع تجمعی: %s"),
  ca_crit_born = list(
    en = "Born in<br>Iran",
    fa = "متولد<br>ایران"),
  ca_crit_ethnic = list(
    en = "Iranian<br>ethnicity",
    fa = "قومیت<br>ایرانی"),
  ca_crit_persian = list(
    en = "Persian<br>language",
    fa = "زبان<br>فارسی"),

  # --- region bar ------------------------------------------------------------
  ca_region_title = list(
    en = "<b>Iranian-Canadians by<br>Region of Residence</b>",
    fa = "<b>ایرانی‌کانادایی‌ها به تفکیک<br>منطقه محل سکونت</b>"),
  ca_region_hover = list(
    en = "<b>%s Canada</b><br>%s (%s%%)",
    fa = "<b>کانادای %s</b><br>%s (%s درصد)"),

  # --- maps ------------------------------------------------------------------
  ca_geo_section = list(
    en = "Geographic Distribution of Iranian-Canadians",
    fa = "توزیع جغرافیایی ایرانی‌کانادایی‌ها"),
  ca_tab_by_province = list(
    en = "By Province",
    fa = "به تفکیک استان"),
  ca_tab_by_ont = list(
    en = "By Ontario Municipality",
    fa = "به تفکیک شهرداری انتاریو"),
  ca_prov_hover = list(
    en = "<b>%s</b><br>%s Iranian-Canadians<br>%s%% of total",
    fa = "<b>%s</b><br>%s ایرانی‌کانادایی<br>%s درصد از کل"),
  ca_ont_hover = list(
    en = "<b>%s</b><br>%s residents with Iranian ethnic origin or born in Iran",
    fa = "<b>%s</b><br>%s ساکن با خاستگاه قومی ایرانی یا متولد ایران"),

  # --- source lines ----------------------------------------------------------
  ca_src_pumf = list(
    en = " — 2021 Canadian Census<br>Based on a 2.7% public-use sample of census records.<br>Estimates for small sub-groups may have limited reliability.",
    fa = " — سرشماری 2021 کانادا<br>بر پایه نمونه عمومی 2.7 درصدی از رکوردهای سرشماری.<br>برآورد زیرگروه‌های کوچک ممکن است اعتبار محدودی داشته باشد."),
  ca_src_ont = list(
    en = " — 2021 Census (cancensus aggregate tables, Iranian ethnic origin OR born in Iran, maximum per municipality). Municipal totals differ from the province-level PUMF count because the published aggregate tables are individually suppressed for small census subdivisions and use a different counting rule than the microdata.",
    fa = " — سرشماری 2021 (جداول تجمیعی cancensus، خاستگاه قومی ایرانی یا متولد ایران، بیشینه در هر شهرداری). مجموع‌های شهرداری با شمار PUMF در سطح استان تفاوت دارد، زیرا جداول تجمیعی منتشرشده برای بخش‌های سرشماری کوچک به‌صورت جداگانه پوشانده شده‌اند و از قاعده شمارشی متفاوت با ریزداده استفاده می‌کنند."),

  # ===========================================================================
  # CA-LANGUAGE & RELIGION
  # ===========================================================================
  ca_langrelig_title = list(
    en = "Canada: Language & Religion",
    fa = "کانادا: زبان و دین"),
  ca_lang_title = list(
    en = "<b>Language of Iranian-Canadians<br>by Generation</b>",
    fa = "<b>زبان ایرانی‌کانادایی‌ها<br>به تفکیک نسل</b>"),
  ca_relig_title = list(
    en = "<b>Religious Identification<br>of Iranian-Canadians by Generation</b>",
    fa = "<b>هویت دینی ایرانی‌کانادایی‌ها<br>به تفکیک نسل</b>"),
  # shared hover: category / generation-or-period / percent
  ca_hover_cat_gen_pct = list(
    en = "<b>%s</b><br>%s<br>%s%%",
    fa = "<b>%s</b><br>%s<br>%s درصد"),

  # --- language / religion cards ---------------------------------------------
  ca_lr_c1_sentence = list(
    en = "of first-generation Iranian-Canadians still speak Persian at home.",
    fa = "از ایرانی‌کانادایی‌های نسل اول همچنان در خانه فارسی صحبت می‌کنند."),
  ca_lr_c1_note = list(
    en = "Another %s%% have shifted to English or French at home.",
    fa = "%s درصد دیگر در خانه به انگلیسی یا فرانسه روی آورده‌اند."),
  ca_lr_c2_sentence = list(
    en = "of second-generation Iranian-Canadians still speak Persian at home.",
    fa = "از ایرانی‌کانادایی‌های نسل دوم همچنان در خانه فارسی صحبت می‌کنند."),
  ca_lr_c2_note = list(
    en = "English or French at home rises to %s%%.",
    fa = "سهم انگلیسی یا فرانسه در خانه به %s درصد می‌رسد."),
  ca_lr_c3_sentence = list(
    en = "of first-generation Iranian-Canadians identify as Muslim.",
    fa = "از ایرانی‌کانادایی‌های نسل اول خود را مسلمان می‌دانند."),
  ca_lr_c3_note = list(
    en = "Another %s%% report no religion or a secular identity.",
    fa = "%s درصد دیگر خود را بی‌دین یا سکولار معرفی می‌کنند."),
  ca_lr_c4_sentence = list(
    en = "of second-generation Iranian-Canadians report no religion or a secular identity.",
    fa = "از ایرانی‌کانادایی‌های نسل دوم خود را بی‌دین یا سکولار معرفی می‌کنند."),
  ca_lr_c4_note = list(
    en = "Muslim %s%%, Christian %s%%.",
    fa = "مسلمان %s درصد، مسیحی %s درصد."),

  # ===========================================================================
  # CA-IMMIGRATION & CITIZENSHIP
  # ===========================================================================
  ca_immig_title = list(
    en = "Canada: Immigration & Citizenship",
    fa = "کانادا: مهاجرت و تابعیت"),
  ca_immig_chart_title = list(
    en = "<b>Iranian Migration to Canada:<br>Arrivals and Cumulative Trends</b>",
    fa = "<b>مهاجرت ایرانیان به کانادا:<br>ورودها و روند تجمعی</b>"),
  ca_immig_hover_period = list(
    en = "<b>%s</b><br>%s arrivals (%s/year avg)<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s ورود (میانگین %s در سال)<br>تجمعی: %s درصد"),
  ca_immig_hover_annual = list(
    en = "<b>%s</b><br>%s arrivals<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s ورود<br>تجمعی: %s درصد"),
  ca_immig_hover_cum = list(
    en = "<b>%s</b><br>%s%% of today's Iran-born population had arrived",
    fa = "<b>%s</b><br>تا این زمان %s درصد از جمعیت کنونی متولدان ایران وارد شده بودند"),
  ca_immig_period_before = list(
    en = "Before ",
    fa = "پیش از "),
  ca_immig_annot = list(
    en = "Pre-1995 values show 5-year period arrivals, averaged per year.",
    fa = "مقادیر پیش از 1995 ورودهای دوره‌های پنج‌ساله را نشان می‌دهند که میانگین سالانه گرفته شده است."),
  ca_cit_title = list(
    en = "<b>Citizenship Status of<br>Iranian-Canadians</b>",
    fa = "<b>وضعیت تابعیت<br>ایرانی‌کانادایی‌ها</b>"),
  ca_cit_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s (%s درصد)"),
  ca_immtype_title = list(
    en = "<b>Iranian Immigration to Canada<br>by Type: Post-1990 Arrivals</b>",
    fa = "<b>مهاجرت ایرانیان به کانادا<br>به تفکیک نوع: ورودهای پس از 1990</b>"),
  ca_tab_cit_status = list(
    en = "Citizenship Status",
    fa = "وضعیت تابعیت"),
  ca_tab_immtype = list(
    en = "Immigration Type",
    fa = "نوع مهاجرت"),

  # --- immigration cards -----------------------------------------------------
  ca_immig_c1_sentence = list(
    en = "Half of first-generation Iranian-Canadians arrived in %s or later.",
    fa = "نیمی از ایرانی‌کانادایی‌های نسل اول در سال %s یا پس از آن وارد شده‌اند."),
  ca_immig_c1_note = list(
    en = "Only %s%% arrived during the 1980s.",
    fa = "تنها %s درصد در دهه 1980 وارد شده‌اند."),
  ca_immig_c2_sentence = list(
    en = "of Iranian-Canadians are naturalized Canadian citizens.",
    fa = "از ایرانی‌کانادایی‌ها تابعیت کانادا را کسب کرده‌اند."),
  ca_immig_c2_b1 = list(
    en = "%s%% have not yet obtained citizenship",
    fa = "%s درصد هنوز تابعیت نگرفته‌اند"),
  ca_immig_c2_b2 = list(
    en = "Economic immigration has been the most common pathway since the 1990s",
    fa = "مهاجرت اقتصادی از دهه 1990 رایج‌ترین مسیر بوده است"),

  # --- source line -----------------------------------------------------------
  ca_src_immig = list(
    en = " — 2021 Canadian Census<br>Iran-born respondents only. Pre-1995 immigration periods are grouped into multi-year bands.<br>Smaller estimates should be interpreted with caution.",
    fa = " — سرشماری 2021 کانادا<br>تنها پاسخ‌دهندگان متولد ایران. دوره‌های مهاجرت پیش از 1995 در بازه‌های چندساله گروه‌بندی شده‌اند.<br>برآوردهای کوچک‌تر را باید با احتیاط تفسیر کرد."),

  # ===========================================================================
  # CA-EDUCATION
  # ===========================================================================
  ca_educ_title = list(
    en = "Canada: Education",
    fa = "کانادا: تحصیلات"),
  # butterfly hover with age: category / gen / gender / age / percent
  ca_hover_bfly_age = list(
    en = "<b>%s</b><br>%s, %s, %s<br>%s%%",
    fa = "<b>%s</b><br>%s، %s، %s<br>%s درصد"),
  # butterfly hover no age: category / gen / gender / percent
  ca_hover_bfly_noage = list(
    en = "<b>%s</b><br>%s, %s<br>%s%%",
    fa = "<b>%s</b><br>%s، %s<br>%s درصد"),
  ca_educ_section1 = list(
    en = "Educational Attainment of Iranian-Canadians: First Generation",
    fa = "سطح تحصیلات ایرانی‌کانادایی‌ها: نسل اول"),
  ca_educ_section2 = list(
    en = "Fields of Study of Iranian-Canadians: First Generation",
    fa = "رشته‌های تحصیلی ایرانی‌کانادایی‌ها: نسل اول"),
  ca_educ_c1_sentence = list(
    en = "of first-generation Iranian-Canadian women aged 30&ndash;34 hold a graduate degree &mdash; slightly ahead of men (%s%%).",
    fa = "از زنان ایرانی‌کانادایی نسل اول در گروه سنی 30&ndash;34 دارای تحصیلات تکمیلی هستند &mdash; اندکی از مردان (%s درصد) پیشی گرفته‌اند."),
  ca_educ_c1_note = list(
    en = "Among those 55&ndash;64, men are more likely to hold graduate degrees (%s%% vs %s%%).",
    fa = "در گروه 55&ndash;64، احتمال داشتن تحصیلات تکمیلی در مردان بیشتر است (%s درصد در برابر %s درصد)."),
  ca_educ_c2_sentence = list(
    en = "of first-generation Iranian-Canadian men studied STEM fields &mdash; vs %s%% of women.",
    fa = "از مردان ایرانی‌کانادایی نسل اول در رشته‌های علوم، فناوری، مهندسی و ریاضی تحصیل کرده‌اند &mdash; در برابر %s درصد زنان."),
  ca_educ_c2_note = list(
    en = "Women are more concentrated in social sciences &amp; humanities (%s%%) and health (%s%%).",
    fa = "زنان بیشتر در علوم اجتماعی و انسانی (%s درصد) و بهداشت و درمان (%s درصد) متمرکز هستند."),
  ca_src_educ_1g = list(
    en = " — 2021 Canadian Census<br>First generation ages 30+. Smaller estimates should be interpreted with caution.",
    fa = " — سرشماری 2021 کانادا<br>نسل اول 30 سال به بالا. برآوردهای کوچک‌تر را باید با احتیاط تفسیر کرد."),
  ca_src_fos_1g = list(
    en = " — 2021 Canadian Census<br>First generation ages 25+. Smaller estimates should be interpreted with caution.",
    fa = " — سرشماری 2021 کانادا<br>نسل اول 25 سال به بالا. برآوردهای کوچک‌تر را باید با احتیاط تفسیر کرد."),

  # ===========================================================================
  # CA-WORK
  # ===========================================================================
  ca_work_title = list(
    en = "Canada: Work",
    fa = "کانادا: کار"),
  ca_work_section1 = list(
    en = "Work of Iranian-Canadians: First Generation",
    fa = "کار ایرانی‌کانادایی‌ها: نسل اول"),
  ca_work_section2 = list(
    en = "Work of Iranian-Canadians: Second Generation",
    fa = "کار ایرانی‌کانادایی‌ها: نسل دوم"),
  ca_tab_emp_cat = list(
    en = "Employment Categories",
    fa = "دسته‌های اشتغال"),
  ca_tab_ind_sect = list(
    en = "Industry Sectors",
    fa = "بخش‌های اقتصادی"),
  ca_work_c1_sentence = list(
    en = "of first-generation men aged 35&ndash;44 work in Professional &amp; Technical fields.",
    fa = "از مردان نسل اول در گروه سنی 35&ndash;44 در حوزه‌های تخصصی و فنی کار می‌کنند."),
  ca_work_c1_note = list(
    en = "Manufacturing &amp; Construction ties at %s%%.",
    fa = "سهم تولید و ساخت‌وساز نیز %s درصد است."),
  ca_work_c2_sentence = list(
    en = "of first-generation women aged 35&ndash;44 work in Trade &amp; Services &mdash; the largest sector.",
    fa = "از زنان نسل اول در گروه سنی 35&ndash;44 در بازرگانی و خدمات کار می‌کنند &mdash; بزرگ‌ترین بخش."),
  ca_work_c2_note = list(
    en = "Professional &amp; Technical (%s%%) and Health &amp; Education (%s%%) are close behind.",
    fa = "تخصصی و فنی (%s درصد) و بهداشت و آموزش (%s درصد) با فاصله‌ای اندک در رتبه‌های بعدی قرار دارند."),
  ca_work_c3_sentence = list(
    en = "of second-generation men work in Professional &amp; Technical fields.",
    fa = "از مردان نسل دوم در حوزه‌های تخصصی و فنی کار می‌کنند."),
  ca_work_c3_note = list(
    en = "Trade &amp; Services follows at %s%%.",
    fa = "بازرگانی و خدمات با %s درصد در رتبه بعدی است."),
  ca_work_c4_sentence = list(
    en = "of second-generation women work in Professional &amp; Technical fields.",
    fa = "از زنان نسل دوم در حوزه‌های تخصصی و فنی کار می‌کنند."),
  ca_work_c4_note = list(
    en = "Health &amp; Education (%s%%) is the second-largest sector.",
    fa = "بهداشت و آموزش (%s درصد) دومین بخش بزرگ است."),
  ca_src_work_1g = list(
    en = " — 2021 Canadian Census<br>First generation ages 25+, by age group and gender. Smaller estimates should be interpreted with caution.",
    fa = " — سرشماری 2021 کانادا<br>نسل اول 25 سال به بالا، به تفکیک گروه سنی و جنسیت. برآوردهای کوچک‌تر را باید با احتیاط تفسیر کرد."),
  ca_src_work_2g = list(
    en = " — 2021 Canadian Census<br>Second generation ages 25+, all ages combined due to small sample size. Smaller estimates should be interpreted with caution.",
    fa = " — سرشماری 2021 کانادا<br>نسل دوم 25 سال به بالا، همه گروه‌های سنی به‌دلیل حجم نمونه کوچک با هم آمده‌اند. برآوردهای کوچک‌تر را باید با احتیاط تفسیر کرد."),

  # ===========================================================================
  # CA-INCOME
  # ===========================================================================
  ca_income_title = list(
    en = "Canada: Income",
    fa = "کانادا: درآمد"),
  ca_inc_decile_title = list(
    en = "<b>Position in Canadian<br>Household Income Distribution:<br>First Generation (Ages 25–54)</b>",
    fa = "<b>جایگاه در توزیع درآمد<br>خانوار کانادا:<br>نسل اول (25–54 سال)</b>"),
  ca_inc_decile_xaxis = list(
    en = "Income Decile (Lowest to Highest)",
    fa = "دهک درآمدی (پایین‌ترین تا بالاترین)"),
  ca_inc_decile_hover = list(
    en = "<b>%s</b><br>Households: %s<br>%s%%",
    fa = "<b>%s</b><br>خانوار: %s<br>%s درصد"),
  ca_inc_decile_word = list(
    en = "Decile ",
    fa = "دهک "),
  ca_inc_baseline_annot = list(
    en = "10% =<br>national<br>baseline",
    fa = "10٪ =<br>سهم پایه<br>هر دهک"),
  ca_inc_age_title = list(
    en = "<b>Personal Income by Age:<br>First-Generation Iranian-Canadians</b>",
    fa = "<b>درآمد شخصی به تفکیک سن:<br>ایرانی‌کانادایی‌های نسل اول</b>"),
  ca_inc_age_hover = list(
    en = "<b>%s</b><br>Age %s<br>%s%%",
    fa = "<b>%s</b><br>سن %s<br>%s درصد"),
  ca_inc_c1_sentence = list(
    en = "of first-generation Iranian-Canadian households fall in the lowest Canadian income decile &mdash; nearly double the 10% national baseline.",
    fa = "از خانوارهای ایرانی‌کانادایی نسل اول در پایین‌ترین دهک درآمدی کانادا قرار دارند &mdash; نزدیک به دو برابر سهم پایه هر دهک (10 درصد)."),
  ca_inc_c1_note = list(
    en = "Only %s%% reach the top decile.",
    fa = "تنها %s درصد به بالاترین دهک می‌رسند."),
  ca_inc_c2_sentence = list(
    en = "of first-generation Iranian-Canadian earners aged %s make CA$100,000 or more &mdash; the highest share of any age group.",
    fa = "از درآمدبگیران ایرانی‌کانادایی نسل اول در گروه سنی %s سالانه CA$100,000 یا بیشتر درآمد دارند &mdash; بالاترین سهم در میان همه گروه‌های سنی."),
  ca_inc_c2_note = list(
    en = "Reports each individual&rsquo;s own pre-tax earnings by age. Household income, by contrast, sums every earner in a household into one total before ranking it against the national distribution.",
    fa = "درآمد پیش از مالیات هر فرد را به تفکیک سن گزارش می‌کند. در مقابل، درآمد خانوار درآمد همه اعضای شاغل خانوار را جمع می‌زند و سپس آن را با توزیع ملی مقایسه می‌کند."),
  ca_src_income = list(
    en = " — 2021 Canadian Census<br>Ages 25–54 (prime working years). Each decile holds 10% of all Canadian households, ranked by pre-tax household income.<br>Smaller estimates should be interpreted with caution.",
    fa = " — سرشماری 2021 کانادا<br>25–54 سال (سال‌های اصلی کار). هر دهک 10 درصد از کل خانوارهای کانادا را در بر می‌گیرد، رتبه‌بندی‌شده بر پایه درآمد خانوار پیش از مالیات.<br>برآوردهای کوچک‌تر را باید با احتیاط تفسیر کرد."),
  ca_src_income_age = list(
    en = " — 2021 Canadian Census<br>First generation, ages 25–74. Pre-tax personal income. Smaller estimates should be interpreted with caution.",
    fa = " — سرشماری 2021 کانادا<br>نسل اول، 25–74 سال. درآمد شخصی پیش از مالیات. برآوردهای کوچک‌تر را باید با احتیاط تفسیر کرد.")
)

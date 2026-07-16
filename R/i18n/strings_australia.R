# ============================================================================
# R/i18n/strings_australia.R  — Australia pages string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_australia.R AFTER R/_helpers_i18n.R. Defines the global
# STR list consumed by tr(), plus several AU_*_FA lookup vectors for the
# repeated category-label sets (occupation, industry, education, religion,
# language, generation, labour-force, citizenship, region, income bands).
#
# RULES (same as strings_nl.R / strings_denmark.R):
#   * en values are the EXACT English strings from build_australia.R so the
#     English editions stay BYTE-IDENTICAL.
#   * Every %d / %.0f / %.1f numeric slot is rewritten as %s; the builder passes
#     fa_num()/fmtv() so the number is language-formatted. The percent SIGN
#     lives in the template (%% en, word درصد fa for prose/cards/hovers).
#   * HTML tags / entities (<strong>, <br>, &mdash;, &ndash;, &rsquo;, <em>)
#     mirror the en structure; only human text is swapped in fa. Latin digits
#     in fa prose are converted at runtime by FA_NUM_SCRIPT — keep ASCII digits
#     in literals.
#   * Australia is a COMPOUND count → demonym title (ایرانی‌استرالیایی).
#   * Australian state / city / LGA names stay Latin place names (handled in the
#     builder, not translated here).
# ============================================================================

# --- Category lookup vectors (keyed on the English label in the data) --------
# Persian display label per category. Used for both chart axis/annotation and
# hover text on the fa edition. en uses the English label unchanged.

AU_OCC_FA <- c(
  "Managers"                                = "مدیران",
  "Professionals"                           = "متخصصان",
  "Technicians and Trades Workers"          = "تکنسین‌ها و پیشه‌وران",
  "Community and Personal Service Workers"  = "خدمات اجتماعی و فردی",
  "Clerical and Administrative Workers"     = "کارکنان دفتری و اداری",
  "Sales Workers"                           = "فروشندگان",
  "Machinery Operators and Drivers"         = "اپراتور ماشین‌آلات و رانندگان",
  "Labourers"                               = "کارگران ساده"
)

AU_IND_FA <- c(
  "Agriculture, Forestry and Fishing"                 = "کشاورزی و ماهیگیری",
  "Mining"                                            = "معدن",
  "Manufacturing"                                     = "صنعت و تولید",
  "Electricity, Gas, Water and Waste Services"        = "برق، گاز، آب و پسماند",
  "Construction"                                      = "ساخت‌وساز",
  "Wholesale Trade"                                   = "عمده‌فروشی",
  "Retail Trade"                                      = "خرده‌فروشی",
  "Accommodation and Food Services"                   = "هتل و رستوران",
  "Transport, Postal and Warehousing"                 = "حمل‌ونقل و انبارداری",
  "Information Media and Telecommunications"          = "رسانه و ارتباطات",
  "Financial and Insurance Services"                  = "مالی و بیمه",
  "Rental, Hiring and Real Estate Services"           = "املاک و مستغلات",
  "Professional, Scientific and Technical Services"   = "خدمات تخصصی و فنی",
  "Administrative and Support Services"               = "خدمات اداری و پشتیبانی",
  "Public Administration and Safety"                  = "اداره عمومی و ایمنی",
  "Education and Training"                            = "آموزش",
  "Health Care and Social Assistance"                = "بهداشت و مددکاری اجتماعی",
  "Arts and Recreation Services"                      = "هنر و تفریح",
  "Other Services"                                    = "سایر خدمات"
)

AU_EDU_FA <- c(
  "Postgraduate Degree Level"                          = "فوق‌لیسانس و دکترا",
  "Graduate Diploma and Graduate Certificate Level"    = "دیپلم و گواهی تکمیلی",
  "Bachelor Degree Level"                              = "لیسانس",
  "Advanced Diploma and Diploma Level"                 = "دیپلم و دیپلم عالی",
  "Certificate III & IV Level"                         = "گواهی سطح 3 و 4",
  "Secondary Education - Years 10 and above"           = "متوسطه: سال 10 و بالاتر",
  "Certificate I & II Level"                           = "گواهی سطح 1 و 2",
  "Secondary Education - Years 9 and below"            = "متوسطه: سال 9 و پایین‌تر"
)

AU_REL_FA <- c(
  "Secular/No religion" = "بدون دین",
  "Islam"               = "اسلام",
  "Christianity"        = "مسیحیت",
  "Other Religions"     = "سایر ادیان"
)

AU_LANG_FA <- c(
  "Persian"        = "فارسی",
  "Other language" = "زبان‌های دیگر",
  "English only"   = "فقط انگلیسی"
)

AU_GEN_FA <- c(
  "1st Generation"    = "نسل اول",
  "2nd Generation"    = "نسل دوم",
  "First generation"  = "نسل اول",
  "Second generation" = "نسل دوم"
)

AU_REGION_FA <- c(
  "Eastern"  = "شرقی",
  "Western"  = "غربی",
  "Southern" = "جنوبی",
  "Northern" = "شمالی"
)

# Waterfall generation-group labels (grp column).
AU_WFGRP_FA <- c(
  "First generation"             = "نسل اول",
  "Second generation"            = "نسل دوم",
  "Iranian ancestry or language" = "اصالت یا زبان ایرانی"
)

# Citizenship: x-axis labels carry a literal "\n"; hover collapses it to a space.
AU_CIT_X_FA <- c(
  "Australian\ncitizen"        = "شهروند\nاسترالیا",
  "Not an Australian\ncitizen" = "غیرشهروند\nاسترالیا"
)
AU_CIT_HOVER_FA <- c(
  "Australian citizen"        = "شهروند استرالیا",
  "Not an Australian citizen" = "غیرشهروند استرالیا"
)

# Labour force: short x-axis label (some two-line via "\n") and full hover label.
AU_LF_SHORT_FA <- c(
  "Full-time"           = "تمام‌وقت",
  "Part-time"           = "پاره‌وقت",
  "Away from work"      = "غایب از کار",
  "Unemp. (FT)"         = "بیکار (تمام‌وقت)",
  "Unemp. (PT)"         = "بیکار (پاره‌وقت)",
  "Not in\nlabor force" = "خارج از\nنیروی کار"
)
AU_LF_FULL_FA <- c(
  "Employed, worked full-time"                  = "شاغل تمام‌وقت",
  "Employed, worked part-time"                  = "شاغل پاره‌وقت",
  "Employed, away from work"                    = "شاغل غایب از کار",
  "Unemployed, looking for full-time work"      = "بیکار جویای کار تمام‌وقت",
  "Unemployed, looking for part-time work"      = "بیکار جویای کار پاره‌وقت",
  "Not in the labour force"                     = "خارج از نیروی کار"
)

# Income weekly bands: only the two named (non-$) bands need translation; the
# $-range bands pass through unchanged (their digits Persian-ize at runtime).
# Keyed on the transformed x-axis label AND the raw hover band.
AU_INCBAND_X_FA <- c("Negative" = "منفی")                    # x-axis (transformed) label
AU_INCBAND_HOVER_FA <- c("Negative income" = "درآمد منفی",
                         "Nil income"       = "بدون درآمد")   # raw hover band

# Decile ordinal labels (en ordinals; fa bare numbers, Persian-ized at runtime).
AU_DECILE_EN <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
AU_DECILE_FA <- as.character(1:10)

STR <- list(

  # ==========================================================================
  # SHARED
  # ==========================================================================
  au_of_total = list(
    en = "of total",
    fa = "از کل"),
  au_bignum_pct = list(       # 36px factoid big number
    en = "%s%%",
    fa = "%s درصد"),

  # ==========================================================================
  # POPULATION
  # ==========================================================================
  au_pop_title = list(
    en = "Australia: Population",
    fa = "استرالیا: جمعیت"),

  # --- three census measures --------------------------------------------------
  au_measures_title = list(
    en = "Three Census Measures",
    fa = "سه معیار سرشماری"),
  au_box_ancestry = list(
    en = "Iranian<br>ancestry",
    fa = "اصالت<br>ایرانی"),
  au_box_persian = list(
    en = "Persian speakers<br>(excl. Dari)",
    fa = "فارسی‌زبانان<br>(به‌جز دری)"),
  au_box_born = list(
    en = "Born in<br>Iran",
    fa = "متولد<br>ایران"),

  # --- region chart -----------------------------------------------------------
  au_region_title = list(
    en = "<b>Iran-Born Australians by<br>Region of Residence</b>",
    fa = "<b>متولدان ایران در استرالیا<br>به تفکیک منطقه سکونت</b>"),
  au_region_hover = list(
    en = "<b>%s Australia</b><br>%s Iran-born (%s%%)",
    fa = "<b>استرالیای %s</b><br>%s نفر متولد ایران (%s درصد)"),

  # --- maps -------------------------------------------------------------------
  au_map_hover = list(
    en = "<b>%s</b><br>%s Iran-born<br>%s%% of total",
    fa = "<b>%s</b><br>%s نفر متولد ایران<br>%s درصد از کل"),
  au_geo_section = list(
    en = "Geographic Distribution of Iran-Born Australians",
    fa = "توزیع جغرافیایی متولدان ایران در استرالیا"),
  au_tab_bystate = list(
    en = "By State",
    fa = "به تفکیک ایالت"),
  au_tab_sydney = list(
    en = "Greater Sydney",
    fa = "سیدنی بزرگ"),
  au_tab_melbourne = list(
    en = "Greater Melbourne",
    fa = "ملبورن بزرگ"),
  au_src_syd_suffix = list(
    en = "<br>Local Government Areas in Greater Sydney with 10+ Iran-born residents.",
    fa = "<br>مناطق شهرداری در سیدنی بزرگ با 10 نفر یا بیشتر ساکن متولد ایران."),
  au_src_melb_suffix = list(
    en = "<br>Local Government Areas in Greater Melbourne with 10+ Iran-born residents.",
    fa = "<br>مناطق شهرداری در ملبورن بزرگ با 10 نفر یا بیشتر ساکن متولد ایران."),

  # --- waterfall + criteria grid ---------------------------------------------
  au_wf_title = list(
    en = "<b>Iranian-Australians: How We Count</b>",
    fa = "<b>ایرانی‌استرالیایی‌ها: روش شمارش</b>"),
  au_wf_hover = list(
    en = "<b>%s</b><br>%s Born in Iran<br>%s Iranian ancestry<br>%s Persian language<br>%s Iran-born parent<br><br>%s people · %s%% of total<br>Running total: %s",
    fa = "<b>%s</b><br>%s متولد ایران<br>%s اصالت ایرانی<br>%s زبان فارسی<br>%s والد متولد ایران<br><br>%s نفر · %s درصد از کل<br>مجموع تجمعی: %s"),
  au_crit_born = list(
    en = "Born in<br>Iran",
    fa = "متولد<br>ایران"),
  au_crit_ancestry = list(
    en = "Iranian<br>ancestry",
    fa = "اصالت<br>ایرانی"),
  au_crit_language = list(
    en = "Persian<br>language",
    fa = "زبان<br>فارسی"),
  au_crit_parent = list(
    en = "Iran-born<br>parent",
    fa = "والد<br>متولد ایران"),

  # --- headline ---------------------------------------------------------------
  au_pop_headline_label = list(
    en = "Estimated Iranian-Australian Population",
    fa = "برآورد جمعیت ایرانی‌استرالیایی"),
  au_pop_headline_caption = list(
    en = "%s, Census of Population and Housing, 2021",
    fa = "%s، سرشماری جمعیت و مسکن 2021"),
  au_pop_idbox_intro = list(
    en = "A person is counted if they meet <em>at least one</em> of four criteria:",
    fa = "هر فرد در صورت داشتن <em>دست‌کم یکی</em> از این چهار معیار شمرده می‌شود:"),
  au_pop_idbox_b1 = list(
    en = '<strong>Country of birth</strong> <span style="color:#6b6b6b;">&mdash; born in Iran</span>',
    fa = '<strong>کشور محل تولد</strong> <span style="color:#6b6b6b;">&mdash; متولد ایران</span>'),
  au_pop_idbox_b2 = list(
    en = '<strong>Ancestry</strong> <span style="color:#6b6b6b;">&mdash; reports Iranian ancestry</span>',
    fa = '<strong>اصالت</strong> <span style="color:#6b6b6b;">&mdash; اعلام اصالت ایرانی</span>'),
  au_pop_idbox_b3 = list(
    en = '<strong>Language at home</strong> <span style="color:#6b6b6b;">&mdash; speaks Persian (excluding Dari)</span>',
    fa = '<strong>زبان خانه</strong> <span style="color:#6b6b6b;">&mdash; صحبت به فارسی (به‌جز دری)</span>'),
  au_pop_idbox_b4 = list(
    en = '<strong>Parental birthplace</strong> <span style="color:#6b6b6b;">&mdash; Australian-born with at least one Iran-born parent</span>',
    fa = '<strong>محل تولد والدین</strong> <span style="color:#6b6b6b;">&mdash; متولد استرالیا با دست‌کم یک والد متولد ایران</span>'),
  au_pop_idbox_note = list(
    en = "The compound total counts each person once, even if they meet more than one criterion.",
    fa = "مجموع ترکیبی هر فرد را یک بار می‌شمارد، حتی اگر بیش از یک معیار را داشته باشد."),

  # --- generation boxes (rendered on immigration page) -----------------------
  au_genbox_title = list(
    en = "Iranian-Ancestry Population by Generation",
    fa = "جمعیت با اصالت ایرانی به تفکیک نسل"),
  au_genbox_1_label = list(
    en = "First generation",
    fa = "نسل اول"),
  au_genbox_1_sub = list(
    en = "Born overseas",
    fa = "متولد خارج"),
  au_genbox_2_label = list(
    en = "Second generation",
    fa = "نسل دوم"),
  au_genbox_2_sub = list(
    en = "Australian-born",
    fa = "متولد استرالیا"),
  au_genbox_caption = list(
    en = "Of the %s Australians who report Iranian ancestry. About %s%% of the first generation were born outside Iran (largely diaspora re-migration via India, Afghanistan and the UK).",
    fa = "از مجموع %s استرالیایی که اصالت ایرانی اعلام می‌کنند. حدود %s درصد از نسل اول خارج از ایران متولد شده‌اند (بیشتر مهاجرت دوباره ایرانیان مهاجر از هند، افغانستان و بریتانیا)."),

  # ==========================================================================
  # IMMIGRATION
  # ==========================================================================
  au_immig_title = list(
    en = "Australia: Immigration",
    fa = "استرالیا: مهاجرت"),
  au_arrival_title = list(
    en = "<b>Iranian Migration to Australia:<br>Arrivals by Period</b>",
    fa = "<b>مهاجرت ایرانیان به استرالیا:<br>ورود به تفکیک دوره</b>"),
  au_arrival_hover_period = list(
    en = "<b>%s</b><br>%s arrivals (%s/year avg)<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s نفر واردشده (میانگین %s در سال)<br>تجمعی: %s درصد"),
  au_arrival_hover_annual = list(
    en = "<b>%s</b><br>%s arrivals<br>Cumulative: %s%%",
    fa = "<b>%s</b><br>%s نفر واردشده<br>تجمعی: %s درصد"),
  au_arrival_hover_cum = list(
    en = "<b>%s</b><br>%s%% of today's Iran-born population had arrived",
    fa = "<b>%s</b><br>%s درصد از جمعیت کنونی متولدان ایران تا این زمان وارد شده بودند"),
  au_arrival_before1971 = list(
    en = "Before 1971",
    fa = "پیش از 1971"),
  au_arrival_footnote = list(
    en = "Pre-2016 values show period arrivals averaged per year. 2016–2021 show annual counts.",
    fa = "مقادیر پیش از 2016 ورودهای دوره‌ای را به‌صورت میانگین سالانه نشان می‌دهند. سال‌های 2016–2021 شمارش سالانه‌اند."),

  au_cit_title = list(
    en = "<b>Citizenship Status<br>of Iran-Born Australians</b>",
    fa = "<b>وضعیت تابعیت<br>متولدان ایران در استرالیا</b>"),
  au_cit_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s نفر (%s درصد)"),

  au_im_card1_primary = list(
    en = "of Iran-born Australians arrived during 2011&ndash;2015.",
    fa = "از متولدان ایران در استرالیا طی سال‌های 2011&ndash;2015 وارد شده‌اند."),
  au_im_card1_note = list(
    en = "Arrivals have continued at about 2,500&ndash;3,500 per year since 2016.",
    fa = "ورود مهاجران از سال 2016 با نرخ حدود 2,500&ndash;3,500 نفر در سال ادامه یافته است."),
  au_im_card2_primary = list(
    en = "of Iran-born Australians hold Australian citizenship.",
    fa = "از متولدان ایران در استرالیا تابعیت استرالیا دارند."),
  au_im_card2_note = list(
    en = "Eligible residents can apply for citizenship after four years in Australia.",
    fa = "ساکنان واجد شرایط می‌توانند پس از چهار سال اقامت در استرالیا برای تابعیت درخواست دهند."),
  au_tab_citizenship = list(
    en = "Citizenship",
    fa = "تابعیت"),
  au_tab_bygeneration = list(
    en = "By Generation",
    fa = "به تفکیک نسل"),

  # ==========================================================================
  # EDUCATION & RELIGION
  # ==========================================================================
  au_edu_title = list(
    en = "Australia: Education & Religion",
    fa = "استرالیا: تحصیلات و دین"),
  au_edu_chart_title = list(
    en = "<b>Highest Educational Attainment<br>of Iran-Born Australians</b>",
    fa = "<b>بالاترین سطح تحصیلات<br>متولدان ایران در استرالیا</b>"),
  au_edu_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s نفر (%s درصد)"),
  au_rel_title = list(
    en = "<b>Religion of Iranian-Australians<br>by Generation</b>",
    fa = "<b>دین ایرانی‌استرالیایی‌ها<br>به تفکیک نسل</b>"),
  au_rel_hover = list(
    en = "<b>%s</b><br>%s<br>%s%%",
    fa = "<b>%s</b><br>%s<br>%s درصد"),
  au_lang_chart_title = list(
    en = "<b>Language Spoken at Home by<br>Iranian-Australians, by Generation</b>",
    fa = "<b>زبان رایج در خانه نزد<br>ایرانی‌استرالیایی‌ها، به تفکیک نسل</b>"),
  au_lang_hover = list(
    en = "<b>%s</b><br>%s<br>%s people (%s%%)",
    fa = "<b>%s</b><br>%s<br>%s نفر (%s درصد)"),
  au_tab_qualifications = list(
    en = "Qualifications",
    fa = "مدارک تحصیلی"),
  au_tab_language = list(
    en = "Language",
    fa = "زبان"),

  au_ed_card1_primary = list(
    en = "of Iran-born Australian adults (15+) hold a bachelor&rsquo;s degree or higher.",
    fa = "از بزرگسالان متولد ایران در استرالیا (15 سال و بالاتر) دارای مدرک لیسانس یا بالاتر هستند."),
  au_ed_card1_note = list(
    en = "%s%% hold a postgraduate degree &mdash; about half of all bachelor&rsquo;s-or-higher holders.",
    fa = "%s درصد تحصیلات تکمیلی دارند &mdash; حدود نیمی از دارندگان مدرک لیسانس و بالاتر."),
  au_ed_card2_primary = list(
    en = "of Iranian-ancestry Australians report no religious affiliation.",
    fa = "از استرالیایی‌های دارای اصالت ایرانی خود را پیرو هیچ دینی نمی‌دانند."),
  au_ed_islam = list(
    en = "Islam %s%%",
    fa = "اسلام %s درصد"),
  au_ed_christ = list(
    en = "Christianity 12%",
    fa = "مسیحیت 12 درصد"),

  # ==========================================================================
  # WORK & INCOME
  # ==========================================================================
  au_workinc_title = list(
    en = "Australia: Work & Income",
    fa = "استرالیا: کار و درآمد"),
  au_occ_title = list(
    en = "<b>Occupation of Iran-Born Workers<br>in Australia</b>",
    fa = "<b>شغل شاغلان متولد ایران<br>در استرالیا</b>"),
  au_occ_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s نفر (%s درصد)"),
  au_ind_title = list(
    en = "<b>Industry of Employment<br>of Iran-Born Workers in Australia</b>",
    fa = "<b>بخش اقتصادی اشتغال<br>شاغلان متولد ایران در استرالیا</b>"),
  au_ind_hover = list(
    en = "<b>%s</b><br>%s (%s%%)",
    fa = "<b>%s</b><br>%s نفر (%s درصد)"),
  au_inc_title = list(
    en = "<b>Weekly Personal Income<br>of Iran-Born Australians</b>",
    fa = "<b>درآمد شخصی هفتگی<br>متولدان ایران در استرالیا</b>"),
  au_inc_hover = list(
    en = "<b>%s weekly</b><br>%s (%s%%)",
    fa = "<b>%s هفتگی</b><br>%s نفر (%s درصد)"),
  au_dec_title = list(
    en = "<b>Position in Australian<br>Personal Income Distribution:<br>Iran-Born (Ages 25–54)</b>",
    fa = "<b>جایگاه در توزیع درآمد شخصی<br>استرالیا:<br>متولدان ایران (25 تا 54 سال)</b>"),
  au_dec_hover = list(
    en = "<b>Decile:</b> %s<br><b>Count:</b> %s<br><b>Share:</b> %s%%",
    fa = "<b>دهک:</b> %s<br><b>تعداد:</b> %s<br><b>سهم:</b> %s درصد"),
  au_dec_xaxis = list(
    en = "Income Decile (Lowest to Highest)",
    fa = "دهک درآمدی (کمترین تا بیشترین)"),
  au_dec_annotation = list(
    en = "10% =<br>national<br>baseline",
    fa = "10٪ =<br>سهم پایه<br>هر دهک"),
  au_lf_title = list(
    en = "<b>Labor Force Status<br>of Iran-Born Australians</b>",
    fa = "<b>وضعیت نیروی کار<br>متولدان ایران در استرالیا</b>"),
  au_lf_hover = list(
    en = "<b>%s</b><br>%s (%s%% of pop. 15+)",
    fa = "<b>%s</b><br>%s نفر (%s درصد از جمعیت 15 سال و بالاتر)"),
  au_tab_occupation = list(
    en = "Occupation",
    fa = "شغل"),
  au_tab_industry = list(
    en = "Industry",
    fa = "بخش اقتصادی"),
  au_tab_laborforce = list(
    en = "Labor Force",
    fa = "نیروی کار"),
  au_tab_incdeciles = list(
    en = "Income Deciles",
    fa = "دهک‌های درآمدی"),
  au_tab_weeklyincome = list(
    en = "Weekly Income",
    fa = "درآمد هفتگی"),

  au_wi_card1_primary = list(
    en = "of employed Iran-born Australians work as professionals &mdash; the largest occupation group.",
    fa = "از شاغلان متولد ایران در استرالیا به‌عنوان متخصص کار می‌کنند &mdash; بزرگ‌ترین گروه شغلی."),
  au_wi_card1_ind = list(
    en = "%s%% work in %s &mdash; the largest industry",
    fa = "%s درصد در %s کار می‌کنند &mdash; بزرگ‌ترین بخش اقتصادی"),
  au_wi_card1_part = list(
    en = "Overall labor-force participation is %s%%",
    fa = "نرخ کلی مشارکت در نیروی کار %s درصد است"),
  au_wi_card2_primary = list(
    en = "of Iran-born Australians aged 25&ndash;54 are in the bottom two deciles of <strong>personal</strong> income.",
    fa = "از متولدان ایران در استرالیا در گروه سنی 25&ndash;54 سال در دو دهک پایین درآمد <strong>شخصی</strong> قرار دارند."),
  au_wi_card2_top = list(
    en = "%s%% are in the top two deciles",
    fa = "%s درصد در دو دهک بالا قرار دارند"),
  au_wi_card2_median = list(
    en = "Median weekly personal income band: %s",
    fa = "دامنه میانه درآمد شخصی هفتگی: %s"),
  au_wi_card2_household = list(
    en = "Iran-born <strong>household</strong> income is above the national median (A$1,927 vs A$1,746 weekly)",
    fa = "درآمد <strong>خانوار</strong> متولدان ایران بالاتر از میانه ملی است (A$1,927 در برابر A$1,746 در هفته)"),

  # ==========================================================================
  # SOURCE LINES  (link passed as %s; en link text preserved byte-identical)
  # ==========================================================================
  au_src_abs = list(
    en = "Source: %s — Census of Population and Housing, 2021<br>Iran-born population enumerated via country of birth question.",
    fa = "منبع: %s — سرشماری جمعیت و مسکن 2021<br>جمعیت متولد ایران بر پایه پرسش کشور محل تولد شمارش شده است."),
  au_src_waterfall = list(
    en = "Source: %s — Census of Population and Housing, 2021. Cell counts are randomly adjusted by ABS to prevent identification of individuals; totals may not sum exactly.",
    fa = "منبع: %s — سرشماری جمعیت و مسکن 2021. ABS ارقام جدول‌ها را برای جلوگیری از شناسایی افراد به‌طور تصادفی اندکی تغییر می‌دهد؛ ممکن است مجموع‌ها دقیقاً برابر نباشند."),
  au_src_genboxes = list(
    en = "Source: %s &mdash; Census 2021, ancestry by country of birth.",
    fa = "منبع: %s &mdash; سرشماری 2021، اصالت به تفکیک کشور محل تولد."),
  au_src_language = list(
    en = "Source: %s — Census 2021, language used at home by Iranian-ancestry population (1st gen = born overseas, 2nd gen = Australian-born).",
    fa = "منبع: %s — سرشماری 2021، زبان رایج در خانه نزد جمعیت با اصالت ایرانی (نسل اول = متولد خارج، نسل دوم = متولد استرالیا)."),
  au_src_religion = list(
    en = "Source: %s — Census 2021. Iranian ancestry population, 1st gen = Iran-born, 2nd gen = Australian-born.",
    fa = "منبع: %s — سرشماری 2021. جمعیت با اصالت ایرانی، نسل اول = متولد ایران، نسل دوم = متولد استرالیا."),
  au_src_income = list(
    en = "Source: %s — Census 2021<br>Ages 25–54 (prime working years).<br>Each decile holds 10%% of all Australians, ranked by pre-tax personal income.",
    fa = "منبع: %s — سرشماری 2021<br>سنین 25–54 سال (سال‌های اصلی کار).<br>هر دهک 10 درصد از همه استرالیایی‌ها را در بر می‌گیرد، مرتب‌شده بر پایه درآمد شخصی پیش از مالیات.")
)

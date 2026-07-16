# ============================================================================
# R/i18n/strings_france.R  — France page string table (en / fa)
# ----------------------------------------------------------------------------
# Sourced by R/build_france.R AFTER R/_helpers_i18n.R. Defines the global STR
# list consumed by tr(). Each entry is list(en = ..., fa = ...).
#
# RULES honored here (same as strings_nl.R / strings_armenia.R):
#   * en values are the EXACT English strings from build_france.R (source of
#     truth) so the English edition stays byte-identical.
#   * Every %d / format() slot from the original is rewritten as %s; the builder
#     passes fa_num()/fmtv() so the number is language-formatted.
#   * HTML tags / entities (<strong>, <br>, &mdash;) mirror the en structure;
#     only the human text is swapped in fa. ASCII digits are KEPT in fa literals
#     (Iranian economic-press convention) — FA_NUM_SCRIPT Persian-izes them at
#     runtime.
#   * Official agency names (INSEE) and the French census name / survey question
#     stay Latin in both editions, as they are on the English page.
#
# France framing: INSEE "immigré" is not used for the headline; the page counts
# Iran-BORN residents (متولد ایران) — title "Estimated Iran-Born Population in
# France".
# ============================================================================

STR <- list(

  # --- page + headline --------------------------------------------------------
  fr_pop_title = list(
    en = "France: Population",
    fa = "فرانسه: جمعیت"),
  fr_pop_headline_label = list(
    en = "Estimated Iran-Born Population in France",
    fa = "برآورد جمعیت متولدان ایران در فرانسه"),
  fr_pop_headline_caption = list(
    en = "Based on the Recensement de la population maintained by %s, %s",
    fa = "بر پایه سرشماری جمعیت فرانسه، انجام‌شده توسط %s، %s"),

  # --- identification box ------------------------------------------------------
  fr_pop_idbox_intro = list(
    en = "France uses a rolling annual census. A person is counted as Iran-born based on:",
    fa = "فرانسه از سرشماری چرخشی سالانه استفاده می‌کند. هر فرد بر این اساس متولد ایران به شمار می‌آید:"),
  fr_pop_idbox_bullet1 = list(
    en = '<strong>Country of birth</strong> <span style="color:#6b6b6b;">&mdash; "Dans quel pays êtes-vous né(e)?"</span>',
    fa = '<strong>کشور محل تولد</strong> <span style="color:#6b6b6b;">&mdash; «Dans quel pays êtes-vous né(e)?»</span>'),
  fr_pop_law_note = list(
    en = "French law (Loi Informatique et Libertés, Art. 8) prohibits collecting ethnicity or ancestry statistics, so French-born children of Iran-born parents are not separately identified. INSEE’s detailed country-of-birth tables are also released with a multi-year delay, so %s is the most recent Iran-specific figure currently published.",
    fa = "قانون فرانسه (Loi Informatique et Libertés، ماده 8) گردآوری آمار قومیت یا تبار را ممنوع کرده است؛ از این‌رو فرزندان متولد فرانسه با والدین متولد ایران به‌طور جداگانه شناسایی نمی‌شوند. جدول‌های تفصیلی کشور محل تولد INSEE نیز با چند سال تأخیر منتشر می‌شوند، بنابراین %s تازه‌ترین رقم مربوط به ایران است که تاکنون منتشر شده است."),

  # --- trend chart -------------------------------------------------------------
  fr_hist_title = list(
    en = "<b>Iran-Born Population in France,<br>%s–%s</b>",
    fa = "<b>جمعیت متولدان ایران در فرانسه،<br>%s–%s</b>"),
  fr_hist_hover = list(
    en = "<b>%s</b><br>%s Iran-born",
    fa = "<b>%s</b><br>%s نفر متولد ایران"),

  # --- source line -------------------------------------------------------------
  fr_src_insee = list(
    en = "Source: %s &mdash; Recensement de la population, Iran-born residents 1968–2019 (periodic censuses 1968–1999, annual from 2006)",
    fa = "منبع: %s &mdash; سرشماری جمعیت فرانسه، ساکنان متولد ایران 1968–2019 (سرشماری‌های دوره‌ای 1968–1999، سالانه از 2006)")
)

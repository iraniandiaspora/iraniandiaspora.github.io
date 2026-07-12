# ============================================================================
# R/_helpers_i18n.R  — Persian (Farsi) edition helpers
# ----------------------------------------------------------------------------
# ADDITIVE. Sourced ALONGSIDE R/_helpers.R by bilingual builders only. It does
# NOT modify the shared page_template() and does NOT touch the 18 English-only
# builders. The English (ltr) output stays byte-identical because the en path
# delegates to the canonical page_template(); the fa path is a *transform* of
# that same canonical shell.
#
# Requires R/_helpers.R to be sourced FIRST (uses page_template(), plotly_*()).
#
# Provides:
#   LANG            global "en"|"fa"; the builder sets it in its language loop
#   is_fa()         TRUE when building the Persian edition
#   fa_digits(s)    Western -> Persian digits in an already-built string
#   fa_num(x,dec)   locale number: en = "1,234"; fa = "۱٬۲۳۴" (٬ ٫)
#   fa_pct(x,dec)   percent: en = "43%"; fa = "۴۳٪"
#   bdi(x)          wrap a Latin/number run in <bdi> for bidi isolation
#   tr(key)         active-language string from the global STR table (fails loud)
#   pj(p)           plotly_to_json(p) + Vazirmatn font on fa charts
#   page_template_i18n(title, body, has_tabs, lang)   lang/dir-aware shell
# ============================================================================

if (!exists("LANG")) LANG <- "en"
is_fa <- function() identical(LANG, "fa")

# --- Persian digits / numbers -------------------------------------------------
# U+06F0..06F9 digits; U+066C thousands; U+066B decimal; U+066A percent.
fa_digits <- function(s) {
  chartr("0123456789",
         "۰۱۲۳۴۵۶۷۸۹",
         as.character(s))
}

# fa_num(): group-format a number, then (fa only) Persian digits + separators.
# en output is identical to format(x, big.mark = ",").
fa_num <- function(x, dec = 0, big = TRUE) {
  s <- formatC(x, format = "f", digits = dec, big.mark = if (big) "," else "")
  if (!is_fa()) return(s)
  s <- fa_digits(s)
  s <- gsub(",", "٬", s, fixed = TRUE)   # thousands -> ٬
  s <- gsub(".", "٫", s, fixed = TRUE)   # decimal   -> ٫
  s
}

# fa_pct(): value already on a 0-100 scale. en "43%" ; fa "۴۳٪".
fa_pct <- function(x, dec = 0) {
  paste0(fa_num(x, dec), if (is_fa()) "٪" else "%")
}

# bdi(): isolate an embedded opposite-direction run (Latin agency name, URL,
# a Latin-adjacent number) so the bidi algorithm can't reorder it.
bdi <- function(x) paste0("<bdi>", x, "</bdi>")

# --- Translation lookup -------------------------------------------------------
# The builder defines a global STR = list(key = list(en=, fa=)). tr() returns
# the active-language value and FAILS LOUD on a missing key / missing fa, so an
# FA page can never silently ship with an untranslated (or English) string.
tr <- function(key) {
  if (!exists("STR")) stop("tr(): no STR table in scope", call. = FALSE)
  v <- STR[[key]]
  if (is.null(v)) stop(sprintf("tr(): unknown key '%s'", key), call. = FALSE)
  out <- v[[LANG]]
  if (is.null(out) || (is.character(out) && !nzchar(out)))
    stop(sprintf("tr(): key '%s' has no '%s' value", key, LANG), call. = FALSE)
  out
}

# --- Persian Plotly locale ----------------------------------------------------
# Registered once per FA page (after the Plotly <script>). setPlotConfig makes
# EVERY chart on the page inherit {locale:"fa"} without touching plotly_div():
# Plotly then renders auto axis ticks + any tickformat/hoverformat number in
# Persian digits with ٫ / ٬ separators. Prebaked hover *text* strings are NOT
# reached by this (they are opaque to Plotly) — those are Persian-digited in the
# builder via fa_num()/fa_digits().
FA_LOCALE_JS <- paste0(
  '<script>(function(){if(!window.Plotly)return;',
  'Plotly.register({moduleType:"locale",name:"fa",dictionary:{},format:{',
  'decimal:"٫",thousands:"٬",grouping:[3],percent:"٪",',
  'numerals:["۰","۱","۲","۳","۴","۵","۶","۷","۸","۹"]}});',
  'Plotly.setPlotConfig({locale:"fa",separators:"٫٬"});})();</script>')

# --- Persian font (self-hosted Vazirmatn) -------------------------------------
# Relative "lib/fonts/..." resolves the same from docs/pages/*.fa.html as the
# vendored Plotly does (docs/pages/lib/). Latin falls back to Montserrat.
FA_FONT_HEAD <- paste0(
  '<style>',
  '@font-face{font-family:"Vazirmatn";font-style:normal;font-weight:400;',
  'font-display:swap;src:url("lib/fonts/Vazirmatn-Regular.woff2") format("woff2");}',
  '@font-face{font-family:"Vazirmatn";font-style:normal;font-weight:700;',
  'font-display:swap;src:url("lib/fonts/Vazirmatn-Bold.woff2") format("woff2");}',
  '</style>')

# Minimal RTL overrides for the few inline-styled bits page_template()/plotly_div()
# emit that a stylesheet + logical-property body cannot reach (the source-footer
# <p> carries an inline text-align:right).
FA_RTL_OVERRIDES <- paste0(
  '<style>',
  '[dir="rtl"] .chart-card p[style*="text-align:right"]{text-align:left !important;}',
  '[dir="rtl"] body{text-align:right;}',
  '[dir="rtl"] .headline div[style*="text-align:left"]{text-align:right !important;}',
  '[dir="rtl"] .headline ul[style*="padding-left"]{padding-left:0 !important;padding-right:20px !important;}',
  '[dir="rtl"] .text-card ul[style*="padding-left"]{padding-left:0 !important;padding-right:18px !important;}',
  '</style>')

# --- FA_NUM_SCRIPT: universal Persian-digit pass ------------------------------
# The Plotly `fa` locale does NOT reliably Persian-ize auto axis ticks / colorbar
# in 3.4.0, and prose years baked as literals (source lines) also stay Latin.
# This runtime pass converts Latin digits in every VISIBLE text node to Persian
# (۰-۹), and swaps , -> ٬ and . -> ٫ inside chart text specifically. It walks
# text nodes only and SKIPS <script>/<style>, so it can never corrupt the Plotly
# data JSON or CSS. Re-runs after every plotly_afterplot so interaction (zoom,
# tab switch, hover re-render) stays Persian. Injected before </body> on fa pages.
FA_NUM_SCRIPT <- paste0(
  '<script>(function(){',
  'var D={"0":"۰","1":"۱","2":"۲","3":"۳","4":"۴","5":"۵","6":"۶","7":"۷","8":"۸","9":"۹"};',
  'function fd(s){return s.replace(/[0-9]/g,function(d){return D[d];});}',
  'function digitsPass(){',
  'var w=document.createTreeWalker(document.body,NodeFilter.SHOW_TEXT,{acceptNode:function(n){',
  'var p=n.parentNode;if(!p)return NodeFilter.FILTER_REJECT;',
  'var t=p.nodeName;if(t==="SCRIPT"||t==="STYLE")return NodeFilter.FILTER_REJECT;',
  'return /[0-9]/.test(n.nodeValue)?NodeFilter.FILTER_ACCEPT:NodeFilter.FILTER_SKIP;}});',
  'var a=[],n;while(n=w.nextNode())a.push(n);',
  'a.forEach(function(x){x.nodeValue=fd(x.nodeValue).split(",").join("٬");});}',
  'function sepPass(){document.querySelectorAll(".js-plotly-plot text").forEach(function(el){',
  'if(el.textContent.indexOf(".")<0)return;',
  'el.textContent=el.textContent.split(".").join("٫");});}',
  'function run(){digitsPass();sepPass();}',
  'function hook(){document.querySelectorAll(".js-plotly-plot").forEach(function(p){',
  'if(p.on&&!p._faHooked){p._faHooked=true;p.on("plotly_afterplot",function(){setTimeout(run,0);});}});}',
  'window.addEventListener("load",function(){run();hook();setTimeout(function(){run();hook();},1500);});',
  '})();</script>')

# --- pj(): plotly JSON with Vazirmatn on fa charts ----------------------------
# plotly_to_json() hardcodes Montserrat on layout.font; on fa pages we want
# Vazirmatn first so Persian glyphs in titles/hover render in the site face.
pj <- function(p, inject_hoveron = FALSE) {
  j <- plotly_to_json(p, inject_hoveron = inject_hoveron)
  if (is_fa()) {
    j$layout <- gsub("Montserrat, sans-serif",
                     "Vazirmatn, Montserrat, sans-serif", j$layout, fixed = TRUE)
  }
  j
}

# --- page_template_i18n() -----------------------------------------------------
# en/ltr: identical to page_template() (delegated -> byte-for-byte English).
# fa    : transform the canonical shell -> RTL Persian.
# fa_shell(): turn ANY builder's English shell HTML into the Persian RTL shell.
# Reusable so bespoke builders (build_global.R) and local-template builders
# (build_armenia.R) can go bilingual without migrating, keeping their English
# output byte-identical. Assumes the shell contains the standard anchors
# (`<html lang="en">`, `<meta charset="utf-8">`, `</head>`, `</body>`,
# `font-family:"Montserrat",sans-serif`); a builder whose markup differs should
# align those tokens so the subs apply (verify dir="rtl"/Vazirmatn present).
fa_shell <- function(html) {
  html <- sub('<html lang="en">', '<html lang="fa" dir="rtl">', html, fixed = TRUE)
  html <- sub('<meta charset="utf-8">',
              '<meta charset="utf-8">\n<meta name="robots" content="noindex,nofollow">',
              html, fixed = TRUE)
  html <- sub('</head>',
              paste0(FA_FONT_HEAD, "\n", FA_LOCALE_JS, "\n",
                     FA_RTL_OVERRIDES, "\n</head>"),
              html, fixed = TRUE)
  html <- gsub('font-family:"Montserrat",sans-serif',
               'font-family:"Vazirmatn","Montserrat",sans-serif', html, fixed = TRUE)
  html <- sub('</body>', paste0(FA_NUM_SCRIPT, "\n</body>"), html, fixed = TRUE)
  html
}

# page_template_i18n(): en delegates to page_template() (byte-identical); fa is
# the canonical shell run through fa_shell().
page_template_i18n <- function(title, body_html, has_tabs = FALSE, lang = "en") {
  html <- page_template(title, body_html, has_tabs)
  if (identical(lang, "fa")) html <- fa_shell(html)
  html
}

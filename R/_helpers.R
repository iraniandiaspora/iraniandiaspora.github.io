# Shared helpers for all R/build_*.R scripts.
#
# Every build script should start with:
#   source("R/_helpers.R")
# (run from the iraniandiaspora.github.io/ directory).
#
# What this file provides:
#   strip_internal_classes()  - clean plotly_build() output for JSON serialization
#   plotly_to_json()          - plotly object -> {data, layout, config} JSON triple
#   plotly_div()              - assemble the <div>+<script> block for a chart,
#                               with optional source footer, HTML legend, and
#                               hover-highlight JS
#   iframe_resize_script      - the iframe auto-resize <script> block used on
#                               every page (posts height back to the parent)
#   MAPBOX_ATTRIB_HIDE_CSS    - CSS snippet that hides the Mapbox "i"
#                               attribution button on choroplethmapbox layers;
#                               inject into each page's <style> block

# --- Categorical color palette (Okabe-Ito) ------------------------------------
# ONE shared qualitative palette for EVERY categorical multi-category bar
# (industry, occupation, etc.) site-wide. Okabe-Ito is the colorblind-safe
# Nature Methods standard. Ordered for bars on white: strong, brand-aligned
# hues first; pale yellow and black last (only reached when many categories
# are shown).
# RULE: cap categories at 8 and bucket the smallest tail into a single "Other"
# row (CAT_OTHER gray) — qualitative palettes stop being distinguishable past
# ~8 colors, so NEVER recycle a hue. Germany's suppressed-cell gray (#c8c8c8)
# is a SEPARATE signal ("<5,000, not published"), not part of this palette.
OKABE_ITO <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7",
               "#56B4E9", "#D55E00", "#F0E442", "#000000")
CAT_OTHER  <- "#b0b0b0"   # "Other" bucket / neutral category

# cat_colors(n): n distinct categorical colors from the shared palette. Builders
# must bucket data to <= 8 rows first; for n > 8 this returns the first 7 hues
# plus CAT_OTHER in slot 8 (pair with an "Other" data row) so colors never repeat.
cat_colors <- function(n) {
  if (n <= 8) return(OKABE_ITO[seq_len(n)])
  c(OKABE_ITO[1:7], CAT_OTHER)
}

# --- hbar_over_labels() -------------------------------------------------------
# Long-category horizontal-bar labels that DON'T eat a fixed left pixel margin.
# The category name rides in the whitespace ABOVE each bar (anchored to the
# plot-area edge), so the bar itself spans the FULL container width at any
# viewport — this is the fix for the mobile "bars crushed into a right-hand
# sliver" problem (a hard-coded l=190/210 margin fighting a fixed-width iframe),
# and it needs ZERO responsive JS (positions are paper/category coords, so it is
# identical at 320px and 1280px and survives tab-switch re-renders).
#
# Optional value labels sit just past each bar's end. Direction-aware: on the
# Persian (RTL) edition (is_fa() TRUE, from _helpers_i18n.R) labels anchor RIGHT
# and $xreversed signals the caller to reverse the x-axis (bars grow leftward).
#
# Returns a list to splice into plot_ly() %>% layout():
#   $annotations : category labels (+ value labels when `values` supplied)
#   $margin_l    : left margin to use (8; replaces the old l=190/210)
#   $yaxis       : y-axis config with tick labels turned OFF
#   $xreversed   : TRUE on RTL (pass to xaxis autorange/range)
#
#   cats     : category labels IN PLOT ORDER (bottom-to-top = the factor levels)
#   ends     : optional numeric vector (same order) = each bar's END position in
#              x-DATA units (i.e. the plotted x value), used to PLACE the value
#              label. On a count-scaled axis pass the counts, not the percents.
#   end_text : optional character vector (same order) = the TEXT shown at each
#              bar end (e.g. "21%"). Defaults to `ends` comma-formatted. Pass ""
#              for a bar that should carry no value label (e.g. suppressed).
#   wrap_at  : soft-wrap category labels longer than this many characters
# Use ONLY for long-label CATEGORY bars (occupations, industries). Short-label
# ranked GEOGRAPHIC bars (states, provinces, counties) keep conventional
# left-axis tick labels — do not convert those.
# pct_lab(): bar-end "N%" label, BLANKED when it would round to 0% — a sub-1%
# sliver shows no number rather than a misleading "0%". Vectorized.
pct_lab <- function(p) ifelse(is.na(p) | round(p) < 1, "", paste0(round(p), "%"))

hbar_over_labels <- function(cats, ends = NULL, end_text = NULL,
                             wrap_at = 28, font_size = 11) {
  fa   <- exists("is_fa") && is_fa()
  cats <- as.character(cats)

  soft_wrap <- function(s) {
    if (nchar(s) <= wrap_at) return(s)
    words <- strsplit(s, " ")[[1]]; line <- ""; out <- character(0)
    for (w in words) {
      if (nzchar(line) && nchar(paste(line, w)) > wrap_at) { out <- c(out, line); line <- w }
      else line <- if (nzchar(line)) paste(line, w) else w
    }
    paste(c(out, line), collapse = "<br>")
  }
  wrapped <- vapply(cats, soft_wrap, character(1))

  lab_anns <- lapply(seq_along(cats), function(i) list(
    x = if (fa) 1 else 0, xref = "paper", xanchor = if (fa) "right" else "left",
    y = cats[i], yref = "y", yanchor = "bottom", yshift = 7,
    text = wrapped[i], showarrow = FALSE, align = if (fa) "right" else "left",
    font = list(size = font_size, family = "Montserrat, sans-serif", color = "#333")))

  val_anns <- list()
  if (!is.null(ends)) {
    if (is.null(end_text))
      end_text <- formatC(ends, format = "f", digits = 0, big.mark = ",")
    for (i in seq_along(cats)) {
      if (!nzchar(end_text[i])) next          # blank -> no value label (suppressed)
      val_anns[[length(val_anns) + 1L]] <- list(
        x = ends[i], xref = "x", xanchor = if (fa) "right" else "left",
        xshift = if (fa) -5 else 5, y = cats[i], yref = "y", yanchor = "middle",
        text = end_text[i], showarrow = FALSE,
        font = list(size = 11, family = "Montserrat, sans-serif", color = "#555"))
    }
  }

  list(annotations = c(lab_anns, val_anns), margin_l = 8,
       yaxis = list(title = "", showticklabels = FALSE, ticks = "", fixedrange = TRUE),
       xreversed = fa)
}

# --- share_of -----------------------------------------------------------------
# Weighted percentage of the rows of `df` whose `col` value is in `levels`, out
# of the total `weight`. `weight` is a COLUMN NAME (string). Returns an unrounded
# percentage; round at the call site.
#
# FAILS LOUD if any requested level is absent from df[[col]]. This is the guard
# against the label-mismatch bug class: a text-card helper hand-typing a category
# label that does not match the data — e.g. matching a chart's post-rename
# "BA degree" against a raw CSV whose column says "Bachelors degree", which
# silently drops those rows and understates the share (shipped once, 2026-07).
# Use ONLY where every requested level is expected to be PRESENT in df (a
# complete cross-tab, so absence == a coding bug). Do NOT use it where an absent
# level legitimately means zero — e.g. an employment class that happens to have
# no members in one sex/generation subgroup; there a hard stop would be wrong.
share_of <- function(df, col, levels, weight) {
  miss <- setdiff(levels, unique(df[[col]]))
  if (length(miss)) {
    stop(sprintf("share_of(): level(s) %s absent from column '%s' (present: %s)",
                 paste(sprintf('"%s"', miss), collapse = ", "), col,
                 paste(sprintf('"%s"', sort(unique(df[[col]]))), collapse = ", ")),
         call. = FALSE)
  }
  w <- df[[weight]]
  sum(w[df[[col]] %in% levels]) / sum(w) * 100
}

# --- strip_internal_classes ---------------------------------------------------
# plotly_build() decorates some list elements with S3 classes like "zcolor"
# that trip up jsonlite::toJSON. Walk the structure and drop those classes.
strip_internal_classes <- function(x) {
  if (is.list(x)) {
    if (inherits(x, "zcolor")) class(x) <- "list"
    return(lapply(x, strip_internal_classes))
  }
  if (inherits(x, "zcolor")) class(x) <- NULL
  x
}

# --- plotly_to_json -----------------------------------------------------------
# Build a plotly object and serialize its data / layout / config to JSON
# strings. Also forces Montserrat + a standard hoverlabel style on every chart.
#
# inject_hoveron: if TRUE, adds hoveron="fills+points" to every trace that has
# a fill (used by the stacked-area chart on the Global page so hover fires
# anywhere in the band, not just on the line).
plotly_to_json <- function(p, inject_hoveron = FALSE) {
  b <- plotly_build(p)
  b$x$data <- strip_internal_classes(b$x$data)
  b$x$layout <- strip_internal_classes(b$x$layout)
  if (is.null(b$x$layout$font)) b$x$layout$font <- list()
  b$x$layout$font$family <- "Montserrat, sans-serif"
  b$x$layout$hoverlabel <- list(
    bgcolor = "white", bordercolor = "#ccc",
    font = list(family = "Montserrat, sans-serif", size = 13, color = "#333"))
  if (inject_hoveron) {
    for (i in seq_along(b$x$data)) {
      if (!is.null(b$x$data[[i]]$fill)) {
        b$x$data[[i]]$hoveron <- "fills+points"
      }
    }
  }
  list(data = toJSON(b$x$data, auto_unbox = TRUE),
       layout = toJSON(b$x$layout, auto_unbox = TRUE),
       config = toJSON(b$x$config, auto_unbox = TRUE))
}

# --- plotly_div ---------------------------------------------------------------
# Assemble the HTML block for one chart: a <div> target, the Plotly.newPlot()
# <script>, and (optionally) a legend and a source footer appended below.
#
# id             plot container id, used by Plotly.newPlot() and by hover JS
# json           list(data, layout, config) returned by plotly_to_json()
# height         CSS height for the container (e.g. "500px")
# source         HTML (or plain text) for the "Source: ..." footer, or NULL
# legend_html    HTML for an external legend to append below the chart, or NULL
# highlight_hover  if TRUE, injects the legendgroup hover-highlight JS used by
#                  charts with an interactive external legend (US/DE/UK/CA). The
#                  Global page's stacked-area chart uses a *different* custom
#                  highlight block defined inline in build_global.R, so it calls
#                  plotly_div() with highlight_hover = FALSE.
plotly_div <- function(id, json, height = "500px",
                       source = NULL, legend_html = NULL,
                       highlight_hover = FALSE) {
  init_js <- sprintf(
    'var c=Object.assign(%s,{responsive:true,scrollZoom:"geo+mapbox",showTips:true});var l=%s;Plotly.newPlot("%s",%s,l,c);',
    json$config, json$layout, id, json$data)
  if (highlight_hover) {
    init_js <- paste0(init_js, sprintf('
var el=document.getElementById("%s");
function hlOn(lg){
  el.data.forEach(function(t,i){
    Plotly.restyle(el,{opacity:t.legendgroup===lg?1:0.15,"marker.line.width":t.legendgroup===lg?3:0,"marker.line.color":"#000"},[i]);
  });
  var wrap=el.closest(".chart-card");
  if(wrap){wrap.querySelectorAll("[data-lg]").forEach(function(s){s.style.opacity=s.getAttribute("data-lg")===lg?1:0.3;});}
}
function hlOff(){
  el.data.forEach(function(t,i){Plotly.restyle(el,{opacity:1,"marker.line.width":0},[i]);});
  var wrap=el.closest(".chart-card");
  if(wrap){wrap.querySelectorAll("[data-lg]").forEach(function(s){s.style.opacity=1;});}
}
el.__hlOn=hlOn;el.__hlOff=hlOff;
el.on("plotly_hover",function(d){hlOn(d.points[0].data.legendgroup);});
el.on("plotly_unhover",hlOff);
var _lastLg=null;
el.on("plotly_click",function(d){var lg=d.points[0].data.legendgroup;if(_lastLg===lg){hlOff();_lastLg=null;}else{hlOn(lg);_lastLg=lg;}});', id))
  }
  chart <- sprintf(
    '<div id="%s" style="width:100%%;height:%s;touch-action:manipulation;"></div>\n<script>(function(){%s})();</script>',
    id, height, init_js)
  if (!is.null(legend_html)) {
    chart <- paste0(chart, '\n', legend_html)
  }
  if (!is.null(source)) {
    chart <- paste0(chart, sprintf(
      '\n<p style="font-size:11px; color:#666; text-align:right; margin:4px 0 0 0; padding-right:2px;">%s</p>',
      source))
  }
  chart
}

# --- criteria_table -----------------------------------------------------------
# The "How We Count" criteria grid, rendered as an HTML table placed directly
# BELOW the waterfall plotly chart (not as a plotly subplot). This gives true
# full-row zebra striping that includes the row labels (which plotly cannot do —
# its axis tick-labels live in the margin and can't be shaded), line-broken
# labels, and ✓/✗ cells.
#
# Column alignment with the bars: build the waterfall with xaxis
# `range = c(0.5, n_cols + 0.5)`, left margin = `label_px`, right margin = 20.
# Then the bars sit at fractions (i-0.5)/n of the plot area, exactly matching
# this table's n equal data columns (label column = label_px, wrapper
# padding-right = 20). Holds at any width, so it stays aligned on mobile.
#
# rows: list of list(label = "<html, may use <br>>", vals = <logical, length n_cols>)
criteria_table <- function(rows, n_cols, label_px = 84, sym_px = 16) {
  body <- vapply(seq_along(rows), function(i) {
    r <- rows[[i]]
    bg <- if (i %% 2 == 1) " background:#f1f3f5;" else ""
    cells <- paste0(vapply(r$vals, function(v) sprintf(
      '<td style="text-align:center; padding:6px 0; font-size:%dpx; color:%s;">%s</td>',
      sym_px, if (isTRUE(v)) "#2f2f2f" else "#c4c4c4", if (isTRUE(v)) "✓" else "✗"),
      character(1)), collapse = "")
    sprintf(paste0('<tr style="%s"><td style="text-align:right; padding:6px 9px 6px 4px;',
      ' font-size:11px; color:#333; line-height:1.2;">%s</td>%s</tr>'),
      bg, r$label, cells)
  }, character(1))
  sprintf(paste0('<div style="padding-right:20px;"><table style="width:100%%;',
    ' table-layout:fixed; border-collapse:collapse;">',
    '<colgroup><col style="width:%dpx;">%s</colgroup><tbody>%s</tbody></table></div>'),
    label_px, paste(rep("<col>", n_cols), collapse = ""), paste(body, collapse = ""))
}

# --- iframe_resize_script -----------------------------------------------------
# Every page is embedded via <iframe> in docs/index.html and posts its body
# height back to the parent so the iframe auto-sizes. Also re-invokes
# Plotly.Plots.resize() on window resize and when chart containers change size.
iframe_resize_script <- '
<script>
function reportHeight() {
  if (window.parent !== window) {
    window.parent.postMessage({ type: "iframeHeight", height: document.body.scrollHeight + 20 }, "*");
  }
}
function resizeAllPlots() {
  document.querySelectorAll(".js-plotly-plot").forEach(function(p) {
    if (window.Plotly) Plotly.Plots.resize(p);
  });
  reportHeight();
}
window.addEventListener("load", function(){ setTimeout(resizeAllPlots, 300); });
window.addEventListener("resize", function(){ setTimeout(resizeAllPlots, 150); });
if (window.ResizeObserver) {
  var ro = new ResizeObserver(function(entries) {
    entries.forEach(function(e) {
      var plot = e.target.querySelector(".js-plotly-plot") || (e.target.classList.contains("js-plotly-plot") ? e.target : null);
      if (plot && window.Plotly) Plotly.Plots.resize(plot);
    });
    reportHeight();
  });
  window.addEventListener("load", function() {
    setTimeout(function() {
      document.querySelectorAll(".js-plotly-plot").forEach(function(p) {
        ro.observe(p.parentElement || p);
      });
    }, 500);
  });
}
new MutationObserver(reportHeight).observe(document.body, { childList: true, subtree: true });
</script>'

# --- MAPBOX_ATTRIB_HIDE_CSS ---------------------------------------------------
# Hides the small "(i)" attribution control Mapbox/Plotly renders in the
# corner of every choroplethmapbox / scattermapbox / densitymapbox layer.
# These selectors only match DOM emitted by Mapbox GL-backed trace types;
# on pages whose maps use SVG-backed plot_ly(type="choropleth") (currently
# the Global and Europe-overview pages) the rule is inert. Inject it into
# every page's <style> block anyway so the base CSS block stays uniform
# across all 18 build scripts; the rule takes effect automatically if a
# page later switches to a Mapbox-backed trace. Carto's basemap ToS
# technically require visible attribution; the site's Bibliography and
# About pages carry attribution explicitly, so the in-map control is
# redundant and visually noisy on small maps.
MAPBOX_ATTRIB_HIDE_CSS <- ".mapboxgl-ctrl-attrib, .mapboxgl-ctrl-logo { display:none !important; }"

# --- map_overlay_legend -------------------------------------------------------
# Build a small absolutely-positioned HTML legend to overlay the top-right
# corner of a map. Used on the Canada and Australia choroplethmapbox pages in
# place of Plotly's built-in colorbar (which renders a continuous gradient
# that reads less cleanly than discrete categorical bins and can't easily be
# flipped to low-at-top, high-at-bottom).
#
# Usage inside a page body template (wrap the map's plotly_div output so the
# legend positions relative to the map container):
#
#   paste0(
#     '<div style="position:relative;">',
#     plotly_div("ca-prov-map", plotly_to_json(p), "380px"),
#     map_overlay_legend(c(
#       "#c6dbef" = "1 – 1,000",
#       "#6baed6" = "1,000 – 10,000",
#       "#2171b5" = "10,000 – 50,000",
#       "#08306b" = "50,000 – 150,000")),
#     '</div>')
#
# The items vector is a NAMED character vector: names are CSS colors, values
# are the labels. Order matters — top-to-bottom in the legend follows the
# argument order. Convention on this site is low-to-high top-to-bottom, with
# an optional "No data" row at the bottom.
map_overlay_legend <- function(items) {
  rows <- vapply(seq_along(items), function(i) {
    sprintf('<div style="display:flex;align-items:center;gap:6px;margin:2px 0;"><span style="display:inline-block;width:12px;height:12px;background:%s;border-radius:2px;flex-shrink:0;"></span>%s</div>',
      names(items)[i], items[[i]])
  }, character(1))
  paste0(
    '<div style="position:absolute;top:10px;right:10px;background:rgba(255,255,255,0.92);padding:6px 10px;border-radius:4px;box-shadow:0 1px 3px rgba(0,0,0,0.15);font-size:11px;line-height:1.4;pointer-events:none;z-index:500;">',
    paste(rows, collapse = ""),
    '</div>')
}

# --- chk ---------------------------------------------------------------------
# Small ✓/✗ formatter for identification / criteria tables.
chk <- function(b) ifelse(b, "✓", "✗")

# --- make_gen_box ------------------------------------------------------------
# One of the two side-by-side colored generation boxes (1st/2nd gen, or
# male/female) shown on population pages. Takes a pre-formatted pct_text.
# Australia keeps a local variant with different sizing; it shadows this.
make_gen_box <- function(val, pct_text, label, sublabel, color) {
  sprintf(
    '<div style="background:%s; border-radius:6px; padding:22px 14px; text-align:center; color:white; flex:1; min-width:0;">
      <div style="font-size:30px; font-weight:700; line-height:1.1;">%s</div>
      <div style="font-size:13px; margin-top:4px; font-weight:600;">%s</div>
      <div style="font-size:12px; opacity:0.9; margin-top:2px;">%s</div>
      <div style="font-size:11px; opacity:0.85; margin-top:3px;">%s</div>
    </div>',
    color, format(val, big.mark = ","), label, pct_text, sublabel)
}

# --- make_html_legend --------------------------------------------------------
# Static external legend: centered swatch+label row(s). break_after wraps
# into two rows. Use with plotly_div(highlight_hover = FALSE).
make_html_legend <- function(colors, labels = names(colors), break_after = NULL) {
  items <- mapply(function(col, lab) {
    sprintf('<span style="display:inline-flex;align-items:center;gap:4px;margin:0 8px;"><span style="display:inline-block;width:14px;height:14px;background:%s;border-radius:2px;flex-shrink:0;"></span><span style="font-size:12px;color:#333;">%s</span></span>', col, lab)
  }, colors, labels, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if (!is.null(break_after) && break_after < length(items)) {
    row1 <- paste(items[1:break_after], collapse = "")
    row2 <- paste(items[(break_after + 1):length(items)], collapse = "")
    inner <- paste0('<div style="display:flex;flex-wrap:wrap;justify-content:center;gap:2px;">', row1, '</div>',
                    '<div style="display:flex;flex-wrap:wrap;justify-content:center;gap:2px;">', row2, '</div>')
  } else {
    inner <- paste(items, collapse = "")
  }
  sprintf('<div style="text-align:center;margin:6px 0 2px;">%s</div>', inner)
}

# --- make_html_legend_hover --------------------------------------------------
# Interactive external legend: emits data-lg spans whose hover handlers call
# el.__hlOn/__hlOff (installed by plotly_div(highlight_hover = TRUE)) to dim
# the other traces. break_after is accepted but unused. US keeps a local
# variant (line-break layout) that shadows nothing here.
make_html_legend_hover <- function(colors, labels = names(colors), break_after = NULL) {
  items <- mapply(function(col, lab) {
    html_lab <- gsub("&", "&amp;", lab)
    sprintf('<span data-lg="%s" style="display:inline-flex; align-items:center; gap:4px; cursor:pointer; transition:opacity 0.2s;" onmouseenter="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOn)el.__hlOn(this.getAttribute(\'data-lg\'));" onmouseleave="var el=this.closest(\'.chart-card\').querySelector(\'.js-plotly-plot\');if(el&&el.__hlOff)el.__hlOff();"><span style="width:12px; height:12px; background:%s; border-radius:2px; display:inline-block;"></span> %s</span>',
      lab, col, html_lab)
  }, colors, labels, SIMPLIFY = TRUE)
  sprintf('<div style="display:flex; justify-content:center; flex-wrap:wrap; gap:6px 14px; font-size:12px; color:#444; margin:6px 0 2px; line-height:2;">%s</div>',
    paste(items, collapse = ""))
}

# --- page_template (standard cluster) ----------------------------------------
# Shared page shell for the standard single-column-friendly country pages
# (8-builder cluster: AT/DK/IL/NL/NO/SE/CH/TR). Emits the base CSS grid,
# optional tab CSS, and the body. Free vars resolved from the builder's
# global env at call time: tab_switch_script (builder-local, e.g. Turkey's
# guarded variant) plus shared MAPBOX_ATTRIB_HIDE_CSS / iframe_resize_script.
# Builders with divergent shells (US/CA/DE/AU/UK/EU/FR/IT/AM/FI) keep a local
# page_template that shadows this one; unifying those is Phase B.
page_template <- function(title, body_html, has_tabs = FALSE) {
  tab_css <- if (has_tabs) '
.tab-bar { display:flex; justify-content:center; gap:0; margin:12px 0 0; }
.tab-btn { padding:6px 16px; border:1px solid #ddd; background:#f0f0f0; cursor:pointer;
  font-family:"Montserrat",sans-serif; font-size:13px; color:#333; border-radius:4px; margin:0 2px; transition:background 0.15s; white-space:nowrap; }
.tab-btn.active { background:#2774AE; color:white; font-weight:600; border-color:#2774AE; }
.tab-btn:hover:not(.active) { background:#e0e0e0; }
.tab-panel { display:none; }
.tab-panel.active { display:block; }' else ''

  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>', title, '</title>
<link href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap" rel="stylesheet">
', plotly_script(body_html), '
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family:"Montserrat",sans-serif; background:#fafafa; color:#333; padding:15px 40px; max-width:100%; overflow-x:hidden; }
.chart-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; align-items:stretch; }
.text-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.text-card { background:white; border-radius:8px; padding:20px; text-align:center;
  font-size:15px; line-height:1.6; border:1px solid #e0e0e0; }
.chart-card { background:white; border-radius:8px; padding:16px; border:1px solid #e0e0e0; margin-bottom:20px; overflow:hidden; min-width:0; }
.section-title { font-size:16px; font-weight:600; text-align:center; margin:16px 0 8px; }
.headline { background:white; border-radius:8px; padding:30px; text-align:center; border:1px solid #e0e0e0; margin-bottom:20px; }
.headline .number { font-size:44px; font-weight:700; color:#1a4e72; line-height:1.1; letter-spacing:-0.02em; }
a { transition: color 0.15s; }
a:hover { color: #1a4e72 !important; text-decoration: underline; }
.headline .label { font-size:14px; color:#666; margin-top:4px; }
.page-content { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:20px; }
.page-content .chart-card { margin-bottom:0; }
.pt1 { grid-area:1/1; } .pt2 { grid-area:1/2; }
.pc1 { grid-area:2/1; } .pc2 { grid-area:2/2; }', tab_css, '
@media (max-width:900px) {
  body { padding:10px 15px; }
  .chart-row, .text-row { grid-template-columns:1fr !important; }
  .page-content { grid-template-columns:1fr; }
  .pt1,.pt2,.pc1,.pc2 { grid-area:auto; }
  .pc1 { order:1; } .pt1 { order:2; } .pc2 { order:3; } .pt2 { order:4; }
  .headline { padding:20px 15px; }
  .section-title { font-size:14px; }
  .tab-bar { flex-wrap:wrap; gap:4px; }
  .tab-btn { font-size:12px; padding:5px 10px; }
}
@media (max-width:480px) {
  body { padding:8px 10px; }
  .chart-card { padding:10px; }
  .text-card { font-size:13px; padding:14px; }
}
', MAPBOX_ATTRIB_HIDE_CSS, '
</style>
</head>
<body>
', body_html, '
', if (has_tabs) tab_switch_script else '', '
', iframe_resize_script, '
</body>
</html>')
}

# --- plotly_script -----------------------------------------------------------
# Choose the Plotly bundle for a page. The full 4.85 MB build is only needed for
# map traces (choropleth / choroplethmapbox) or a maplibre/pmtiles map; every
# other page uses only bar + scatter, which the ~1.1 MB plotly-basic build
# renders with the SAME v3.4.0 engine (pixel-identical). Auto-detects from the
# page body so a page that later gains a map upgrades itself — no per-call flag.
plotly_script <- function(body_html) {
  needs_full <- grepl('"type":"choropleth', body_html, fixed = TRUE) ||
                grepl('maplibre', body_html, fixed = TRUE) ||
                grepl('pmtiles', body_html, fixed = TRUE)
  src <- if (needs_full) 'lib/plotly-3.4.0.min.js' else 'lib/plotly-basic-3.4.0.min.js'
  sprintf('<script src="%s"></script>', src)
}

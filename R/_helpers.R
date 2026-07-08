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

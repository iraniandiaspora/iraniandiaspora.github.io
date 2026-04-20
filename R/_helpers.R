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

#!/usr/bin/env python3
"""Inject SEO/social head metadata + a standalone-view nav link into built pages.

Run AFTER any R/build_*.R rebuild, from the deployment repo root:

    python3 tools/inject_head_meta.py

Idempotent: each page's injected content lives between marker comments and is
replaced wholesale on re-run. The builders do not emit this metadata themselves
(their page_template shells never learn the output filename); this post-build
pass derives everything from the built file: filename -> canonical URL and
hreflang twin, <html lang> -> language, <title> -> og:title / description.

The smoke gate (pipelines/smoke_check_pages.sh) fails any page missing the
injected block, so a rebuild without this step cannot pass verification.
"""

import glob
import os
import re
import sys

BASE = "https://iraniandiaspora.github.io"

META_BEGIN = "<!-- injected-meta:begin (tools/inject_head_meta.py) -->"
META_END = "<!-- injected-meta:end -->"
NAV_BEGIN = "<!-- standalone-nav:begin (tools/inject_head_meta.py) -->"
NAV_END = "<!-- standalone-nav:end -->"

DESC_EN = ("%s — Iranian Diaspora Dashboard. Interactive data on Iranian "
           "immigrant and diaspora populations worldwide, from official census "
           "and register sources.")
DESC_FA = ("%s — داشبورد دیاسپورای ایرانی. "
           "داده‌های تعاملی دربارهٔ جمعیت ایرانیان مهاجر و "
           "دیاسپورا در سراسر جهان، بر پایهٔ منابع رسمی "
           "سرشماری و ثبت جمعیت.")
SITE_NAME_EN = "Iranian Diaspora Dashboard"
NAV_TEXT_EN = "← Back to the Iranian Diaspora Dashboard"
NAV_TEXT_FA = "بازگشت به داشبورد دیاسپورای ایرانی"


def esc(s):
    return s.replace("&", "&amp;").replace('"', "&quot;")


def build_meta(name, title, is_fa, has_twin, stem):
    self_url = f"{BASE}/pages/{name}"
    en_url = f"{BASE}/pages/{stem}.html"
    fa_url = f"{BASE}/pages/{stem}.fa.html"
    desc = (DESC_FA if is_fa else DESC_EN) % title
    lines = [
        META_BEGIN,
        f'<link rel="canonical" href="{self_url}">',
    ]
    if has_twin:
        lines += [
            f'<link rel="alternate" hreflang="en" href="{en_url}">',
            f'<link rel="alternate" hreflang="fa" href="{fa_url}">',
            f'<link rel="alternate" hreflang="x-default" href="{en_url}">',
        ]
    lines += [
        f'<meta name="description" content="{esc(desc)}">',
        f'<meta property="og:site_name" content="{SITE_NAME_EN}">',
        f'<meta property="og:title" content="{esc(title)}">',
        f'<meta property="og:description" content="{esc(desc)}">',
        '<meta property="og:type" content="website">',
        f'<meta property="og:url" content="{self_url}">',
        f'<meta property="og:image" content="{BASE}/og-image.png">',
        f'<meta property="og:locale" content="{"fa_IR" if is_fa else "en_US"}">',
    ]
    if has_twin:
        lines.append(
            f'<meta property="og:locale:alternate" content="{"en_US" if is_fa else "fa_IR"}">')
    lines += [
        '<meta name="twitter:card" content="summary_large_image">',
        '<link rel="icon" href="../favicon.ico">',
        '<link rel="apple-touch-icon" href="../apple-touch-icon.png">',
        META_END,
    ]
    return "\n".join(lines)


def build_nav(is_fa, stem):
    # Pages are normally viewed inside the dashboard's iframes; this link is
    # for visitors landing on the bare fragment URL from a search engine.
    # Shown only when the page is the top window (not iframed).
    text = NAV_TEXT_FA if is_fa else NAV_TEXT_EN
    return "\n".join([
        NAV_BEGIN,
        f'<div id="standalone-nav" style="display:none; text-align:center; margin:18px 0 6px;">'
        f'<a href="../#{stem}" style="color:#2774AE; font-size:14px; text-decoration:none; '
        f'font-weight:600;">{text}</a></div>',
        '<script>if(window.self===window.top){'
        'document.getElementById("standalone-nav").style.display="block";}</script>',
        NAV_END,
    ])


def strip_block(html, begin, end):
    pat = re.compile(re.escape(begin) + r".*?" + re.escape(end) + r"\n?", re.S)
    return pat.sub("", html)


def process(path):
    name = os.path.basename(path)
    is_fa = name.endswith(".fa.html")
    stem = name[:-8] if is_fa else name[:-5]
    twin = os.path.join(os.path.dirname(path),
                        f"{stem}.html" if is_fa else f"{stem}.fa.html")
    has_twin = os.path.exists(twin)

    with open(path, encoding="utf-8") as f:
        html = f.read()

    m = re.search(r"<title>(.*?)</title>", html, re.S)
    if not m:
        print(f"  SKIP (no <title>): {name}")
        return False
    title = m.group(1).strip()

    html = strip_block(html, META_BEGIN, META_END)
    html = strip_block(html, NAV_BEGIN, NAV_END)

    meta = build_meta(name, title, is_fa, has_twin, stem)
    html = html.replace("</title>", "</title>\n" + meta, 1)

    nav = build_nav(is_fa, stem)
    if "</body>" in html:
        html = html.replace("</body>", nav + "\n</body>", 1)
    else:
        html += "\n" + nav + "\n"

    with open(path, "w", encoding="utf-8") as f:
        f.write(html)
    return True


def main():
    repo = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    pages = sorted(glob.glob(os.path.join(repo, "docs", "pages", "*.html")))
    if not pages:
        sys.exit("No pages found under docs/pages/ — run from the repo root.")
    done = sum(process(p) for p in pages)
    print(f"Injected head metadata + standalone nav into {done}/{len(pages)} pages.")


if __name__ == "__main__":
    main()

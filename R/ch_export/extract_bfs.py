#!/usr/bin/env python3
"""Extract Iran-born population data from BFS (Swiss Federal Statistical Office) PX-Web API.

Run from deployment repo root:
    python3 R/ch_export/extract_bfs.py

Output: data/switzerland/
    ch_headline.csv   — total, male, female, swiss_citizen, foreign
    ch_canton.csv     — canton_name, canton_code, iran_born
    ch_trend.csv      — year, total (2010-2024)
    ch_age_sex.csv    — age, sex, count
    ch_arrivals.csv   — year, count (2011-2024 immigration flow)

Data source: BFS PX-Web API (no authentication needed)
    Base URL: https://www.pxweb.bfs.admin.ch/api/v1/en/

Iran codes: 8513 (country of birth), 8100 (Switzerland citizenship).
"""

import json
import csv
import os
import urllib.request

BASE = "https://www.pxweb.bfs.admin.ch/api/v1/en"
OUT_DIR = "data/switzerland"

# Canton code-to-abbreviation mapping (BFS PX-Web uses these codes)
CANTON_CODES = {
    "ZH": "ZH", "BE": "BE", "LU": "LU", "UR": "UR", "SZ": "SZ",
    "OW": "OW", "NW": "NW", "GL": "GL", "ZG": "ZG", "FR": "FR",
    "SO": "SO", "BS": "BS", "BL": "BL", "SH": "SH", "AR": "AR",
    "AI": "AI", "SG": "SG", "GR": "GR", "AG": "AG", "TG": "TG",
    "TI": "TI", "VD": "VD", "VS": "VS", "NE": "NE", "GE": "GE",
    "JU": "JU"
}

# BFS full names → short names (for matching with GeoJSON)
BFS_SHORT_NAME = {
    "Zürich": "Zürich",
    "Bern / Berne": "Bern",
    "Luzern": "Luzern",
    "Uri": "Uri",
    "Schwyz": "Schwyz",
    "Obwalden": "Obwalden",
    "Nidwalden": "Nidwalden",
    "Glarus": "Glarus",
    "Zug": "Zug",
    "Fribourg / Freiburg": "Fribourg",
    "Solothurn": "Solothurn",
    "Basel-Stadt": "Basel-Stadt",
    "Basel-Landschaft": "Basel-Landschaft",
    "Schaffhausen": "Schaffhausen",
    "Appenzell Ausserrhoden": "Appenzell Ausserrhoden",
    "Appenzell Innerrhoden": "Appenzell Innerrhoden",
    "St. Gallen": "St. Gallen",
    "Graubünden / Grigioni / Grischun": "Graubünden",
    "Aargau": "Aargau",
    "Thurgau": "Thurgau",
    "Ticino": "Ticino",
    "Vaud": "Vaud",
    "Valais / Wallis": "Valais",
    "Neuchâtel": "Neuchâtel",
    "Genève": "Genève",
    "Jura": "Jura"
}


def post_json(table_id, query):
    """POST a PX-Web query and return parsed JSON-stat2 response."""
    url = f"{BASE}/{table_id}/{table_id}.px"
    body = json.dumps(query).encode("utf-8")
    req = urllib.request.Request(url, data=body,
                                headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=60) as resp:
        return json.load(resp)


def dim_labels(d, dim_name):
    """Return {index: label} dict for a dimension."""
    cats = d["dimension"][dim_name]["category"]
    return {cats["index"][k]: cats["label"][k] for k in cats["index"]}


def write_csv(filename, rows, fieldnames):
    path = os.path.join(OUT_DIR, filename)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(rows)
    print(f"  {filename}: {len(rows)} rows")


def main():
    os.makedirs(OUT_DIR, exist_ok=True)

    # =========================================================================
    # 1. Headline: total, male, female, Swiss citizen, Iranian citizen (2024)
    # =========================================================================
    print("Pulling headline data...")
    d = post_json("px-x-0103010000_499", {
        "query": [
            {"code": "Jahr", "selection": {"filter": "item", "values": ["2024"]}},
            {"code": "Kanton", "selection": {"filter": "item", "values": ["8100"]}},
            {"code": "Bevölkerungstyp", "selection": {"filter": "item", "values": ["1"]}},
            {"code": "Geburtsstaat", "selection": {"filter": "item", "values": ["8513"]}},
            {"code": "Staatsangehörigkeit", "selection": {"filter": "item", "values": ["-99999", "8100", "8513"]}},
            {"code": "Geschlecht", "selection": {"filter": "item", "values": ["-99999", "1", "2"]}}
        ],
        "response": {"format": "json-stat2"}
    })
    # Dimensions: Citizenship(3) × Sex(3) → 9 values
    # [total/total, total/male, total/female,
    #  swiss/total, swiss/male, swiss/female,
    #  iranian/total, iranian/male, iranian/female]
    v = d["value"]
    total, male, female = v[0], v[1], v[2]
    swiss_citizen = v[3]
    iranian_citizen = v[6]
    foreign = total - swiss_citizen  # includes Iranian + other foreign

    rows = [
        {"category": "total", "count": total, "year": 2024},
        {"category": "male", "count": male, "year": 2024},
        {"category": "female", "count": female, "year": 2024},
        {"category": "swiss_citizen", "count": swiss_citizen, "year": 2024},
        {"category": "iranian_citizen", "count": iranian_citizen, "year": 2024},
        {"category": "foreign", "count": foreign, "year": 2024},
    ]
    write_csv("ch_headline.csv", rows, ["category", "count", "year"])
    print(f"  Total: {total:,} (M: {male:,}, F: {female:,})")
    print(f"  Swiss citizens: {swiss_citizen:,}, Iranian citizens: {iranian_citizen:,}, Other foreign: {total - swiss_citizen - iranian_citizen:,}")

    # =========================================================================
    # 2. Canton breakdown (2024)
    # =========================================================================
    print("Pulling canton data...")
    d = post_json("px-x-0103010000_499", {
        "query": [
            {"code": "Jahr", "selection": {"filter": "item", "values": ["2024"]}},
            {"code": "Kanton", "selection": {"filter": "all", "values": ["*"]}},
            {"code": "Bevölkerungstyp", "selection": {"filter": "item", "values": ["1"]}},
            {"code": "Geburtsstaat", "selection": {"filter": "item", "values": ["8513"]}},
            {"code": "Staatsangehörigkeit", "selection": {"filter": "item", "values": ["-99999"]}},
            {"code": "Geschlecht", "selection": {"filter": "item", "values": ["-99999"]}}
        ],
        "response": {"format": "json-stat2"}
    })
    canton_labels = dim_labels(d, "Kanton")
    vals = d["value"]

    rows = []
    for idx in sorted(canton_labels.keys()):
        name = canton_labels[idx]
        if name in ("Switzerland", "No indication"):
            continue
        short = BFS_SHORT_NAME.get(name, name)
        rows.append({
            "canton_name": short,
            "canton_code": name.split(" / ")[0][:2].upper() if " / " in name else "",
            "iran_born": vals[idx]
        })
    # Fill in canton codes from the API dimension values
    canton_dim = d["dimension"]["Kanton"]["category"]
    code_by_idx = {}
    for code, idx in canton_dim["index"].items():
        code_by_idx[idx] = code
    for row_idx, idx in enumerate(sorted(canton_labels.keys())):
        if canton_labels[idx] in ("Switzerland", "No indication"):
            continue
    # Rebuild with proper codes
    rows2 = []
    for idx in sorted(canton_labels.keys()):
        name = canton_labels[idx]
        if name in ("Switzerland", "No indication"):
            continue
        short = BFS_SHORT_NAME.get(name, name)
        code = code_by_idx[idx]
        rows2.append({
            "canton_name": short,
            "canton_code": code,
            "iran_born": vals[idx]
        })
    write_csv("ch_canton.csv", rows2, ["canton_name", "canton_code", "iran_born"])

    # =========================================================================
    # 3. Time series 2010-2024 (national, total sex)
    # =========================================================================
    print("Pulling trend data...")
    d = post_json("px-x-0103010000_499", {
        "query": [
            {"code": "Jahr", "selection": {"filter": "all", "values": ["*"]}},
            {"code": "Kanton", "selection": {"filter": "item", "values": ["8100"]}},
            {"code": "Bevölkerungstyp", "selection": {"filter": "item", "values": ["1"]}},
            {"code": "Geburtsstaat", "selection": {"filter": "item", "values": ["8513"]}},
            {"code": "Staatsangehörigkeit", "selection": {"filter": "item", "values": ["-99999"]}},
            {"code": "Geschlecht", "selection": {"filter": "item", "values": ["-99999"]}}
        ],
        "response": {"format": "json-stat2"}
    })
    year_labels = dim_labels(d, "Jahr")
    vals = d["value"]
    rows = []
    for idx in sorted(year_labels.keys()):
        rows.append({"year": int(year_labels[idx]), "total": vals[idx]})
    write_csv("ch_trend.csv", rows, ["year", "total"])

    # =========================================================================
    # 4. Age × sex pyramid (2024, national, single-year ages)
    # =========================================================================
    print("Pulling age-sex pyramid...")
    d = post_json("px-x-0103010000_324", {
        "query": [
            {"code": "Jahr", "selection": {"filter": "item", "values": ["2024"]}},
            {"code": "Kanton", "selection": {"filter": "item", "values": ["8100"]}},
            {"code": "Bevölkerungstyp", "selection": {"filter": "item", "values": ["1"]}},
            {"code": "Staatsangehörigkeit (Auswahl)", "selection": {"filter": "item", "values": ["-99999"]}},
            {"code": "Geburtsstaat", "selection": {"filter": "item", "values": ["8513"]}},
            {"code": "Geschlecht", "selection": {"filter": "item", "values": ["1", "2"]}},
            {"code": "Alter", "selection": {"filter": "all", "values": ["*"]}}
        ],
        "response": {"format": "json-stat2"}
    })
    age_labels = dim_labels(d, "Alter")
    n_ages = len(age_labels)
    vals = d["value"]

    rows = []
    for idx in sorted(age_labels.keys()):
        label = age_labels[idx]
        if label == "Age - total":
            continue
        # Parse age from label
        if "100" in label:
            age = 100
        else:
            age = int(label.split()[0])
        male_val = vals[idx]           # first n_ages values are male
        female_val = vals[n_ages + idx]  # next n_ages values are female
        if male_val > 0:
            rows.append({"age": age, "sex": "Male", "count": male_val})
        if female_val > 0:
            rows.append({"age": age, "sex": "Female", "count": female_val})
    write_csv("ch_age_sex.csv", rows, ["age", "sex", "count"])

    # =========================================================================
    # 5. Immigration flow 2011-2024 (annual arrivals, national)
    # =========================================================================
    print("Pulling immigration arrivals...")
    d = post_json("px-x-0103020200_103", {
        "query": [
            {"code": "Jahr", "selection": {"filter": "all", "values": ["*"]}},
            {"code": "Kanton", "selection": {"filter": "item", "values": ["0"]}},
            {"code": "Staatsangehörigkeit (Kategorie)", "selection": {"filter": "item", "values": ["0"]}},
            {"code": "Geburtsstaat", "selection": {"filter": "item", "values": ["8513"]}},
            {"code": "Geschlecht", "selection": {"filter": "item", "values": ["0"]}},
            {"code": "Altersklasse", "selection": {"filter": "item", "values": ["0"]}}
        ],
        "response": {"format": "json-stat2"}
    })
    year_labels = dim_labels(d, "Jahr")
    vals = d["value"]
    rows = []
    for idx in sorted(year_labels.keys()):
        rows.append({"year": int(year_labels[idx]), "count": vals[idx]})
    write_csv("ch_arrivals.csv", rows, ["year", "count"])

    print("\nDone. All CSVs written to data/switzerland/")


if __name__ == "__main__":
    main()

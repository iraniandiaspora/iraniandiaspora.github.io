<!--
  PUBLIC DOCUMENTATION. Intended audience: readers of the Iranian Diaspora
  Dashboard at https://iraniandiaspora.github.io. This file is committed to
  the deployment repo and linked from the About page.

  Do NOT include Claude-session state, TODOs, or internal gotchas here.
  Session state lives in ../.claude/memory/HISTORY.md. Internal methodology
  notes for Claude sessions live in ../.claude/rules/<topic>-pipeline.md.
-->

# Methodology

## United States

### Population Identification

Iranian-Americans are identified in the American Community Survey (ACS) using four criteria. A person is counted if they meet at least one:

1. **Place of birth** — reports being born in Iran
2. **Ancestry** — reports "Iranian" as first or second ancestry
3. **Race** — writes in "Iranian" under the White race category (available in 2024 only)
4. **Parental origin** — lives with a parent who meets any of the above criteria

The headline estimate (794,915) uses the 2024 ACS 1-Year survey, which includes all four criteria. All other charts use the 2020–2024 ACS 5-Year pooled data (754,595), which excludes the race write-in category (not available in 5-year files).

Persian language speakers are not counted as a standalone criterion because the Census language code also captures Afghan Dari speakers.

**Data access:** ACS Public Use Microdata Samples are available from the [U.S. Census Bureau](https://www.census.gov/programs-surveys/acs/microdata.html) and [IPUMS USA](https://usa.ipums.org/).

### Admissions History

The Admissions History page draws on official government records of all Iranians granted lawful permanent resident status in the United States from fiscal year 1978 through 2023 — a total of 562,864 grants across 46 years. These are administrative counts, not survey estimates.

**Sources:**

- **1978–2004:** U.S. Immigration and Naturalization Service, *Statistical Yearbook of the Immigration and Naturalization Service* (Washington, D.C.: U.S. Government Printing Office). Data extracted from Table 4 or Table 8 (immigrants by country of birth and class of admission) in each annual yearbook.
- **2005–2023:** U.S. Department of Homeland Security, Office of Immigration Statistics, *Yearbook of Immigration Statistics* (Washington, D.C.). Data extracted from expanded Table 10 spreadsheets available at [ohss.dhs.gov](https://ohss.dhs.gov/topics/immigration/yearbook).

**Categories.** Admissions are grouped into five categories: family (immediate relatives of U.S. citizens plus family-sponsored preferences), employment (employment-based preferences), refugee/asylee (refugee and asylee adjustments to permanent residence), diversity (visa lottery program), and other (special immigrants, cancellation of removal, and IRCA legalization). Prior to 1992, immigration law did not separate family and employment preferences, so the family category includes both for fiscal years 1978–1991.

**Admissions vs. arrivals.** These figures count grants of permanent resident status, not physical entries. Many grants go to individuals already living in the United States who adjust from a temporary status (such as student, asylee, or refugee) to permanent residence. The 1989–1991 peak was substantially driven by status adjustments under the Immigration Reform and Control Act of 1986, which allowed long-term undocumented residents to regularize their status, and by asylee and refugee adjustments from the post-1979 Iranian cohort.

### Income

Uses continuous household income (HHINCOME) from ACS PUMS. National household pre-tax income decile thresholds are computed from the full population aged 25–54, and Iranian households are placed into those bins. Each decile holds 10% of all U.S. households; deviation from 10% indicates over- or under-representation.

### Data Reliability

The ACS supports standard errors via replicate weights, but the dashboard presents descriptive estimates only for consistency across countries. The ACS sample is considerably larger than the Canadian PUMF, so reliability concerns are less acute for the U.S. estimates.

## Canada

### Population Identification

Iranian-Canadians are identified in the 2021 Census Public Use Microdata File (PUMF) using three criteria. A person is counted if they meet at least one:

1. **Place of birth** — born in Iran
2. **Ethnic origin** — reports Iranian ethnic or cultural origin
3. **Mother tongue or home language** — reports Iranian Persian as the language first learned or spoken most often at home

Unlike the U.S. census, the Canadian census distinguishes Iranian Persian from Afghan Dari, so Persian language can be used as an identification criterion.

**Population estimate range.** These three criteria overlap substantially. Most people born in Iran also report Iranian ethnic origin and speak Persian. The total count depends on how Persian speakers who do *not* separately report Iranian birthplace or ethnic origin are handled. These individuals are predominantly Canadian-born children of Iranian immigrants. Including them produces a broad estimate of approximately 240,000; excluding ambiguous cases (who may be Afghan, Tajik, or other Central Asian Persian speakers) produces a narrow estimate of approximately 224,000. The dashboard uses the narrow definition by default.

**Comparison with published census tables.** Statistics Canada reports 200,465 people with Iranian ethnic or cultural origin in the 2021 Census (157,475 single responses + 42,985 multiple responses). This figure counts only ethnic origin — one component of the compound definition used here. The dashboard estimate is larger because it also captures people identified through birthplace or language who reported a different ethnic origin (e.g., Persian, Kurdish, Azerbaijani).

**Limitations.** The PUMF individual file does not link household members. Unlike the U.S. ACS, where children can be identified through parental characteristics via household linkage, the Canadian PUMF does not allow researchers to identify children of Iranian parents unless those children independently report Iranian ethnic origin or speak Persian. The PUMF includes a generation status variable (first generation, second generation, etc.) derived from whether a respondent's parents were born outside Canada, but it does not preserve the parents' specific country of birth. This means that some Canadian-born children of Iranian immigrants — particularly those who do not speak Persian or report Iranian ethnicity — are not captured in the dashboard.

**Data access:** The 2021 Census PUMF can be requested through the [Canadian Census Analyser](https://dc1.chass.utoronto.ca/census/index.html).

### Income

The PUMF provides household income in 33 categorical bands rather than continuous dollar amounts. Each band is assigned its midpoint value, and national decile thresholds are computed from all 386,700 PUMF records aged 25–54. The same decile approach is used as for the United States to enable cross-country comparison.

The Canada income page also includes individual pre-tax income broken down by age group. This uses a different variable (total personal income, continuous) and is not directly comparable to the household decile chart or to the U.S. methodology.

### Data Reliability

The Canadian PUMF is a 2.7% sample of the population. For small sub-groups, estimates based on few observations may be unreliable. Each data file includes a reliability flag based on unweighted sample size, following Statistics Canada's disclosure guidelines:

- **Reliable** — 30 or more unweighted observations
- **Use with caution** — 10 to 29 unweighted observations
- **Suppress** — fewer than 10 unweighted observations

The PUMF does not include bootstrap replicate weights suitable for computing standard errors or confidence intervals. All Canadian estimates in the dashboard are descriptive and should not be used for statistical inference.

## Australia

### Population Identification

Iran-born Australians are identified in the 2021 Census of Population and Housing using a single criterion:

1. **Country of birth** — reports Iran as country of birth (BPLP code 4203)

The census question asks: "In which country was the person born?" The total count of 70,899 includes all persons enumerated in the 2021 Census who reported Iran as their birthplace, regardless of citizenship status or year of arrival.

**Scope.** Unlike the U.S. and Canadian estimates, which combine multiple identification criteria (ancestry, language, parental origin), the Australian figure counts only foreign-born residents. Second-generation Iranians born in Australia are not included unless future pages incorporate ancestry data (the census separately asks about ancestry, recording 81,111 people reporting Iranian ancestry in 2021).

**Age groupings.** The publicly available census tables from the ABS API provide age in broad bands (0–4, 5–14, 15–24, 25–44, 45–54, 55–64, 65–74, 75–84, 85+). The 25–44 band spans 20 years and contains 53% of all Iran-born Australians. Finer age breakdowns (5-year groups) are available through ABS TableBuilder.

**Year of arrival.** The census records year of arrival for overseas-born persons. Pre-2016 data is grouped into multi-year periods (e.g., 2001–2010, 2011–2015). From 2016 onward, individual years are recorded. The dashboard displays period data as per-year averages and annual data at actual counts, following the same convention used for the Canadian immigration page.

**Data access:** Census data tables are available from [ABS Census Data](https://www.abs.gov.au/census/find-census-data). Cross-tabulations of country of birth by socioeconomic variables (education, income, occupation) require the free [ABS TableBuilder](https://www.abs.gov.au/statistics/microdata-tablebuilder/tablebuilder) tool.

## Germany

### Population Identification

Iranian-origin residents in Germany are identified in the 2024 [Mikrozensus](https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Migration-Integration/Methoden/mikrozensus.html), an annual 1% household survey run by the Federal Statistical Office (Destatis). Destatis uses a strictly birth-based definition called *Migrationsgeschichte*. A person is counted if they meet at least one:

1. **Place of birth** — born in Iran
2. **Parental origin** — at least one parent born in Iran

The headline estimate of 319,000 includes both the first generation (born in Iran, 250,000) and the second generation (born in Germany with at least one Iran-born parent, 69,000).

Persian language is not used as a counting criterion because the Mikrozensus language code does not distinguish Iranian Persian from Afghan Dari, and Germany hosts a much larger Afghan-origin population than Iranian. The 176,000 Persian-at-home figure shown on the Language & Education page describes a subset of the 319,000 Iranian-origin population, not an alternative count.

Third-generation children (German-born with German-born parents but Iran-born grandparents) are not counted as Iranian-origin under any post-2017 Mikrozensus definition.

**Data access:** The Mikrozensus Erstergebnisse (first-results publication) is a free public download from [Destatis](https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Migration-Integration/Publikationen/_publikationen-innen-migrationshintergrund.html). Destatis also publishes a Scientific Use File with full microdata, available to researchers through [GESIS](https://www.gesis.org/); access is restricted to researchers at German-registered institutions.

### Annual Arrivals Time Series

The annual arrivals chart on the Immigration & Citizenship page shows Iranian *Zuzüge* (arrivals) in Germany from 1991 through 2023, compiled from four editions of the BAMF *Migrationsbericht* (2005, 2015, 2020, 2023). This is a flow series — it counts arrivals during each year, not the current Iran-born population. Overlapping years across editions are used as a sanity check and agree exactly. Pre-1991 data exists only for West Germany and is not included.

### Other Sources

The Immigration & Citizenship page also draws on the BAMF publication [*Das Bundesamt in Zahlen 2024*](https://www.bamf.de/SharedDocs/Anlagen/DE/Statistik/BundesamtinZahlen/) for asylum and naturalization counts.

The dashboard does not cite third-party analyses of German administrative data (such as the Institut der deutschen Wirtschaft Kurzberichte), even though their underlying data are official, in order to keep sourcing transparent and consistent.

## United Kingdom

### Population Identification

Iran-born residents in the United Kingdom are identified in the 2021 Census of England and Wales (ONS), Scotland's Census 2022, and the 2021 Census of Northern Ireland (NISRA). All four nations ask the same place-of-birth question. A person is counted if they report Iran as their country of birth.

**Narrower than the US/Canada definition.** The UK census does not ask about ancestry or parental origin, so British-born children of Iran-born parents are not counted here. This makes the 114,432 UK total a narrower estimate than the compound definitions used on the United States and Canada pages.

The regional map on the UK Population page aggregates the 331 lower-tier local authorities in England and Wales into the nine English regions plus Wales, and appends Scotland's total. Northern Ireland (461 Iran-born residents) is not shown on the regional map because the Great Britain regions GeoJSON does not include it.

**Data access:** England and Wales counts are from [NOMIS](https://www.nomisweb.co.uk/sources/census_2021) table TS012. Scotland and Northern Ireland totals are published directly by [Scotland's Census](https://www.scotlandscensus.gov.uk/) and [NISRA](https://www.nisra.gov.uk/statistics/census/2021-census).

## Europe Overview

The All Europe overview page on the Europe tab combines three types of sources to produce a single comparable ranking of Iran-born populations across 11 European countries:

- **[Eurostat](https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/default/table)** publishes population by country of birth for nine European countries (Austria, Belgium, Denmark, France, Italy, Netherlands, Norway, Sweden, Switzerland), available as annual time series going back to 1998.
- **Destatis Mikrozensus 2024** is used for Germany, because Eurostat does not publish Iran-born figures for Germany (only Iranian-citizen counts, which would leave out about 100,000 naturalized Germans).
- **UK national censuses** are used for the United Kingdom, because the UK is not a Eurostat reporting country after Brexit. The UK figure is summed across the 2021 Censuses of England, Wales, and Northern Ireland and the 2022 Census of Scotland.

All figures count people born in Iran who currently reside in the reporting country. Second-generation European-born children of Iran-born parents are not included in the totals shown on the overview page, because Eurostat does not publish a harmonized second-generation count. National sources that do collect second-generation data (Germany, the Netherlands, Sweden, Denmark, Norway) report larger Iranian-origin populations on their individual country pages.

Denmark, France, and Switzerland have gaps in recent Eurostat reporting and are shown with their most recent available year, which may lag by several years. Reference years for each country are shown in the map tooltips.

## Global

Global migration figures are drawn from the [UN International Migrant Stock](https://www.un.org/development/desa/pd/content/international-migrant-stock) database (2024 revision). These count people born in Iran who reside in another country, based on each country's census or population register. Second-generation Iranians born in the destination country are not included.

## Replication

The repository is organized so that the build stage is fully reproducible
from committed intermediate data, while the upstream extraction stage
requires raw microdata that is not committed here.

**Build stage (always reproducible in-place).** The scripts in `R/build_*.R`
regenerate every page under `docs/pages/` from the committed intermediate
data under `data/`:

```
Rscript R/build_us.R
Rscript R/build_canada.R
Rscript R/build_global.R
Rscript R/build_australia.R
Rscript R/build_germany.R
Rscript R/build_uk.R
Rscript R/build_europe.R
```

Anyone with a working R installation and the packages listed below can
reproduce the full dashboard from this repository without touching raw
microdata.

**Upstream extraction stage (requires raw microdata).** The intermediate
files under `data/us/`, `data/canada/`, `data/global/`, `data/australia/`,
`data/germany/`, `data/uk/`, and `data/europe/` are produced by separate
export pipelines that read raw census/PUMF microdata from outside this
repository. Those raw inputs are 1–10 GB in total and not redistributable,
so they are not committed here. The export scripts live in two places:

- **U.S. pipeline** — `../ID_Analysis/scripts/` (separate repo, ACS PUMS
  pulled via `tidycensus`).
- **Canada pipeline** — `../IDD_Canada/R/` (separate repo, 2021 PUMF fixed-width
  file under `../IDD_Canada/data/pumf_individual/`).
- **Australia** — `R/australia_export/` in this repo (raw ABS inputs under
  `../_data/australia/`).
- **Germany** — `R/germany_export/` in this repo (Mikrozensus xlsx under
  `../_data/germany/`).
- **United Kingdom** — `R/uk_export/` in this repo (NOMIS CSVs under
  `../_data/uk/`).
- **Europe overview** — `R/europe_export/` in this repo (Eurostat CSV under
  `../_data/eurostat/` plus the Germany and UK files above).
- **Global (UN Migrant Stock)** — `R/global_export/export_un_migrant_stock.R`
  (raw UN xlsx staged in `../ID_Analysis/output/dashboard_us/`).

The copies under `R/us_export/` and `R/canada_export/` are reference
snapshots of the upstream pipeline scripts, preserved for transparency.
They are not runnable from this repo on their own — see
`R/canada_export/README.md` for details.

**R package dependencies** for the build stage: `plotly`, `dplyr`,
`jsonlite`, `readxl`, `sf`, `ozmaps` (Australia only), `cancensus` (Canada
Ontario CSD map only). `mapgl` and `tigris` are used by the U.S. state-map
builder.

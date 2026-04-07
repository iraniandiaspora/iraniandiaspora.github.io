# Methodology

## Population Identification

### United States

Iranian-Americans are identified in the American Community Survey (ACS) using four criteria. A person is counted if they meet at least one:

1. **Place of birth** — reports being born in Iran
2. **Ancestry** — reports "Iranian" as first or second ancestry
3. **Race** — writes in "Iranian" under the White race category (available in 2024 only)
4. **Parental origin** — lives with a parent who meets any of the above criteria

The headline estimate (794,915) uses the 2024 ACS 1-Year survey, which includes all four criteria. All other charts use the 2020–2024 ACS 5-Year pooled data (754,595), which excludes the race write-in category (not available in 5-year files).

Persian language speakers are not counted as a standalone criterion because the Census language code also captures Afghan Dari speakers.

**Data access:** ACS Public Use Microdata Samples are available from the [U.S. Census Bureau](https://www.census.gov/programs-surveys/acs/microdata.html) and [IPUMS USA](https://usa.ipums.org/).

### Canada

Iranian-Canadians are identified in the 2021 Census Public Use Microdata File (PUMF) using three criteria:

1. **Place of birth** — born in Iran
2. **Ethnic origin** — reports Iranian ethnic or cultural origin
3. **Mother tongue or home language** — reports Iranian Persian as the language first learned or spoken most often at home (the Canadian census distinguishes Iranian Persian from Afghan Dari, unlike the U.S. census)

The population estimate ranges from 223,968 (narrow, excluding ambiguous Persian speakers) to 240,189 (broad, including all criteria).

**Data access:** The 2021 Census PUMF can be requested through the [Canadian Census Analyser](https://dc1.chass.utoronto.ca/census/index.html) at the University of Toronto (restricted to faculty, staff, and students at subscribing institutions).

### Global

Global migration figures are drawn from the [UN International Migrant Stock](https://www.un.org/development/desa/pd/content/international-migrant-stock) database (2024 revision). These count people born in Iran who reside in another country, based on each country's census or population register. Second-generation Iranians born in the destination country are not included.

## Income Methodology

Both countries use the same approach to enable cross-country comparison:

1. Compute national household pre-tax income decile thresholds from the full population aged 25–54
2. Place Iranian households into those decile bins
3. Display the share of Iranians in each decile (10% per decile = no difference from the national distribution)

**United States:** Uses continuous household income from ACS PUMS.

**Canada:** The PUMF provides household income in 33 categorical bands rather than continuous dollar amounts. Each band is assigned its midpoint value, and national decile thresholds are computed from all 386,700 PUMF records aged 25–54.

The Canada income page also includes individual pre-tax income broken down by age group. This measure provides an additional view of income variation across the life course, particularly since the second-generation sample in Canada is too small for a separate decile analysis.

## Replication

The R scripts and intermediate data files needed to rebuild the dashboard are included in this repository. The `R/` directory contains the build scripts that generate static HTML pages, and `R/us_export/` and `R/canada_export/` contain the data processing scripts that produce the intermediate files from raw census microdata. The raw ACS PUMS and Canadian PUMF files are not included due to size and licensing but can be obtained from the sources listed above.

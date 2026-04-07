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

Iranian-Canadians are identified in the 2021 Census Public Use Microdata File (PUMF) using three criteria. A person is counted if they meet at least one:

1. **Place of birth** — born in Iran
2. **Ethnic origin** — reports Iranian ethnic or cultural origin
3. **Mother tongue or home language** — reports Iranian Persian as the language first learned or spoken most often at home

Unlike the U.S. census, the Canadian census distinguishes Iranian Persian from Afghan Dari, so Persian language can be used as an identification criterion.

**Population estimate range.** These three criteria overlap substantially. Most people born in Iran also report Iranian ethnic origin and speak Persian. The total count depends on how Persian speakers who do *not* separately report Iranian birthplace or ethnic origin are handled. These individuals are predominantly Canadian-born children of Iranian immigrants. Including them produces a broad estimate of approximately 240,000; excluding ambiguous cases (who may be Afghan, Tajik, or other Central Asian Persian speakers) produces a narrow estimate of approximately 224,000. The dashboard uses the narrow definition by default.

**Comparison with published census tables.** Statistics Canada reports 200,465 people with Iranian ethnic or cultural origin in the 2021 Census (157,475 single responses + 42,985 multiple responses). This figure counts only ethnic origin — one component of the compound definition used here. The dashboard estimate is larger because it also captures people identified through birthplace or language who reported a different ethnic origin (e.g., Persian, Kurdish, Azerbaijani).

**Limitations.** The PUMF individual file does not link household members. Unlike the U.S. ACS, where children can be identified through parental characteristics via household linkage, the Canadian PUMF does not allow researchers to identify children of Iranian parents unless those children independently report Iranian ethnic origin or speak Persian. The PUMF includes a generation status variable (first generation, second generation, etc.) derived from whether a respondent's parents were born outside Canada, but it does not preserve the parents' specific country of birth. This means that some Canadian-born children of Iranian immigrants — particularly those who do not speak Persian or report Iranian ethnicity — are not captured in the dashboard.

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

The Canada income page also includes individual pre-tax income broken down by age group. This uses a different variable (total personal income, continuous) and is not directly comparable to the household decile chart or to the U.S. methodology.

## Data Reliability

### Canada

The Canadian PUMF is a 2.7% sample of the population. For small sub-groups, estimates based on few observations may be unreliable. Each data file includes a reliability flag based on unweighted sample size, following Statistics Canada's disclosure guidelines:

- **Reliable** — 30 or more unweighted observations
- **Use with caution** — 10 to 29 unweighted observations
- **Suppress** — fewer than 10 unweighted observations

The PUMF does not include bootstrap replicate weights suitable for computing standard errors or confidence intervals. All Canadian estimates in the dashboard are descriptive and should not be used for statistical inference.

### United States

The ACS supports standard errors via replicate weights, but the dashboard presents descriptive estimates only for consistency across countries. The ACS sample is considerably larger than the Canadian PUMF, so reliability concerns are less acute for the U.S. estimates.

## Replication

The R scripts and intermediate data files needed to rebuild the dashboard are included in this repository. The `R/` directory contains the build scripts that generate static HTML pages, and `R/us_export/`, `R/canada_export/`, and `R/global_export/` contain the data processing scripts that produce the intermediate files from raw census microdata. The raw ACS PUMS and Canadian PUMF files are not included due to size and licensing but can be obtained from the sources listed above.

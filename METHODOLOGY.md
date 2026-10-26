# Methodology

This page describes how the Iranian Diaspora Dashboard identifies and counts Iranian-origin populations in each country, what data sources are used, and what methodological choices were made.

The dashboard is published in English and Persian (فارسی). Both editions are built from the same data; the English edition is the reference version.

## Global

Global figures are drawn primarily from the [UN International Migrant Stock](https://www.un.org/development/desa/pd/content/international-migrant-stock) database (2024 revision), which counts people born in Iran who reside in another country. Estimates are based on each country's most recent census or population register. For countries where UN coverage is incomplete, the data are supplemented by [Eurostat](https://ec.europa.eu/eurostat) population-by-country-of-birth tables. For Israel, the 2024 figure uses the CBS register count (38,200) instead of the UN estimate; see the Israel section below for details. The headline total is computed dynamically from the combined data.

## United States

### Population

Iranian-Americans are identified in the American Community Survey (ACS) using four criteria. A person is counted if they meet at least one:

1. **Place of birth** – born in Iran
2. **Ancestry** – reports "Iranian" as first or second ancestry
3. **Race** – writes in "Iranian" under the White race category (2024 survey only)
4. **Parental origin** – lives with a parent who meets any of the above criteria

The headline estimate of 794,915 uses the 2024 ACS 1-Year survey, which is the only year that includes all four criteria. The detail pages (immigration, marriage, education, work, income) use the 2020–2024 ACS 5-Year pooled data (754,595), which excludes the race write-in because it is not available in multi-year files. The waterfall chart on the Population page shows how each criterion, and its overlaps, contribute to the total.

The race criterion counts people who write in "Iranian" as a single race; those who mark "Iranian" together with another race are included only when they also report Iranian birthplace or ancestry, so a small number of multiracial Iranians who identify through the race question alone are not counted.

Persian language is not used as a standalone criterion because the Census language category also captures Afghan Dari speakers.

### Income

The income page compares Iranian-American households to the national distribution. National pre-tax household income decile thresholds are computed from all U.S. households with a reference person aged 25–54. Iranian households are then placed into those decile bins. Each decile holds 10% of all U.S. households; shares above or below 10% indicate over- or under-representation.

### Immigration History

The Immigration History page shows all Iranians granted lawful permanent resident status from fiscal year 1970 through 2024 – a total of 596,144 grants across 55 years.

Sources by period:

- **1970–1977:** U.S. Immigration and Naturalization Service, *Annual Report* (Washington, D.C.: U.S. Government Printing Office). Admission categories differ from later years and are shown as a single combined total.
- **1978–2004:** INS *Statistical Yearbook of the Immigration and Naturalization Service*.
- **2005–2024:** Department of Homeland Security, Office of Immigration Statistics, [*Yearbook of Immigration Statistics*](https://ohss.dhs.gov/topics/immigration/yearbook).

Prior to 1992, immigration law did not separate family and employment preferences, so these appear as a combined category for 1978–1991. Fiscal year 1976 includes the transition quarter (July–September 1976) when the federal fiscal year shifted from a July–June to an October–September cycle.

**Data access:** ACS Public Use Microdata Samples are available from the [U.S. Census Bureau](https://www.census.gov/programs-surveys/acs/microdata.html) and [IPUMS USA](https://usa.ipums.org/). Immigration data are from the [DHS Yearbook of Immigration Statistics](https://ohss.dhs.gov/topics/immigration/yearbook).

### Language

The Language page reports the language spoken at home, from the census language question in the ACS Public Use Microdata Sample. Persian/Farsi is shown as its own category. Respondents who report no language other than English are recorded as "English only." "Other language" combines every remaining language; among first-generation Iranian-Americans, Armenian accounts for roughly two-thirds of this group.

## Europe Overview

The Europe overview page combines six source families to show Iran-born populations across 23 countries:

- **[Eurostat](https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/default/table)** – population by country of birth for eighteen countries (Austria, Belgium, Bulgaria, Czechia, Estonia, Finland, Hungary, Iceland, Italy, Latvia, Lithuania, Luxembourg, Netherlands, Norway, Romania, Slovakia, Slovenia, Sweden), with annual data back to 1999.
- **Destatis Mikrozensus 2025** – for Germany, because Eurostat does not publish Iran-born counts for Germany.
- **[Statistics Denmark (DST)](https://www.dst.dk/en/)** – for Denmark, using the population register, because Eurostat's Denmark series ends in 2022. The two sources agree within about 1% in their overlap years.
- **National censuses** – for the United Kingdom (combined from ONS, NRS, and NISRA as described below).
- **[Swiss Federal Statistical Office (BFS)](https://www.bfs.admin.ch/bfs/en/home.html)** – for Switzerland, using the STATPOP register.
- **[INSEE Recensement de la population](https://www.insee.fr/)** – for France, because INSEE uses a narrower "immigré" definition that is not directly comparable to Eurostat's foreign-born definition.

All figures on the overview page count people born in Iran. Second-generation European-born children are not included in the overview totals because Eurostat does not publish a harmonized second-generation count across countries. Türkiye is not part of the Europe overview; it has its own section below. The over-time chart covers 1999–2026.

## Germany

Iranian-origin residents are identified in the 2025 [Mikrozensus](https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Migration-Integration/Methoden/mikrozensus.html), an annual 1% household survey conducted by the Federal Statistical Office (Destatis). A person is counted if they were born in Iran or have at least one parent born in Iran. The headline estimate of 334,000 includes both the first generation (267,000 born in Iran) and the second generation (67,000 born in Germany to Iranian-born parents).

Persian language is not used as a counting criterion because the language category does not distinguish Iranian Persian from Afghan Dari, and Germany has a larger Afghan-origin than Iranian-origin population.

Mikrozensus figures are rounded to the nearest thousand. State-level counts below 5,000 are suppressed by Destatis and appear as grey on the geography charts.

The annual arrivals chart compiles Iranian arrivals from 1991 through 2024 using five editions of the BAMF *Migrationsbericht* (2005, 2015, 2020, 2023, 2024).

**Data access:** Mikrozensus publications are available from [Destatis](https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Migration-Integration/Publikationen/_publikationen-innen-migrationshintergrund.html). BAMF publications are at [bamf.de](https://www.bamf.de/SharedDocs/Anlagen/DE/Statistik/BundesamtinZahlen/).

## Sweden

Iranian-origin residents are identified in the population register maintained by [Statistics Sweden (SCB)](https://www.scb.se/en/). A person is classified as Iranian-origin if they were born in Iran (first generation) or were born in Sweden with at least one parent born in Iran (second generation). The headline figure of 135,704 includes both generations (2025). The second generation includes children with one Swedish-born and one Iran-born parent.

Third-generation residents (Swedish-born with Swedish-born parents) are not counted. Classification is from birth records, not self-reported ethnicity.

The population-over-time chart covers 1990–2025. It uses Eurostat data (Iran-born stock, 1999–2025) for consistency with the Europe overview, with missing years filled from SCB and the 1990 and 1995 points from the UN International Migrant Stock. The year-of-arrival chart converts SCB data on years since immigration among Iran-born residents (2024) into calendar year of arrival.

**Data access:** SCB data are publicly available via the [PxWeb API](https://www.scb.se/en/services/open-data-api/api-for-the-statistical-database/).

## United Kingdom

### Population

Iran-born residents are identified in the 2021 Census of England and Wales (ONS), Scotland's Census 2022, and the 2021 Census of Northern Ireland (NISRA). All four nations ask the same place-of-birth question. The combined total is 114,432.

The UK Census does not ask about ancestry or parental origin, so British-born children of Iran-born parents are not captured. The regional map aggregates local authorities into nine English regions plus Wales, Scotland, and Northern Ireland (12 regions; Northern Ireland has 461 Iran-born residents).

### Cross-tabulations (Population and Work & Education pages)

Age, sex, economic activity, highest qualification, year of arrival, and religion cross-tabulations for Iran-born residents in England and Wales are drawn from the ONS Census 2021 custom dataset tool. Age and sex, year of arrival, and religion appear on the Population page; economic activity and qualification appear on the Work & Education page. This tool allows cross-tabulation of country of birth with socioeconomic variables not available in pre-built tables. Data cover England and Wales only; Scotland and Northern Ireland do not publish comparable cross-tabulations by country of birth.

**Data access:** England and Wales data from [NOMIS](https://www.nomisweb.co.uk/sources/census_2021) and the [ONS custom dataset tool](https://www.ons.gov.uk/datasets/create). Scotland from [Scotland's Census](https://www.scotlandscensus.gov.uk/). Northern Ireland from [NISRA](https://www.nisra.gov.uk/statistics/census/2021-census).

## Netherlands

Iranian-origin residents are identified in the population register maintained by [CBS Statistics Netherlands](https://opendata.cbs.nl/statline/). The Netherlands does not conduct a traditional census; population statistics are derived from the municipal personal records database (BRP). A person is classified as Iranian-origin if they were born in Iran (first generation) or were born in the Netherlands with at least one parent born in Iran (second generation). The headline figure of 65,721 includes both generations (January 2026).

Third-generation residents (Dutch-born with Dutch-born parents) are not counted as Iranian-origin. The classification is based on birth records, not self-reported ethnicity or language.

The population-over-time chart uses Eurostat data (Iran-born stock, 1999–2025), with 1990 and 1995 points from the UN International Migrant Stock, because CBS only began publishing Iran-specific population counts in 2022. The year-of-arrival chart derives arrival timing from single-year residence duration in the population register; it shows who is currently present, not total historical arrivals.

Labor force participation and employment type come from the CBS Labour Force Survey (2021–2024). Income data come from the CBS household income and wealth tables (2011–2024). Average disposable income figures are in nominal euros. The low-income threshold is inflation-adjusted by CBS, so the low-income share is comparable across years.

**Data access:** All CBS data are publicly available via the [StatLine](https://opendata.cbs.nl/statline/) portal and its [programmatic API](https://www.cbs.nl/en-gb/our-services/open-data/statline-as-open-data).

## Denmark

Iranian-origin residents are identified in the population register maintained by [Statistics Denmark (DST)](https://www.dst.dk/en/). A person is classified as Iranian-origin if they are an *indvandrer* (immigrant: born in Iran, neither parent a Danish citizen born in Denmark) or an *efterkommer* (descendant: born in Denmark, neither parent a Danish citizen born in Denmark). The headline figure of 28,855 includes both categories (2026).

The descendant category requires that neither parent is a Danish citizen born in Denmark. Third-generation residents (both parents Danish-born citizens) are not counted. The population chart shows both immigrants and descendants as a stacked area chart, 1980–2026.

Employment data come from the Register-based Labour Force Statistics (RAS), which provides a 19-sector industry breakdown for Iranian-origin full-time employees (fourth quarter of 2024).

**Data access:** DST data are publicly available via [StatBank Denmark](https://www.statbank.dk/statbank5a/default.asp?w=1920).

## Norway

Iranian-origin residents are identified in the population register maintained by [Statistics Norway (SSB)](https://www.ssb.no/en/). A person is classified as Iranian-origin if they are an *innvandrer* (immigrant: born in Iran with two foreign-born parents) or *norskfødt med innvandrerforeldre* (Norwegian-born to immigrant parents: born in Norway with two Iran-born parents). The headline figure of 26,497 includes both categories (2026).

Children with one Norwegian-born parent are not counted as immigrants or Norwegian-born to immigrant parents. The population page has two over-time tabs: the total Iran-born population (1970–2026) and a stacked area chart of immigrants and Norwegian-born to immigrant parents (2010–2026). Employment rate data cover ages 20–66, 2001–2025, from the annual labor force register published by SSB.

**Data access:** SSB data are publicly available via [StatBank](https://www.ssb.no/en/statbank).

## Austria

Iran-born residents are identified in the population register maintained by [Statistik Austria](https://www.statistik.at/) and reported via [Eurostat](https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/default/table). A person is counted as Iran-born based on their registered country of birth. The headline figure of 31,135 is the most recent annual stock (2025).

Austrian-born children of Iran-born parents are not counted as Iran-born in population statistics. This figure counts first-generation residents only. The trend chart covers 1990–2025. The geographic distribution by Bundesland uses 2021 register-based census data from [Eurostat](https://ec.europa.eu/eurostat/databrowser/).

**Data access:** Eurostat population-by-country-of-birth tables are publicly available via the [Eurostat Data Browser](https://ec.europa.eu/eurostat/databrowser/). Bundesland-level data are from the Eurostat Census 2021 tables.

## Italy

Iran-born residents are identified in the population register maintained by [ISTAT](https://demo.istat.it/) and reported via [Eurostat](https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/default/table). A person is counted as Iran-born based on their registered country of birth. The headline figure of 30,532 is the most recent annual stock (2025).

Italian-born children of Iran-born parents are not counted. The geographic distribution covers all 20 Italian regions. The trend chart covers 2002–2025.

**Data access:** ISTAT population data are available from [demo.istat.it](https://demo.istat.it/). Eurostat data are available via the [Eurostat Data Browser](https://ec.europa.eu/eurostat/databrowser/).

## France

Iran-born residents are identified in the [INSEE](https://www.insee.fr/) Recensement de la population using the "immigré" definition: a person born abroad with foreign nationality at birth. This definition is narrower than Eurostat's foreign-born concept, so France's values are not strictly comparable with the other Eurostat-sourced European countries on the overview chart. The headline figure of 23,800 is the most recent published Iran-specific figure (2019).

French law (Loi Informatique et Libertés, Art. 8) prohibits collecting ethnicity or ancestry statistics, so French-born children of Iran-born parents are not separately identifiable in published tables. INSEE publishes detailed country-of-birth tables with a multi-year lag. The 2021 table has been withdrawn and replaced by the 2022 census tables, which group Iran with other Asian countries; 2019 is therefore the most recent published Iran-specific figure. The trend chart covers 1968–2019 (periodic censuses 1968–1999, annual from 2006).

**Data access:** INSEE country-of-birth tables are linked from [insee.fr](https://www.insee.fr/fr/statistiques/6478089).

## Switzerland

Iran-born residents are identified in the population register maintained by the [Swiss Federal Statistical Office (BFS)](https://www.bfs.admin.ch/bfs/en/home.html). A person is counted as Iran-born based on their registered country of birth. The headline figure of 17,774 is the most recent annual stock (2025).

The register tracks country of birth but not parental birthplace; Swiss-born children of Iran-born parents are not counted. The geographic distribution covers all 26 cantons. The trend chart covers 2010–2025, with an additional chart showing annual immigration from Iran (2011–2025).

**Data access:** BFS population data are publicly available from [bfs.admin.ch](https://www.bfs.admin.ch/bfs/en/home.html).

## Finland

Iranian-origin residents are identified in the population register maintained by [Statistics Finland (StatFin)](https://stat.fi/en/). A person is classified as Iranian-origin if they were born in Iran (first generation) or were born in Finland with at least one parent born in Iran (second generation), using StatFin's "background country" classification. The headline figure of 15,164 includes both generations (2025).

Third-generation Finnish-born residents are not counted as Iranian-origin. The population-over-time chart shows the first and second generations as a stacked area over 1990–2025.

**Data access:** StatFin data are publicly available via the [Statistics Finland database](https://stat.fi/en/).

## Canada

### Population

Iranian-Canadians are identified in the 2021 Census Public Use Microdata File (PUMF) using three criteria. A person is counted if they meet at least one:

1. **Place of birth** – born in Iran
2. **Ethnic origin** – reports Iranian ethnic or cultural origin
3. **Mother tongue or home language** – reports Iranian Persian as the language first learned or spoken most often at home

Unlike the U.S. Census, the Canadian Census distinguishes Iranian Persian from Afghan Dari, so Persian language can serve as an identification criterion. However, not all Persian speakers are Iranian. The dashboard uses a narrow definition (~223,931) that restricts the Persian-language criterion to second- and third-generation respondents who also identify as [West Asian visible minorities](https://www23.statcan.gc.ca/imdb/p3Var.pl?Function=DEC&Id=45152), excluding first-generation Afghan, Tajik, and other Central Asian Persian speakers.

The PUMF does not link household members, so children of Iranian parents cannot be identified through parental characteristics the way they can in the U.S. ACS. The PUMF is a 2.7% sample.

### Income

The PUMF reports household income in categorical bands rather than continuous dollar amounts. Each band is assigned its midpoint value, and national decile thresholds are computed from all PUMF records aged 25–54. This matches the U.S. approach to enable cross-country comparison. A separate chart on the income page shows individual pre-tax income by age group using a different variable and is not directly comparable to the decile chart.

**Data access:** The 2021 Census PUMF can be requested through the [Canadian Census Analyser](https://dc1.chass.utoronto.ca/census/index.html).

## Türkiye

Iran-born residents are identified in the address-based population register (ADNKS) maintained by [TÜİK](https://data.tuik.gov.tr/) and reported via [Eurostat](https://ec.europa.eu/eurostat/databrowser/view/migr_pop3ctb/default/table). Years follow Eurostat's convention: a figure labelled 2026 is the stock on 1 January 2026, which TÜİK reports as 31 December 2025. The headline figure of 131,957 is the most recent annual stock (2026), taken from TÜİK's population statistics portal ahead of its Eurostat release; it is down from a peak of 154,965 in 2022. The trend chart covers 2015–2026, using TÜİK's portal for years Eurostat does not report (2015–2018, 2021–2022 and 2026). Türkiye-born children of Iran-born parents are not counted.

The Immigration page shows Iranian passport holders resident in Türkiye (2014–2026), which grew 7.7-fold between 2014 (16,834) and the 2022 peak (128,883) before easing to 91,672 in 2026. The difference between Iran-born and Iranian-citizen stock approximates — but does not exactly measure — naturalization to Turkish citizenship, because it also includes Iran-born non-Iranian nationals. A second tab shows Iranian residence permits by type (short-term, student, family, and other long-term) from a Presidency of Migration Management (Göç İdaresi) snapshot dated 7 May 2026.

Provincial (il-level) breakdowns, education, employment, and income cross-tabulations for Iran-born residents are not in public tables and would require custom data requests to TÜİK.

**Data access:** Eurostat population tables are publicly available via the [Eurostat Data Browser](https://ec.europa.eu/eurostat/databrowser/). TÜİK International Migration Statistics are at [data.tuik.gov.tr](https://data.tuik.gov.tr/).

## Israel

Iranian-origin residents are identified in the population register maintained by the [Israel Central Bureau of Statistics (CBS)](https://www.cbs.gov.il/en). CBS uses a paternal-line, two-generation definition: a person is counted as Iranian-origin if they were born in Iran, or were born in Israel with a father born in Iran. The headline figure of 130,600 includes both generations (2025).

Third-generation residents (Israeli-born with an Israeli-born father) and maternal-only lines are not counted. CBS publishes counts in thousands. The age chart presents both generations together using harmonized bins, since the Israeli-born column is open-ended above 55. A second tab shows the full age detail for the Iran-born population, including the 75+, 65–74, and 55–64 bins.

The comparison chart shows the Iranian-origin group alongside other Asian-origin groups in Israel under the same definition. CBS does not publish geographic distribution by individual country of origin (only by continent), so a regional map is not included.

The [UN International Migrant Stock 2024](https://www.un.org/development/desa/pd/content/international-migrant-stock) reports approximately 61,500 Iran-born residents in Israel in 2024, higher than the CBS 2024 figure of 38,200. The UN combines Israeli foreign-born data with UNHCR refugee additions and interpolates between reporting points; UN documentation does not specify which Israeli source it draws on. The dashboard uses the CBS register count as the headline figure on the Israel page and as Israel's 2024 value on the Global page. The population-over-time tab on the Israel page uses the UN International Migrant Stock throughout (1990–2024).

**Data access:** CBS data are publicly available from the [Israel Central Bureau of Statistics](https://www.cbs.gov.il/en).

## Australia

### Population

Iranian-origin Australians are identified in the 2021 Census of Population and Housing using four criteria. A person is counted if they meet at least one:

1. **Country of birth** – born in Iran
2. **Ancestry** – reports Iranian ancestry
3. **Language at home** – speaks Persian (excluding Dari)
4. **Parental birthplace** – Australian-born with at least one Iran-born parent

The compound total of ~110,993 counts each person once, even if they meet more than one criterion. The waterfall chart on the Population page shows how each criterion and its overlaps contribute to the total.

Within the Iranian-ancestry population (81,119), the Census separates the first generation (born overseas, 62,986) from the second generation (Australian-born, 18,133); the Population page shows this split. The Education page reports language spoken at home by generation: Persian use at home falls from 81% in the first generation to 47% in the second, while English-only use rises from 11% to 43%.

Cross-tabulations by education, religion, language, occupation, industry, and income are derived from ABS TableBuilder, which provides Iran-born breakdowns not available in standard published tables. Cell counts are randomly adjusted by ABS to prevent identification of individuals.

Year of arrival is grouped into multi-year periods before 2016 and individual years from 2016 onward. The chart displays period data as per-year averages for visual consistency with the annual bars.

### Income

The income page compares Iran-born Australians to the national distribution using personal income rather than household income. The Australian Census does not allow cross-tabulating household income with country of birth, so personal weekly income is used instead. National decile thresholds are computed from all Australians aged 25–54, and Iran-born individuals in the same age range are placed into those decile bins. The chart uses a teal line to distinguish it from the household income decile charts used for the United States and Canada.

**Data access:** Census tables are available from [ABS Census Data](https://www.abs.gov.au/census/find-census-data). Cross-tabulations by country of birth require [ABS TableBuilder](https://www.abs.gov.au/statistics/microdata-tablebuilder/tablebuilder) (free registration).

## Armenia

The Armenia page combines two sources. The trend line uses the [UN International Migrant Stock 2024](https://www.un.org/development/desa/pd/content/international-migrant-stock) for Iran-born estimates at five-year intervals, 1990–2024. Two additional points come from the [Statistical Committee of the Republic of Armenia (Armstat)](https://armstat.am/en/) Population Censuses: the 2011 census recorded 10,532 Iran-born residents and the 2022 census recorded 5,470. The 2011 census count is 5% below the UN 2010 estimate (11,117), and the 2022 census count is 10% below the UN 2020 estimate (6,065) and 19% below the UN 2024 estimate (6,793). Both sources show a decline from roughly 22,000 Iran-born residents in 1990 (UN) to 5,470 in the 2022 census.

**Data access:** UN Migrant Stock data are at the [UN DESA Population Division](https://www.un.org/development/desa/pd/content/international-migrant-stock). Armstat census tables are at [armstat.am](https://armstat.am/en/).

## Data Reliability

All estimates on the dashboard are descriptive. The dashboard does not display standard errors or confidence intervals. The U.S. ACS microdata include replicate weights that support variance estimation; the Canadian PUMF does not include replicate weights; the Australian Census and the population registers are full counts. Showing no intervals keeps the presentation consistent across countries whose sources have different statistical frameworks.

For the Canadian PUMF (2.7% sample), small sub-groups may have few observations. For the German Mikrozensus (1% sample), state-level counts below 5,000 are suppressed by Destatis. The U.S. ACS has a large sample and fewer reliability concerns at the subgroup level. Population-register counts (Netherlands, Sweden, Denmark, Norway, Finland, Austria, Italy, Switzerland, Türkiye, and Israel) are full-population counts without sampling error. The Netherlands labour force and income figures come from the CBS Labour Force Survey and household income statistics, which are sample-based.

## Replication

The R build scripts (`R/build_*.R`) regenerate every page from intermediate data committed under `data/`. The intermediate data files are produced by upstream extraction scripts under `R/<country>_export/` for the European countries, Türkiye, Israel, Armenia, Australia, the Global page, and the U.S. business-ownership charts, and under a separate `pipelines/` directory for the U.S. and Canada. Some upstream pipelines require raw census microdata not included in this repository (not redistributable).

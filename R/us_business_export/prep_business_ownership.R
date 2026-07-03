#!/usr/bin/env Rscript
# Prepare tidy business-ownership CSVs for the us-work page from the validated
# ID_Analysis outputs (Fairlie-style self-employment on ACS/IPUMS 2020-2024
# 5-year — the SAME survey vintage the us-work/us-income pages already use).
# "Business owner" = employed, age 20+, >=15 hrs/week, main-job class of worker
# = self-employed (CLASSWKR). Iranians = Iran-born (first generation).
#
# Run from the deployment repo root:
#   Rscript R/us_business_export/prep_business_ownership.R
# Reads the sibling analysis project's outputs (not in this repo's git, same as
# the _data extract scripts); writes three CSVs into data/us/.
suppressMessages({library(dplyr); library(readr)})

SRC <- "../../2026_04_ID_Analysis/output"
OUT <- "data/us"

# ---- 1. Self-employment rate by country of birth (comparative bar) ----------
osum <- read_csv(file.path(SRC, "us_iranian_business_owner_extension",
                           "origin_business_owner_summary.csv"), show_col_types = FALSE)
# Curated, well-sampled comparison set: MENA/West-Asian peers + major
# immigrant-origin groups spanning the range + the all-immigrant benchmark.
keep_birth <- c("Israel/Palestine","Armenia","Iran","Lebanon/Syria","Turkey",
                "Korea","Vietnam","Pakistan","China","India","Mexico","Philippines")
rate_birth <- osum %>%
  filter(frame == "birthplace_first_generation", origin %in% keep_birth) %>%
  transmute(origin, rate_pct = round(business_owner_rate_pct),
            unweighted_n = business_owner_unweighted_n,
            is_iran = origin == "Iran", is_benchmark = FALSE)
rate_bench <- osum %>%
  filter(frame == "foreign_born_benchmark") %>%
  transmute(origin = "All immigrants", rate_pct = round(business_owner_rate_pct),
            unweighted_n = business_owner_unweighted_n,
            is_iran = FALSE, is_benchmark = TRUE)
rate <- bind_rows(rate_birth, rate_bench) %>% arrange(desc(rate_pct))
write_csv(rate, file.path(OUT, "us_business_rate.csv"))

# ---- 2. Industries of Iranian-owned businesses (stable cells only) ----------
ind_labels <- c(
  "Arts/entertainment/accommodation/food" = "Arts, entertainment & food service",
  "Education/health/social services"      = "Education, health & social services",
  "Manufacturing"                          = "Manufacturing",
  "Finance/real estate/professional"       = "Finance, real estate & professional",
  "Professional/scientific/management"     = "Professional & scientific services",
  "Other services"                         = "Other services",
  "Transport/utilities/information"        = "Transportation, utilities & information",
  "Wholesale/retail"                       = "Wholesale & retail trade")
ind <- read_csv(file.path(SRC, "us_iranian_business_owner_extension",
                          "industry_business_owner_profile.csv"), show_col_types = FALSE) %>%
  filter(origin == "Iran", frame == "birthplace_first_generation",
         unstable_owner_cell == FALSE, industry_group %in% names(ind_labels)) %>%
  transmute(industry = ind_labels[industry_group],
            share_pct = round(share_of_origin_business_owners_pct, 1),
            unweighted_n = business_owner_unweighted_n) %>%
  arrange(desc(share_pct))
write_csv(ind, file.path(OUT, "us_business_industry.csv"))

# ---- 3. Owner residence by metro area ---------------------------------------
geo <- read_csv(file.path(SRC, "us_iranian_business_owner_extension",
                          "iranian_business_owner_geography.csv"), show_col_types = FALSE) %>%
  filter(origin == "Iran", frame == "birthplace_first_generation") %>%
  group_by(region = destination_region) %>%
  summarise(owners_weighted = round(sum(business_owner_weighted_n)),
            unweighted_n = sum(business_owner_unweighted_n), .groups = "drop") %>%
  mutate(share_pct = round(100 * owners_weighted / sum(owners_weighted), 1)) %>%
  filter(share_pct >= 0.5) %>%              # drop trace metros (Denver n=2)
  arrange(desc(owners_weighted))
write_csv(geo, file.path(OUT, "us_business_geography.csv"))

cat("Wrote 3 CSVs to", OUT, "\n")
cat("  rate:", nrow(rate), "origins; Iran =",
    rate$rate_pct[rate$is_iran], "%\n")
cat("  industry:", nrow(ind), "sectors; top =", ind$industry[1],
    paste0("(", ind$share_pct[1], "%)"), "\n")
cat("  geography:", nrow(geo), "metros; LA/OC share =",
    geo$share_pct[grepl("Los Angeles", geo$region)], "%\n")

# generate_income_deciles_hhinc.R
# Produces: output_work_page/income_distribution_FINAL.csv
#
# Computes custom household pre-tax income deciles for Iranian-Canadians
# using the same methodology as the US dashboard:
#   1. Take ALL Canadians ages 25-54 from the PUMF
#   2. Compute national decile thresholds from hhinc (pre-tax household income)
#   3. Place Iranian-Canadians into those decile bins
#   4. Report what % of Iranians fall in each decile (10% = national baseline)
#
# This makes the Canada income chart directly comparable to the US income chart.

library(tidyverse)
library(here)
source(here("R/comprehensive_iranian_variables_correct_pumf.R"))

cat("=== Generating income_distribution_FINAL.csv (household pre-tax deciles) ===\n")

# --- Step 1: Load full PUMF with hhinc, weight, agegrp, and Iranian identifiers ---
vars <- c("pob", "ethder", "mtnno", "hlmostno", "weight", "genstat",
           "agegrp", "hhinc")
cat("Loading full PUMF (all 980K records)...\n")
all_data <- load_individual_pumf(vars)

# Filter to ages 25-54 with valid hhinc
all_25_54 <- all_data %>%
  filter(agegrp >= 9, agegrp <= 14, hhinc < 88)

cat("National population ages 25-54 with valid hhinc:", nrow(all_25_54), "records\n")

# --- Step 2: Map hhinc codes to midpoint dollar values ---
hhinc_midpoints <- c(
  `1` = 1000, `2` = 3500, `3` = 6000, `4` = 8500, `5` = 11000,
  `6` = 13500, `7` = 16000, `8` = 18500, `9` = 22500, `10` = 27500,
  `11` = 32500, `12` = 37500, `13` = 42500, `14` = 47500, `15` = 52500,
  `16` = 57500, `17` = 62500, `18` = 67500, `19` = 72500, `20` = 77500,
  `21` = 82500, `22` = 87500, `23` = 92500, `24` = 97500, `25` = 105000,
  `26` = 115000, `27` = 125000, `28` = 135000, `29` = 145000, `30` = 162500,
  `31` = 187500, `32` = 225000, `33` = 275000
)

all_25_54$hhinc_dollars <- hhinc_midpoints[as.character(all_25_54$hhinc)]

# --- Step 3: Compute national decile thresholds (weighted) ---
# Use Hmisc::wtd.quantile or manual approach
national_sorted <- all_25_54 %>%
  arrange(hhinc_dollars) %>%
  mutate(cum_weight = cumsum(weight),
         cum_pct = cum_weight / sum(weight))

total_weight <- sum(all_25_54$weight)
decile_breaks <- c(-Inf)
for (d in 1:9) {
  threshold_pct <- d / 10
  # Find the hhinc_dollars value where cumulative weight crosses this threshold
  row_idx <- which(national_sorted$cum_pct >= threshold_pct)[1]
  decile_breaks <- c(decile_breaks, national_sorted$hhinc_dollars[row_idx])
}
decile_breaks <- c(decile_breaks, Inf)

cat("\nNational decile thresholds (household pre-tax income, ages 25-54):\n")
for (i in 1:10) {
  cat(sprintf("  D%d: $%s to $%s\n", i,
    ifelse(i == 1, "0", format(decile_breaks[i], big.mark = ",")),
    ifelse(i == 10, "250,000+", format(decile_breaks[i+1], big.mark = ","))))
}

# --- Step 4: Identify Iranians and place into deciles ---
iranians_25_54 <- all_25_54 %>%
  filter(pob == 19 | ethder == 38 | mtnno == 11 | hlmostno == 15)

cat("\nIranians ages 25-54 with valid hhinc:", nrow(iranians_25_54), "records\n")

iranians_25_54$decile <- cut(iranians_25_54$hhinc_dollars,
  breaks = decile_breaks, labels = paste0("Decile ", 1:10), include.lowest = TRUE)

# --- Step 5: Compute shares by generation ---
decile_labels <- paste0("Decile ", 1:10)

# First generation
first_gen <- iranians_25_54 %>%
  filter(genstat == 1) %>%
  group_by(decile) %>%
  summarize(households = sum(weight), .groups = "drop") %>%
  mutate(percentage = round(households / sum(households) * 100, 1),
         generation = "First",
         income_category = as.character(decile)) %>%
  select(generation, income_category, households, percentage)

# Second generation (quintiles due to small sample)
second_gen <- iranians_25_54 %>%
  filter(genstat %in% c(2, 3))

cat("Second gen count:", nrow(second_gen), "\n")

if (nrow(second_gen) >= 50) {
  second_gen$quintile <- cut(second_gen$hhinc_dollars,
    breaks = decile_breaks[c(1, 3, 5, 7, 9, 11)],
    labels = c("Bottom 20%", "2nd Quintile", "3rd Quintile", "4th Quintile", "Top 20%"),
    include.lowest = TRUE)

  second_gen_out <- second_gen %>%
    group_by(quintile) %>%
    summarize(households = sum(weight), .groups = "drop") %>%
    mutate(percentage = round(households / sum(households) * 100, 1),
           generation = "Second",
           income_category = as.character(quintile)) %>%
    select(generation, income_category, households, percentage)
} else {
  cat("WARNING: Second gen sample too small for income analysis\n")
  second_gen_out <- data.frame()
}

result <- bind_rows(first_gen, second_gen_out)

cat("\n=== First Generation Income Deciles ===\n")
print(first_gen %>% select(income_category, percentage))

if (nrow(second_gen_out) > 0) {
  cat("\n=== Second Generation Income Quintiles ===\n")
  print(second_gen_out %>% select(income_category, percentage))
}

write_csv(result, here("output_work_page/income_distribution_FINAL.csv"))
cat("\nSaved income_distribution_FINAL.csv\n")

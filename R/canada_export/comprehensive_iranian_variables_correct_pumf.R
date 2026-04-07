# Comprehensive Iranian-Canadian Variables - Individual PUMF (Correct Path)
# All variables from the actual Individual PUMF dataset 
# Source: IDD_Canada/data/pumf_individual/sub-stata.txt (symlink to _data/canada/pumf_individual/)
# Created: June 26, 2025

library(tidyverse)
library(readr)
library(here)

# Complete variable specification from actual Individual PUMF
create_pumf_variable_spec <- function() {
  
  # All variables with exact column positions from Individual PUMF
  variable_spec <- tribble(
    ~variable_name, ~start_col, ~end_col, ~col_type, ~category, ~description,
    
    # CORE IDENTIFICATION & DEMOGRAPHICS
    "agegrp", 1, 2, "double", "Demographics", "Age groups (21 categories: 0-4, 5-6, etc.)",
    "marsth", 4, 4, "double", "Demographics", "Marital status (6 categories)",
    "gender", 6, 6, "double", "Demographics", "Gender (1=Woman+, 2=Man+)",
    
    # MOBILITY
    "mob1", 8, 8, "double", "Geography", "Mobility 1 year ago",
    "mob5", 10, 10, "double", "Geography", "Mobility 5 years ago",
    "pr1", 12, 13, "double", "Geography", "Province 1 year ago",
    "pr5", 15, 16, "double", "Geography", "Province 5 years ago",
    
    # INDIGENOUS IDENTITY
    "aboid", 18, 18, "double", "Indigenous", "Aboriginal identity",
    "bfnmemb", 20, 20, "double", "Indigenous", "First Nation membership",
    "regind", 22, 22, "double", "Indigenous", "Registered Indian status",
    
    # ETHNIC & POPULATION IDENTITY (KEY FOR IRANIANS)
    "dpgrsum", 24, 25, "double", "Identity", "Population group (9=West Asian)",
    "ethder", 27, 28, "double", "Identity", "Ethnic origin (38=Iranian, 59 categories)",
    "vismin", 30, 31, "double", "Identity", "Visible minority (9=West Asian)",
    "relig", 33, 34, "double", "Religion", "Religion (22 categories)",
    
    # LANGUAGE VARIABLES (KEY FOR IRANIANS)
    "li_elig_oml_u18", 36, 36, "double", "Language", "Minority language eligibility",
    "liprogtype", 38, 38, "double", "Language", "French program type",
    "fol", 40, 40, "double", "Language", "First official language",
    "hlmosten", 42, 42, "double", "Language", "English at home",
    "hlmostfr", 44, 44, "double", "Language", "French at home",
    "hlmostno", 46, 47, "double", "Language", "Non-official language at home (15=Persian)",
    "hlregen", 49, 49, "double", "Language", "Other English at home",
    "hlregfr", 51, 51, "double", "Language", "Other French at home", 
    "hlregno", 53, 53, "double", "Language", "Other non-official at home",
    "kol", 55, 55, "double", "Language", "Knowledge of official languages",
    "lwmosten", 57, 57, "double", "Language", "English at work",
    "lwmostfr", 59, 59, "double", "Language", "French at work",
    "lwmostno", 61, 61, "double", "Language", "Non-official at work",
    "lwregen", 63, 63, "double", "Language", "Other English at work",
    "lwregfr", 65, 65, "double", "Language", "Other French at work",
    "lwregno", 67, 67, "double", "Language", "Other non-official at work",
    "mtnen", 69, 69, "double", "Language", "English mother tongue",
    "mtnfr", 71, 71, "double", "Language", "French mother tongue",
    "mtnno", 73, 74, "double", "Language", "Non-official mother tongue (11=Iranian Persian)",
    "nol", 76, 77, "double", "Language", "Knowledge of non-official languages",
    
    # IMMIGRATION VARIABLES (KEY FOR IRANIANS)
    "ageimm", 79, 80, "double", "Immigration", "Age at immigration",
    "citizen", 82, 82, "double", "Immigration", "Citizenship status",
    "citoth", 84, 85, "double", "Immigration", "Other country of citizenship",
    "genstat", 87, 87, "double", "Immigration", "Generation status (1st/2nd/3rd+)",
    "immcat5", 89, 90, "double", "Immigration", "Immigration category",
    "immstat", 92, 93, "double", "Immigration", "Immigration status",
    "pob", 95, 96, "double", "Immigration", "Place of birth (19=Iran)",
    "pobpar1", 98, 98, "double", "Immigration", "Parent 1 place of birth",
    "pobpar2", 100, 100, "double", "Immigration", "Parent 2 place of birth",
    "yrim", 102, 105, "double", "Immigration", "Year of immigration",
    
    # EDUCATION VARIABLES
    "attsch", 107, 107, "double", "Education", "School attendance",
    "cip2021", 109, 110, "double", "Education", "Field of study (12 categories)",
    "cip2021_stem_sum", 112, 113, "double", "Education", "STEM vs non-STEM",
    "hdgree", 115, 116, "double", "Education", "Highest degree (13 levels)",
    "loc_st_res", 118, 118, "double", "Education", "Study location vs residence",
    "locstud", 120, 121, "double", "Education", "Location of study",
    "ssgrad", 123, 124, "double", "Education", "Secondary school completion",
    
    # EMPLOYMENT VARIABLES
    "jobperm", 126, 126, "double", "Employment", "Job permanency",
    "cow", 128, 128, "double", "Employment", "Class of worker",
    "fptwk", 130, 130, "double", "Employment", "Full-time vs part-time",
    "lfact", 132, 133, "double", "Employment", "Labour force status",
    "lstwrk", 135, 135, "double", "Employment", "When last worked",
    "naics", 137, 139, "double", "Employment", "Industry (19 sectors)",
    "noc21", 141, 142, "double", "Employment", "Occupation (26 major groups)",
    "wkswrk", 144, 144, "double", "Employment", "Weeks worked in 2020",
    "wrkact", 146, 147, "double", "Employment", "Work activity",
    
    # COMMUTING VARIABLES
    "dist", 149, 149, "double", "Commuting", "Distance home to work",
    "mode", 151, 151, "double", "Commuting", "Main commuting mode",
    "powst", 153, 153, "double", "Commuting", "Place of work status",
    "pwdur", 155, 155, "double", "Commuting", "Commuting duration",
    "pwleave", 157, 157, "double", "Commuting", "Time leaving for work",
    "pwocc", 159, 159, "double", "Commuting", "Vehicle occupancy",
    "pwpr", 161, 162, "double", "Commuting", "Place of work province",
    
    # INCOME VARIABLES
    "capgn", 164, 171, "double", "Income", "Net capital gains/losses",
    "cfinc", 173, 174, "double", "Income", "Census family income categories",
    "cfinc_at", 176, 177, "double", "Income", "Census family after-tax income",
    "chdbn", 179, 186, "double", "Income", "Child benefits",
    "chldc", 188, 195, "double", "Income", "Child care expenses",
    "cqppb", 197, 204, "double", "Income", "CPP/QPP benefits",
    "efdecile", 206, 207, "double", "Income", "Economic family income decile",
    "efdimbm_2018", 209, 210, "double", "Income", "Economic family MBM status",
    "efinc", 212, 213, "double", "Income", "Economic family income",
    "efinc_at", 215, 216, "double", "Income", "Economic family after-tax income",
    "eicbn", 218, 225, "double", "Income", "Employment Insurance benefits",
    "empin", 227, 234, "double", "Income", "Employment income",
    "govti", 236, 243, "double", "Income", "Other government income",
    "gtrfs", 245, 252, "double", "Income", "Government transfers",
    "hhinc", 254, 255, "double", "Income", "Household income categories",
    "hhinc_at", 257, 258, "double", "Income", "Household after-tax income",
    "hhmrkinc", 260, 261, "double", "Income", "Household market income",
    "inctax", 263, 270, "double", "Income", "Income taxes",
    "invst", 272, 279, "double", "Income", "Investment income",
    "lico_at", 281, 281, "double", "Income", "LICO after-tax status",
    "lico_bt", 283, 283, "double", "Income", "LICO before-tax status",
    "lolima", 285, 285, "double", "Income", "LIM after-tax status",
    "lolimb", 287, 287, "double", "Income", "LIM before-tax status",
    "lombm_2018", 289, 289, "double", "Income", "MBM poverty status",
    "mrkinc", 291, 298, "double", "Income", "Market income",
    "oasgi", 300, 307, "double", "Income", "OAS/GIS benefits",
    "otinc", 309, 316, "double", "Income", "Other market income",
    "retir", 318, 325, "double", "Income", "Private retirement income",
    "sempi", 327, 334, "double", "Income", "Net self-employment income",
    "totinc", 336, 343, "double", "Income", "Total income",
    "totinc_at", 345, 352, "double", "Income", "After-tax income",
    "wages", 354, 361, "double", "Income", "Wages and salaries",
    "covid_erb", 363, 370, "double", "Income", "COVID emergency benefits",
    
    # FAMILY VARIABLES
    "cfsize", 372, 372, "double", "Family", "Census family size",
    "cfstat", 374, 374, "double", "Family", "Household living arrangements",
    "efsize", 376, 376, "double", "Family", "Economic family size",
    "pkid0_1", 378, 378, "double", "Family", "Children 0-1 in family",
    "pkid15_24", 380, 380, "double", "Family", "Children 15-24 in family",
    "pkid2_5", 382, 382, "double", "Family", "Children 2-5 in family",
    "pkid25", 384, 384, "double", "Family", "Children 25+ in family",
    "pkid6_14", 386, 386, "double", "Family", "Children 6-14 in family",
    "pkids", 388, 388, "double", "Family", "Any children in family",
    "hhsize", 390, 390, "double", "Family", "Household size",
    "hhtype", 392, 393, "double", "Family", "Household type",
    "prihm", 395, 395, "double", "Family", "Primary household maintainer",
    
    # HOUSING VARIABLES
    "bedrm", 397, 397, "double", "Housing", "Number of bedrooms",
    "condo", 399, 399, "double", "Housing", "Condominium status",
    "dtype", 401, 401, "double", "Housing", "Dwelling type",
    "hcoreneed_ind", 403, 405, "double", "Housing", "Housing core need",
    "nos", 407, 407, "double", "Housing", "Housing suitability",
    "presmortg", 409, 409, "double", "Housing", "Presence of mortgage",
    "repair", 411, 411, "double", "Housing", "Dwelling condition",
    "room", 413, 414, "double", "Housing", "Number of rooms",
    "shelco", 416, 419, "double", "Housing", "Shelter cost",
    "subsidy", 421, 421, "double", "Housing", "Subsidized housing",
    "tenur", 423, 423, "double", "Housing", "Tenure (owner/renter)",
    "value", 425, 432, "double", "Housing", "Owner estimated value",
    
    # GEOGRAPHIC VARIABLES
    "cma", 434, 436, "double", "Geography", "Census metropolitan area",
    "pr", 438, 439, "double", "Geography", "Current province/territory",
    
    # TECHNICAL VARIABLES
    "ppsort", 441, 446, "double", "Technical", "Unique record identifier",
    "weight", 448, 463, "double", "Technical", "Individual weighting factor",
    "wt1", 465, 481, "double", "Technical", "Replicate weight 1",
    "wt2", 483, 499, "double", "Technical", "Replicate weight 2",
    "wt3", 501, 517, "double", "Technical", "Replicate weight 3",
    "wt4", 519, 535, "double", "Technical", "Replicate weight 4",
    "wt5", 537, 553, "double", "Technical", "Replicate weight 5",
    "wt6", 555, 571, "double", "Technical", "Replicate weight 6",
    "wt7", 573, 589, "double", "Technical", "Replicate weight 7",
    "wt8", 591, 607, "double", "Technical", "Replicate weight 8",
    "wt9", 609, 625, "double", "Technical", "Replicate weight 9",
    "wt10", 627, 643, "double", "Technical", "Replicate weight 10",
    "wt11", 645, 661, "double", "Technical", "Replicate weight 11",
    "wt12", 663, 679, "double", "Technical", "Replicate weight 12",
    "wt13", 681, 697, "double", "Technical", "Replicate weight 13",
    "wt14", 699, 715, "double", "Technical", "Replicate weight 14",
    "wt15", 717, 733, "double", "Technical", "Replicate weight 15",
    "wt16", 735, 751, "double", "Technical", "Replicate weight 16"
  )
  
  return(variable_spec)
}

# Key Iranian identification variables
iranian_core_vars <- function() {
  c("agegrp", "gender", "ethder", "pob", "mtnno", "hlmostno", "vismin", 
    "dpgrsum", "genstat", "immstat", "yrim", "ageimm", "weight")
}

# Socioeconomic analysis variables
socioeconomic_vars <- function() {
  c("agegrp", "gender", "ethder", "pob", "genstat", "hdgree", "cip2021",
    "lfact", "noc21", "naics", "totinc", "totinc_at", "empin", "cow",
    "marsth", "cfstat", "tenur", "pr", "cma", "weight")
}

# Family analysis variables (with potential spouse ethnicity from linkage)
family_vars <- function() {
  c("agegrp", "gender", "ethder", "pob", "genstat", "marsth", "cfstat", 
    "cfsize", "hhtype", "hhsize", "pkids", "pkid0_1", "pkid2_5", "pkid6_14", 
    "pkid15_24", "cfinc", "efinc", "weight")
}

# Immigration patterns variables
immigration_vars <- function() {
  c("agegrp", "gender", "ethder", "pob", "pobpar1", "pobpar2", "genstat", 
    "immstat", "yrim", "ageimm", "immcat5", "citizen", "mtnno", "hlmostno", 
    "pr", "cma", "weight")
}

# Load function for the correct Individual PUMF path
load_individual_pumf <- function(variables_subset = NULL) {
  
  # Correct path to Individual PUMF data
  individual_path <- here("data/pumf_individual/sub-data.txt")
  
  # Get variable specifications
  var_spec <- create_pumf_variable_spec()
  
  # Filter to subset if specified
  if (!is.null(variables_subset)) {
    var_spec <- var_spec %>% filter(variable_name %in% variables_subset)
  }
  
  # Create column positions
  col_positions <- fwf_positions(
    start = var_spec$start_col,
    end = var_spec$end_col,
    col_names = var_spec$variable_name
  )
  
  # Create column types (all double for numeric data)
  col_types <- setNames(rep("d", nrow(var_spec)), var_spec$variable_name)
  col_types <- do.call(cols, as.list(col_types))
  
  # Read data
  message("Loading Individual PUMF data...")
  message("Variables included: ", nrow(var_spec))
  message("Data path: ", individual_path)
  
  data <- read_fwf(
    individual_path,
    col_positions = col_positions,
    col_types = col_types
  )
  
  message("Loaded ", nrow(data), " records")
  return(data)
}

# Usage summary
cat("=== INDIVIDUAL PUMF COMPREHENSIVE VARIABLES ===\n")
spec <- create_pumf_variable_spec()
cat("Total variables available:", nrow(spec), "\n\n")

cat("Variable categories:\n")
var_counts <- spec %>% count(category, sort = TRUE)
print(var_counts)

cat("\n=== KEY VARIABLE SETS ===\n")
cat("Iranian identification:", length(iranian_core_vars()), "variables\n")
cat("Socioeconomic analysis:", length(socioeconomic_vars()), "variables\n") 
cat("Family analysis:", length(family_vars()), "variables\n")
cat("Immigration patterns:", length(immigration_vars()), "variables\n")

cat("\n=== USAGE EXAMPLES ===\n")
cat("# Load all Iranian identification variables:\n")
cat("data <- load_individual_pumf(iranian_core_vars())\n\n")
cat("# Load comprehensive socioeconomic dataset:\n") 
cat("data <- load_individual_pumf(socioeconomic_vars())\n\n")
cat("# Load all variables:\n")
cat("data <- load_individual_pumf()\n")
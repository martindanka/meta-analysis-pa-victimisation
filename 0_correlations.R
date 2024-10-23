
# Recommended: Restore the project environment (install correct package versions)
## Run renv::restore() after importing the project from Git for the first time.
## Hit Y when prompted to locally install the correct package versions.
## This will install these packages specifically into the project folder.

# Packages --------------------------------------------------------------------------------------------------------
library(haven)
library(jtools)
library(survey)
library(tidyverse)

# Easterlin et al. ------------------------------------------------------------------------------------------------

# Note: AddHealth public use datasets can be obtained at https://addhealth.cpc.unc.edu/data/. 

# Import the AddHealth public use datasets
w1_addhealth_data <- read_sav("data/easterlin2019/w1inhome_dvn.sav")
w4_addhealth_data <- read_sav("data/easterlin2019/w4inhome_dvn.sav")
w4_addhealth_weights <- read_sav("data/easterlin2019/w4weight.sav")

# Select variables

## W1
w1_addhealth_data_sel <- w1_addhealth_data |>
  select(AID, PA62, PA10) |>
  rename(ace_alc_abuse = PA62,
         ace_single_parent = PA10)

# W4
w4_addhealth_data_sel <- w4_addhealth_data |>
  select(AID, H4ID5H, H4ID5J, H4MH18:H4MH28, H4MA1, H4MA3, H4MA5, H4WP3, H4WP9, H4WP30, H4WP16) |>
  rename(
    depression = H4ID5H, 
    anxiety = H4ID5J,
    ace_emotional = H4MA1,
    ace_physical = H4MA3,
    ace_sexual = H4MA5,
    ace_incarc_mother_bio = H4WP3,
    ace_incarc_mother_fig = H4WP16,
    ace_incarc_father_bio = H4WP9,
    ace_incarc_father_fig = H4WP30
  )

# Weights
weights <- w4_addhealth_weights |>
  # unclear which weights were used by Easterlin et al.
  # longitudinal weights for W1 -> W4 analyses used
  select(AID, GSWGT4_2) |>
  rename(weights = GSWGT4_2)

# Merge the three datasets 
addhealth_data <- w1_addhealth_data_sel |>
  left_join(w4_addhealth_data_sel, by = "AID") |>
  left_join(weights, by = "AID")
  
# Data cleaning

## Abuse
addhealth_data <- addhealth_data |>
  mutate(
    # Emotional abuse present if occurred more than ten times
    ace_emotional_bin = case_when(
      ace_emotional == 5 ~ 1,
      ace_emotional %in% c(1:4, 6) ~ 0,
      .default = NA
    ), 
    # Physical abuse present if occurred more than twice 
    ace_physical_bin = case_when(
      ace_physical %in% c(3:5) ~ 1,
      ace_physical %in% c(1, 2, 6) ~ 0,
      .default = NA
    ),
    # Sexual abuse if ever occurred 
    ace_sexual_bin = case_when(
      ace_sexual %in% c(1:5) ~ 1,
      ace_sexual == 6 ~ 0,
      .default = NA
    ),
    # Alcohol abuse if parent had five or more drinks on one or more occassions
    ace_alc_abuse_bin = case_when(
      ace_alc_abuse %in% c(2:6) ~ 1,
      ace_alc_abuse == 1 ~ 0,
      .default = NA
      )
    )

## Single parents and incarceration
addhealth_data <- addhealth_data |>
  mutate(
    # Living with a single parent (single/widowed/divorced/separated)
    ace_single_parent_bin = case_when(
      ace_single_parent %in% c(1, 3, 4, 5) ~ 1,
      ace_single_parent == 2 ~ 0,
      .default = NA
    ),
    # Incarceration (bio father/mother or mother/father figure ever in jail)
    ace_incarc_bin = case_when(
      (ace_incarc_mother_bio == 1 
      | ace_incarc_mother_fig == 1
      | ace_incarc_father_bio == 1 
      |  ace_incarc_father_fig == 1) 
      ~ 1,
      .default = 0
    )
  )

## Deriving ACEs
addhealth_data_aces <- addhealth_data %>%
  # Remove all the helper variables
  select(!starts_with("ace") | ends_with("bin")) %>%
  mutate(
    aces_total = case_when(
      # If all ACE variables are 0, make ace_total = 0
      if_all(starts_with("ace"), ~ .x == 0) ~ 0, 
      # If ACEs present and no NAs, make ace_total = 1
      if_all(starts_with("ace"), ~ !is.na(.x)) & if_any(starts_with("ace"), ~ .x == 1) ~ 1,  
      # Anything else is NA
      TRUE ~ NA  # If none of the conditions above are met, make it NA
    )
  )

### Note: If any of the ACE variables are NAs, ace_total will automatically also be NA.
### This means that if a person experienced some ACEs, but information on other ACEs are missing,
### the final presence/absence of ACEs will still be coded as missing.
### These participants will later be excluded from the dataset.
### Although this approach results in losing some participants whose ACE status is known, 
### we believe it mirrors the approach used by Easterlin et al., where ACEs were summed first,
### a process requiring all the ACE variables being complete. 

# Check if ACEs proportion similar to that reported by Easterlin et al. (49%)
prop.table(table(addhealth_data_aces$aces_total))

# CES-D score

## Recode CES-D items
addhealth_data_cesd <- addhealth_data_aces |>
  mutate(
    ## Reverse code CES-D items and set other values to NAs
    across(
      c(H4MH20, H4MH24, H4MH25),
      ~case_when(
        .x == 0 ~ 3,
        .x == 1 ~ 2, 
        .x == 2 ~ 1, 
        .x == 3 ~ 0,
        .default = NA
      )
    ),
    across(
      ## Set other values to NAs
      c(H4MH18:H4MH28),
      ~case_when(
        .x == 0 | .x == 1 | .x == 2 | .x == 3 ~ .x,
        .default = NA
      )
    )
  )

## Derive CES-D score
addhealth_data_cesd <- addhealth_data_cesd |>
  rowwise() |>
  mutate(
    cesd_total = sum(c_across(H4MH18:H4MH28))
  ) |>
  ungroup() |>
  ## Create a binary CES-D variable
  mutate(
    cesd_total_bin = case_when(
      cesd_total >= 10 ~ 1,
      cesd_total < 10 ~ 0,
      .default = cesd_total
    )
  )

table(addhealth_data_cesd$cesd_total)
prop.table(table(addhealth_data_cesd$cesd_total_bin))

## Depression and anxiety
addhealth_data_clean <- addhealth_data_cesd |>
  mutate(
    across(
      c(depression, anxiety),
      ~case_when(
        .x == 1 | .x == 0 ~ .x,
        .default = NA
      )
    )
  )

table(addhealth_data_clean$depression)
table(addhealth_data_clean$anxiety)

prop.table(table(addhealth_data_clean$depression, addhealth_data_clean$anxiety), 1)
prop.table(table(addhealth_data_clean$depression, addhealth_data_clean$cesd_total_bin), 1)

# Apply eligibility criteria

addhealth_data_clean <- addhealth_data_clean |>
  ## Remove those without a valid survey weight
  filter(!is.na(weights))

## Define a survey object 
addhealth_design <- svydesign(id=~1, weights = ~weights, data = addhealth_data_clean) 

## Subset those who were eligible
addhealth_eligible <- subset(addhealth_design, is.na(aces_total) == F & aces_total == 1)

# Obtain the correlations
svycor(~cesd_total_bin + anxiety + depression, design = addhealth_eligible, na.rm = T)

# cesd_total_bin anxiety depression
# cesd_total_bin           1.00    0.19       0.25
# anxiety                  0.19    1.00       0.47
# depression               0.25    0.47       1.00

# Sibold2015 ------------------------------------------------------------------------------------------------------

# Note: Data from YRBS can be obtained at https://www.cdc.gov/yrbs/data/national-yrbs-datasets-documentation.html 

# Data
yrbs2013 <- read_sav("data/sibold2015/yrbs2013.sav")

yrbs2013_clean <- yrbs2013 |>
  select(psu, weight, stratum, Q24, Q25, Q26, Q27) |>
  mutate(
    bullied_school = case_when(
      Q24 == 1 ~ 1,
      Q24 == 2 ~ 0,
      .default = Q24
    ),
    bullied_electronically = case_when(
      Q25 == 1 ~ 1,
      Q25 == 2 ~ 0,
      .default = Q25
    ),
    bullied = case_when(
      Q24 == 1 | Q25 == 1 ~ 1,
      Q24 == 2 & Q25 == 2 ~ 0,
      .default = NA
    ),
    sadness = case_when(
      Q26 == 1 ~ 1, 
      Q26 == 2 ~ 0,
      .default = Q26
      ),
    suic_ideation = case_when(
      Q27 == 1 ~ 1,
      Q27 == 2 ~ 0,
      .default = Q27
    )
  ) |>
  select(-c(Q24, Q25, Q26, Q27))

# Create the YRBS design object 
yrbs_design <- svydesign(id=~psu, weight=~weight, strata=~stratum,data=yrbs2013_clean, nest=TRUE) 

# Check if the percentage of bullied is the same as reported in the paper
bullied <- svyciprop(~I(bullied==1), yrbs_design, na.rm=TRUE, method = "xlogit")
bullied

# Obtain correlation for the whole sample
svycor(~sadness + suic_ideation, design = yrbs_design, na.rm = T)

# Obtain correlation for those who were bullied
yrbs_bullied <- subset(yrbs_design, bullied == 1)
svycor(~sadness + suic_ideation, design = yrbs_bullied, na.rm = T)

# r = 0.46.

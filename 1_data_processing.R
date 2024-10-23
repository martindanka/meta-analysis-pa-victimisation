
# Packages --------------------------------------------------------------------------------------------------------
library(esc)
library(metafor)
library(tidyverse)

# Data cleaning  --------------------------------------------------------------------------------------------------

# Create the dataset of studies that reported ORs
data_raw <- tibble(author = c(rep("Afifi2016", 2), rep("Bjereld2014", 2), rep("Cheung2018", 4), "Dube2017", 
                              rep("Easterlin2019", 3), rep("Sibold2015", 6)),
                   or = c(1.47, 1.93, 1.68, 1.01, 1.7, 1.4, 1.9, 1.3, 2.1, 0.85, 0.76, 0.70, 0.95, 0.71, 0.68, 1.07, 
                          0.77, 0.77),
                   ci_low = c(1.11, 1.43, 1.02, 0.59, 1.0, 0.8, 1.1, 0.8, 1.6, 0.71, 0.59, 0.56, 0.69, 0.53, 0.53, 0.78,
                              0.60, 0.59),
                   ci_high = c(1.95, 2.59, 2.77, 1.74, 2.8, 2.4, 3.2, 2.2, 2.7, 1.01, 0.97, 0.89, 1.32, 0.96, 0.88, 
                               1.45, 1.00, 1.02), 
                   effect = c("moderate_mh", "good_mh", "boys", "girls", "summer_good_mh", "summer_moderate_mh", 
                              "winter_good_mh", "winter_moderate_mh", "", "CES-D", "depression", "anxiety", "sadness1", 
                              "sadness2", "sadness3", "ideation1", "ideation2", "ideation3")
                   )

data_raw 

# Reverse coding ORs (poor mental health and equivalents as the reference)

data_recoded <- data_raw |>
  # Reverse ORs where needed
  mutate(
    across(
      c(or, ci_low, ci_high), 
      ~case_when(
        author == "Sibold2015" | author == "Easterlin2019" ~ 1/.x,
        .default = .x
      )
    ),
    # Swap upper and lower CI values where ORs were reverse-coded
    low = ci_low,
    high = ci_high,
    ci_low = case_when(author == "Sibold2015" | author == "Easterlin2019" ~ high, .default = ci_low),
    ci_high = case_when(author == "Sibold2015" | author == "Easterlin2019" ~ low, .default = ci_high)
  ) |>
  select(-c(low, high))

data_recoded 

# Obtain logORs and convert CIs to variance
data_transformed <- conv.wald(out = or, ci.lb = ci_low, ci.ub = ci_high, data = data_recoded, transf = log)

data_transformed

# Drop ORs and CIs (keep logORs and variance)
data_processed <- data_transformed |>
  select(-c(or, ci_low, ci_high))

data_processed

# Conversion of M and SDs in Aas et al. (2021) to logOR using the Hasselblad & Hedges formula
esc_mean_sd(grp1m = 27.0, grp1sd = 16.0, grp1n = 22, grp2m = 17.9, grp2sd = 11.4, grp2n = 40, es.type = "logit")

# Add Aas et al. (2021) to the dataset
data_processed <- data_processed |>
  add_row(author = "Aas2021", effect = "", yi =  1.2510, vi = 0.2444, .before = 1)


# Aggregating estimates  ------------------------------------------------------------------------------------------

# Aggregating independent effect sizes

## When different levels of PA included as dummies in the same model,
## the effect sizes were assumed to be uncorrelated.
sibold_pa_pooled <- data_processed |>
  filter(author == "Sibold2015") |>
  mutate(effect = c(rep("sadness", 3), rep("ideation", 3))) |>
  metafor::aggregate.escalc(x = _, cluster = effect, rho = 0)

data_processed_indep_pooled <- data_processed |>
  filter(author != "Sibold2015") |>
  bind_rows(sibold_pa_pooled)

# Aggregating dependent effect sizes

## When aggregating correlated effect sizes with unknown correlation, a correlation of 0.5 was assumed. 
aggregate_data <- data_processed_indep_pooled |>
  mutate(
    esid = c(1, 1:2, 1:2, 1:4, 1, 1:3, 1:2),
    r1 = case_when(
      esid == 1 ~ 1,
      esid == 2 | esid == 3 |esid == 4 ~ 0.5,
      .default = NA
      ),
    r2 = case_when(
      esid == 2 ~ 1,
      esid %in% c(2, 3, 4) ~ 0.5,
      .default = NA
      ),
    r3 = case_when(
      esid == 3 ~ 1,
      esid == 4 ~ 0.5,
      .default = NA
      ),
    r4 = case_when(
      esid == 4 ~ 1,
      .default = NA
      )
    )

## Fill correlations from the data
aggregate_data <- aggregate_data |>
  mutate(
    r2 = case_when(
      author == "Easterlin2019" & effect == "anxiety" ~ 0.47,
      .default = r2
      ),
    r1 = case_when(
      author == "Easterlin2019" & effect == "depression" ~ 0.25,
      author == "Easterlin2019" & effect == "anxiety" ~ 0.19,
      author == "Sibold2015" & effect == "ideation" ~ 0.46,
      .default = r1
    )
  )

aggregate_data

# Derive variance-covariance matrix
V <- vcalc(vi, cluster = author, rvars = c(r1:r4), data = aggregate_data)

# Aggregate using the intercorrelations between effect sizes stored in the vcov matrix.
final_dataset <- aggregate_data |>
  select(-c(effect, esid, r1:r4)) |>
  aggregate.escalc(x = _, yi = yi, vi = vi, cluster = author, V = V) |>
  mutate(sei = sqrt(vi))

final_dataset

# Save final dataset
# saveRDS(final_dataset, file = "final_dataset.rds")

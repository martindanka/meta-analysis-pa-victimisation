
# Packages --------------------------------------------------------------------------------------------------------
library(dmetar)
library(meta)
library(metafor)
library(tidyverse)
library(officer)
library(flextable)

# Random-effects meta-analysis ------------------------------------------------------------------------------------
final_dataset <- readRDS(file = "final_dataset.rds")

# Clean labels
## Insert et al. before the year 
final_dataset$author <- gsub("(\\d{4})", " et al., \\1", final_dataset$author)

# Amend Dube to Dube & Rishi and delete the et al in Dube & Rishi only
final_dataset$author <- gsub("Dube et al.", "Dube & Rishi", final_dataset$author)

pooled_meta <- metagen(
  TE = yi,
  seTE = sei,
  studlab = author,
  data = final_dataset,
  sm = "OR",
  fixed = FALSE,
  random = TRUE,
  # use the Paule-Mandel estimator
  method.tau = "PM",
  # apply the Knapp-Hartung adjustment
  method.random.ci = "HK",
  # compute prediction intervals
  prediction = TRUE,
  title = "Effects of physical activity on mental health")

summary(pooled_meta)

# Inspect outliers
find.outliers(pooled_meta)

# Generate a forest plot
meta::forest(
  pooled_meta, 
  # sort by effect size
  sortvar = TE,
  # include the prediction interval
  prediction = TRUE, 
  # color of the prediction interval
  col.predict = "gray",
  # color of the diamond
  col.diamond = "steelblue",
  # suppress tau^2
  print.tau2 = FALSE,
  # specify columns to include to the left and right of the forest plot
  leftcols = c("studlab", "effect", "ci"),
  rightcols = c("w.random"),
  # specify names of the columns
  leftlabs = c("Study", "OR", "95% CI"),
  # label what the effect direction means
  label.right = "Improved mental health",
  label.left = "Worsened mental health",
  # delete forest plot label
  smlab = "",
  # set colour of the points and CIs to black
  col.inside = "black"
  )

# Save forest plot

pdf(file = "forestplot.pdf", width = 8, height = 7)
meta::forest(
  pooled_meta, 
  sortvar = TE,
  prediction = TRUE, 
  col.predict = "gray",
  col.diamond = "steelblue",
  print.tau2 = FALSE,
  leftcols = c("studlab", "effect", "ci"),
  rightcols = c("w.random"),
  leftlabs = c("Study", "OR", "95% CI"),
  label.right = "Improved mental health",
  label.left = "Worsened mental health",
  smlab = "",
  col.inside = "black"
)
dev.off()

png(file = "forestplot.png", width = 2800, height = 2400, res = 300)
meta::forest(
  pooled_meta, 
  sortvar = TE,
  prediction = TRUE, 
  col.predict = "gray",
  col.diamond = "steelblue",
  print.tau2 = FALSE,
  leftcols = c("studlab", "effect", "ci"),
  rightcols = c("w.random"),
  leftlabs = c("Study", "OR", "95% CI"),
  label.right = "Improved mental health",
  label.left = "Worsened mental health",
  smlab = "",
  col.inside = "black"
)
dev.off()

# Publication bias ------------------------------------------------------------------------------------------------

# Funnel plot
funnel(pooled_meta, studlab = TRUE)

# Save funnel plot
# 
# pdf(file = "funnelplot.pdf", width = 8, height = 7)
# funnel(pooled_meta, studlab = TRUE)
# dev.off()
# 
# png(file = "funnelplot.png", width = 2800, height = 2400, res = 300)
# funnel(pooled_meta, studlab = TRUE)
# dev.off()

# Egger test
metabias(pooled_meta, method.bias = "linreg", k.min = 7)
dmetar::eggers.test(pooled_meta)

# Sensitivity analyses --------------------------------------------------------------------------------------------

# Re-run using REML
pooled_meta_REML <- update(pooled_meta, method.tau = "REML")

meta::forest(
  pooled_meta_REML, 
  sortvar = TE,
  prediction = TRUE, 
  # color of the prediction interval
  col.predict = "gray",
  # color of the diamond
  col.diamond = "steelblue",
  print.tau2 = FALSE,
  leftcols = c("studlab", "effect", "ci"),
  rightcols = c("w.random"), 
  leftlabs = c("Study", "OR", "95% CI"),
  label.right = "Improved mental health",
  label.left = "Worsened mental health",
  smlab = "",
  col.inside = "black"
)

# Re-run using Sidik-Jonkman estimator
pooled_meta_SJ <- update(pooled_meta, method.tau = "SJ")

meta::forest(
  pooled_meta_SJ, 
  sortvar = TE,
  prediction = TRUE, 
  # color of the prediction interval
  col.predict = "gray",
  # color of the diamond
  col.diamond = "steelblue",
  print.tau2 = FALSE,
  leftcols = c("studlab", "effect", "ci"),
  rightcols = c("w.random"), 
  leftlabs = c("Study", "OR", "95% CI"),
  label.right = "Improved mental health",
  label.left = "Worsened mental health",
  smlab = "",
  col.inside = "black"
)

# Re-run using fixed effects

## Tau estimators, Knapp-Hartung adjustment, and prediction interval 
## are irrelevant for fixed-effects models, therefore the arguments 
## were deleted so that they are not included.

pooled_meta_fixed <-  update(pooled_meta, fixed = TRUE, random = FALSE, prediction = FALSE)
  

meta::forest(
  pooled_meta_fixed, 
  sortvar = TE,
  prediction = FALSE,
  # color of the diamond
  col.diamond = "steelblue",
  print.tau2 = FALSE,
  leftcols = c("studlab", "effect", "ci"),
  rightcols = c("w.common"),
  leftlabs = c("Study", "OR", "95% CI"),
  label.right = "Improved mental health",
  label.left = "Worsened mental health",
  smlab = "",
  col.inside = "black"
)



# Create table ----------------------------------------------------------------------------------------------------

# library(officer)
# library(flextable)
# 
# pooled_meta_list <- list(pooled_meta, pooled_meta_REML, pooled_meta_SJ)
# sensitivity_table <- tibble(
#   estimator = c("Paule-Mandel", "REML", "Sidik-Jonkman"),
#   OR = round(map_dbl(pooled_meta_list, ~ exp(.x$TE.random)), 2),
#   CI_lower = round(map_dbl(pooled_meta_list, ~exp(.x$lower.random)), 2),
#   CI_upper = round(map_dbl(pooled_meta_list, ~exp(.x$upper.random)), 2),
#   pred_lower = round(map_dbl(pooled_meta_list, ~exp(.x$lower.predict)), 2),
#   pred_upper = round(map_dbl(pooled_meta_list, ~exp(.x$upper.predict)), 2)
# )
# 
# ft <- flextable(sensitivity_table)
# doc <- read_docx()
# doc <- doc %>%
#   body_add_flextable(ft)
# print(doc, target = "sensitivity_table.docx")


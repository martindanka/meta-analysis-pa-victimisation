# Physical activity and common mental health disorders following childhood victimisation
A repository for the systematic review on 'The role of physical activity and sedentary behaviours in the prevention and treatment of common mental disorders following childhood victimisation'

What you'll need to do:
Optional: Install the R version used (R 4.3.1)
1. Import, clone or download the contents of this repository.
2. Open meta-analysis-pa-vict.Rproj in RStudio.
3. Open the scripts (0_correlations.R, 1_data_processing.R, 2_meta-analysis.R)
4. Install the renv package and run renv::restore() in R before going through the rest of the script.

The scripts can be used independently, as all outputs and the meta-analytical dataset needed have been saved as files. These files are imported at the beginning of each script, if required. You should be able to run all the scripts directly, except for the first one (0_correlations.R), as estimating the correlations between effect sizes requires downloading the public datasets from the websites referenced in the file. These correlations are imported directly in the other scripts, so running 0_correlations.R is not a pre-requisite for the other scripts.

Scripts:
- 0_correlations.R: Estimating correlations between multiple eligible outcomes from publicly available datasets. 
- 1_data_processing.R: Data cleaning, effect size conversions, aggregating effect sizes.
- 2_meta-analysis.R: Pooling effect sizes, forest plots, funnel plots, sensitivity analyses

This directory contains the codes we use for IGME meeting

## single country
+ CombinedSvyGLM.R new surveyGLM codes processing the modified stata files and save new estimates to CSV files.
+ Run.R calculates meta estimators and run the INLA model

## multiple country
+ addnational_estimates.R computes national estimates
+ get_final_results.R computes the long table of everything
+ combine_plots.R combines all the map files into two rda files for later use
+ Summary/plot_supplement plots all individual country results
+ Summary/plot_supplement_combined.R plots the joint plots.
    * Need to specify Africa or Asia

The dependency file for the corrected GLM call is
+ ../HelperFunctions/5q0glm2.R
+ CombinedRegionChange.R function for boundary changes, mostly for some preexisting files where region names are not standardized.

## output
The stata files are at directory ../Data/CountryFix (not synced to this repository; the raw files are stored on Google Drive)
The new CSV files are at directory ../Data/5q0_estimates_new/

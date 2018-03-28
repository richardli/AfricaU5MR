# ChildMortality
This is the repository for Child Mortality project. 

## Structure of the repository
_The folders are set up in a slightly weird way due to the long edit history of the original repository_
### CountryMain folder
CountryInfo folder contains some of the country specific information:
+ Filenames of the input .dta files from stata pre-process (this step can now be done in R too, with the SUMMER package)
+ Survey names: names of the survey to be used in final output
+ Region names: **Region names needs to match exactly (1) the names in the .dta files, and (2) the order of the regions in the map file** (See map section below.)

### Data
+ CountryDTARaw: files are too large and require registration with DHS, not uploaded.
+ comparison: UN (B3) and IHME estimates
+ 5q0_estimates_new: country specific direct estimates for each survey
+ map: map files, notice the regions should correspond to regions used in the analysis. The names need not match exactly (as they are usually not), but the order need to be exactly the same as that in the files of the CountryMain/ folder. To check the orders match, I used the code at Main/check_region_names.R, which prints different names so that I can check they are referring to the same region.

### Main
This is the main working folder containing all the codes, results, tables, and figures of the analysis.
+ Figures/Tables/Fitted: as names suggested
+ HIV_Adjustments: factors for HIV adjustments for some countries
+ runscript: scripts I used to run all countries in parallel on a cluster. **The run.R script also demonstrates the order of the several key scripts used.**
+ Codes (main ones)
    * CombinedSvyGLM.R: run surveyGLM to get direct estimates, save to ../Data/5q0_estimates_new/ folder
    * Run.R: fit all models to one country
    * Diag.R: put together a large panel plots of diagnostics of the random effects.
    * Plot.R: generate all the country plots
    * CrossValidation.R: perform cross validation analysis for one country.
    * Plot_all.R: generate all country plots
+ Codes (other)
    * check_region_names.R: check region names are the same in map and data, see map section above.
    * addsurveytable.R: save a table of all the surveys listed.
    * add_country_script.R: to be removed?
    * CombinedRegionChange.R: to combine some regions in several countries (used when calling CombinedSvyGLM.R)

### HelperFunctions.R
This folder contains the codes to fit INLA models. They are now incorporated into the SUMMER package already.


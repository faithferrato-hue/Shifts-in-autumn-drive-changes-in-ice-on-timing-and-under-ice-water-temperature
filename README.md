# Shifts-in-autumn-drive-changes-in-ice-on-timing-and-under-ice-water-temperature
This repository contains the R code for the analysis in the paper:

"Phenological shifts in autumn drive changes in ice-on timing and under-ice water temperature in dimictic Finnish lakes"
by Faith Ferrato¹, Joshua Culpepper¹, Merja Pulkkanen², Raine Kortet²,³ and Sapna Sharma¹

¹ Department of Biology, York University, Toronto, Ontario, Canada
² Finnish Environmental Institute (Syke), Jyväskylä, Finland
³ Department of Environmental and Biological Sciences, University of Eastern Finland, Joensuu, Finland

# Repository Structure
The analysis is organized into 8 R scripts 

# Analysis Scripts
01_ice_on_doy_code.R - Calculate ice-on day of year (DOY) and prepare clean dataset for trend and anomaly/breakpoint analysis

02_under_ice_water_temperature_calculations.R - Clean and calculate under-ice bottom water temperature averages, general statistics, Sen's slope, anomaly and breakpoint analysis

03_combined_climate_variables.R - Clean and combine climate variables associated with under-ice water temperature sites into one dataframe

04_predictor_variables_for_models.R - Combine ice-on, under-ice temperature, morphology, and climate variables to derive fall transition metrics and predictors for regression tree and SEM models

05_regression_tree_model_and_maps.R - Create regression tree model and maps below nodes

06_structural_equation_modeling_under_ice_drivers.R - Explore how fall meteorological and morphological variables influence ice-on timing and under-ice temperature

07_SWT_trend_analysis_and_map.R - Identify specific lakes of interest and average surface water temperature (SWT) for each lake for trend analysis

08_cross_seasonal_SEM_and_correlations.R - Identify lakes of interest, create additional summer variables (e.g., max summer SWT), and combine with previously calculated variables to create cross-seasonal SEM

# Usage
Note: The original data files are not included in this repository due to data sharing agreements
For orgional files contact SYKE. 

# Citation
If you use this code in your research, please cite our paper:
Ferrato, F., Culpepper, J., Pulkkanen, M., Kortet, R., & Sharma, S. (Year). Phenological shifts in autumn drive changes in ice-on timing and under-ice water temperature in dimictic Finnish lakes. Journal Name, Volume(Issue), pages. DOI

# License
This code is made available under the MIT License.

# Contact
For questions about this code or the analysis, please contact Faith Ferrato at faithferrato@gmail.com

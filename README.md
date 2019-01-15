# SLR_VegChange_EF
Formatting and analysis of field-collected vegetation data from eastern North 
Carolina, USA

### Manuscript Citation
P. J. Taillie, C. E. Moorman, B. Poulter, M. Ardon, R. E. Emanuel. *In Review.*
  Decadal-scale vegetation change driven by salinity at the leading edge of 
  rising sea level. Ecosystems.
  
R project written by Paul J. Taillie
30 August, 2018

### Project Overview and Script Format
The project includes all the required data and code to format raw data, fit 
models, summarize results, and produce figures.  The files are grouped in an R
project and thus are relatively linked.  This means the user does not have to 
set working directory to get scripts to run.  Simply unzip the folder, open R,
open the project file with the same name as that found at the top of this 
document.  Required packages may have to be installed.

### Naming and Organization of Files and Folders

raw_data
  * by_plot_veg.csv - contains field collected vegetation and soil data
  * elev_samples.csv - contains elevation samples from LiDAR-derived DEM
  
scripts
  * figure2_script.R - Change in vegetation metrics by site and community
  * figure3_script.R - soil and elevation summaries
  * table1_script.R - linear model effect sizes for soil sodium and elevation
  * table2_script - measured vegetation density at veg plots in each sampling period






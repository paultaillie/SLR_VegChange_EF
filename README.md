# SLR_VegChange_EF
Formatting and analysis of field-collected vegetation data from eastern North 
Carolina,USA

### Manuscript Citation
P. J. Taillie, C. E. Moorman, B. Poulter, M. Ardon, R. E. Emanuel. *In Review.*
  Decadal-scale vegetation change driven by salinity at the leading edge of 
  rising sea level. Earth's Future.
  
R project written by Paul J. Taillie
30 August, 2018

### Project Overview and Script Format
The project includes all the required data and code to format raw data, fit 
models, summarize results, and produce figures.  The files are grouped in an R
project and thus are relatively linked.  This means the user does not have to 
set working directory to get scripts to run.  Simply unzip the folder, open R,
open the project file with the same name as that found at the top of this 
document.  Required packages may have to be installed.

### Naming and Organization of Files
The first letter or a script file relates to a hierarchy of dependency.  For 
example, scripts starting with "a" need to be run before scripts starting with 
successive letters, e.g. "b", "c", etc.

"a" scripts
  * do not depend on other scripts
  * use data from "raw_data" folder
  * typically involve data formatting
  * end by writing processed data for use in successive scripts

"b" scripts
  * Depend on output from "a" scripts
  * Read in "proccessed_data"
  * Typically perform an analysis
  
"c" scripts
  * Depend on "processed_data"
  * Typically produce graphics
  
 "Figures"
   * Destination folder for created graphics

## List of Files






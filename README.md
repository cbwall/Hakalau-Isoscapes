## Isoscapes of remnant and restored Hawaiian montane forests reveal differences in biological nitrogen fixation and carbon inputs  
Data here is found in following manuscript (in revision, *PeerJ*)   
  
CB Wall<sup>1,2</sup>, SIO Swift<sup>2</sup>, CM D'Antonia<sup>3</sup>, G Gebauer<sup>4</sup>, NA Hynson<sup>2</sup>  
<sup>1</sup>University of California, San Diego, USA  
<sup>2</sup>University of Hawai'i at Mānoa, Pacific Biosciences Research Center, USA  
<sup>3</sup>University of California, Santa Barbara, USA  
<sup>4</sup>University of Bayreuth, Germany   
 
<p align="center">
  <img align="center" src="https://github.com/cbwall/Hakalau-Isoscapes/blob/master/photos/Hakalau_forest_figure_small.jpg" width="50%" height="50%">
</p>


Stable isotope analysis of plants and soil in remnant and restored koa stands in Hakalau Forest National Wildlife Refuge. Koa (*Acacia koa*) leaves, understory plants (*Rubus* spp.), and soil in remanant forests where cattle have been excluded and in restored forests where cattle once roamed and vegetation was cleared in 19th century. Plots (20 x 35m) were laid out in each forest stand with samples collected within 4 x 5m subplots (*A. koa* or *Rubus* spp.) or at spatially explicit intervals (soil) to create an isotope-map, or "isoscape," of δ<sup>15</sup>N and δ<sup>13</sup>C isotope values.    

## File Directory  
The file directory contains folders and scripts (Rmd) to be run in RStudio. The folders house data, output, and raw + polished figures.  
   - *Hakalau Isoscape.Rproj* = the R project for the analysis, load this first to allow code to run from correct directory in R Studio
   - *Hakalau Isoscapes.Rmd* = the scripts and annotated code chunks here will walk through analyses and produce outputs
   - *Hakalau-Isoscapes.html* = the knit output of the Rmd file. Open this in any browser.
 
   - Folders
     - *data* = contains raw and processed data files
     - *figures* = contains raw exported figures from code and processed/cleaned figures in the *final.published* folder
     - *output* = contains the *scape.data.csv* file, used in generating isoscopes on x-y point system
     - *photos* = contains filed photos or photos used in Rmd.html knitr

## Metadata
Important data files to generate maps, figures, and run models can be foudn in the *data folder*. The key data files are:  

  - *Hakalau.gps.csv*  
    - This is the gps data used to make the ggmap using satellite images  
    - Data include latitude, longitude, and elevation for the mapped plot corners and *A. koa* trees
    - Two trees are mapped as they bordered the plot but are outside plot boundaries
  - *krig.matrix.csv*  
    - This is used to make the krig landscape for spatial interpolation  
    - Data here use the distance in x-y meters of the superplot to make an x-y matrix for autoKrig
  - *Hakalau.rawdata.csv*   
    - This is the raw data used to generate metrics such as diameter at breast height (dbh) and canopy area  
  - *Hakalau.fulldata.csv*    
    - This is the final data once dbh and canopy area have been calculated  
 
 Explanation of column info in raw *Hakalau.rawdata.csv*
  - *Plot* = either Koa Plantation (KP) or remnant koa forest (RK)
  - *Sample* = soil, koa (*Acacia koa*), RUBARG (*Rubus argutus*), RUBHAW (*Rubus hawaiiensis*)
  - *ID* = individual koa trees, not IDs for Rubus spp. (RUBARG, RUBHAW)
  - *Position.point* = the quadrat each plant is in, with corresponding *ID* number (for koa only)
  - *replicates* = the number of replicates sampled for each sample (pooling across different plants only in *Rubus* spp.)
  - *DBH..cm.1* - *DBH..cm.5* = diameter at breast height (dbh) in cm for individual stem/trunks, with #(1-5) indicating the number of stems
  - *canopy.0..m* - *canopy.270..m* = canopy area measured in m, making an ellipse of 4 headings (0, 90, 180, 270 degrees)
  - *N..ug* = nitrogen content in micrograms N in soil and foliar samples run for isotope analysis
  - *N..percent* = percent nitrogen for soil and foliar samples run for isotope analyses
  - *Total.N..mmol.gdw* = total N content for soil and foliar samples as mmol of N per gram of dry weight in parent material
  - *d15N* = δ<sup>15</sup>N, nitrogen isotope values relative to air-standard in permil notation
  - *C..ug* = carbon content in micrograms C in soil and foliar samples run for isotope analysis, later used to calculate molar C:N
  - *C..percent* = percent carbon for soil and foliar samples run for isotope analyses
  - *Total.C..mmol.gdw* = total C content for soil and foliar samples as mmol of C per gram of dry weight in parent material
  - *d13C* = δ<sup>13</sup>C, carbon isotope values relative to VPDB-standard in permil notation
  - *C.N* = molar ratio of carbon to nitrogen using *Total.C..mmol.gdw / Total.N..mmol.gdw*
  
In the final data *Hakalau.fulldata.csv*, all the above aplly, except *DBH.total* (cm) and *Canopy.area* (m) are now the calculated variables.
 


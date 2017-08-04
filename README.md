# birboat
R codes to analyse interactions of birds with boats (or more generally of two moving objects whose paths are known)

Data (files .csv) contains some of the intermediate files (a subsample of them) we use for analyses, once we have calculated the distance bird-boat (from script Functions_BirdToBoatCorrespondence) from bird and boat tracks,
estimated the maximum attraction threshold (thanks to script Functions_CategoryRatiosForAttr&AttendThresholds, and extracted only the bird locs within the relevant range we obtain : AllDetLoc30km.csv thus contains all bird locations that were within 30km of a boat.

From this file we can define "encounters", that is, series of consecutive locs within a certain range. This is done by the R code GetMetaSerForDifferentAT&TTR. It leads to establishing a metadata table of indices for each encounter (example here: DetectionSeries_30km_3km_TTR4locs.csv, codes for different parameter values, used for sensitivity analyses, is provided).

Some statistics, such as the number of encounters per hour of trip (encounter rate) needs be calculated at the trip level. Metadata at the trip level is given in MetaWithSensitivityERs.csv (ER for encounter Rate).

This can then be statistically analysed (probability to attend at each encounter, time remaining there, distance while attending etc) through remaining codes!

Relevant publications are 
  - Collet, J., Patrick, S. & Weimerskirch, H. Albatrosses redirect flight towards vessels at the limit of their visual range. (2015) Marine Ecology Progress Series vol. 526 pp. 199-205 (doi: 10.3354/meps11233)
  - Collet, J., Patrick, S. C., & Weimerskirch, H. (2017). Behavioral responses to encounter of fishing boats in wandering albatrosses. Ecology and evolution, 7(10), 3335-3347
  - Collet, J., Patrick, S. C., & Weimerskirch, H. (2017). A comparative analysis of the behavioral response to fishing boats in two albatross species. Behavioral Ecology. (early view)

In case of any doubt, please drop me an email at pro@colletj.fr

Best

Julien


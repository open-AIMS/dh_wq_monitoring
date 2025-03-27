## run ../debug.sh first
setwd("R")
options(width = 80, browser = "chromium")
unloadNamespace("status")
unloadNamespace("reportcards")
#detach(package:status)
library(status)
library(reportcards)

## Stage 2
source("startup_functions.R")
## source("05_stats_functions.R")

start_matter()

## Stage 2
#####################################################################
## The following module simply reads in the csv input source files ##
## into R and saves them in .RData format in                       ##
## /data/YEAR/primary/processed/stage1                             ##
#####################################################################
source("10_load_data.R")
module_load_data()

## Stage 3
#####################################################################
## The following module simply reads in the shapefiles provided    ##
## and packs them together into a complete set for this project.   ##
## The purpose of this step is to assign spatial fields (like      ##
## ZoneName and Region to the observed data based on lat/long      ##
## /data/YEAR/processed/dh_sf.rds                                  ##
#####################################################################
source("15_prepare_spatial.R")
module_prepare_spatial()

## Stage 4
#################################################################
## The following module processes the water data:              ##
## - selecting (filter) only Measures with guideline values    ##
## - selecting (filter) only Dates within the range defined by ##
##   StartDate and EndDate in config/config.ini                ##
## - adding financial year                                     ##
#################################################################
source("20_process_data.R")
module_process_data()

## Stage 5
#######################################################################
## The following module applies the scaled modified amplitude method ##
## to all water quality Measures (at the Site level)                 ##
## except DO (which uses a modification of the Fitzroy Partnership   ##
## method). It also starts the process of LOD by identifying if and  ##
## which LOD Rule should be applied.  Note the actual Rules are      ##
## applied in the DH_water_prepareBoot.R module                      ##
#######################################################################
source("30_indices.R")
module_indices()

## Stage 6
#######################################################################
## The following module produces figure and tabular summaries in     ##
## the form of a water quality QAQC report                           ##
## (data/YEAR/output/figures/QAQC/waterQAQC.pdf).                    ##
#######################################################################
source("40_qaqc.R")
module_qaqc()

## Stage 7
#################################################################################
## The following module performs aggregation via a Boostrapping entirely coded ##
## within R.  At each aggregation, the following are calculated from           ##
## the conditional probability tables:                                         ##
## - Mean:       the Expected Utility                                          ##
## - Grade:      a letter code derived from the Mean                           ##
## - sd:         standard deviation of the Expected Utility                    ##
## - Confidence: standard deviation scaled to the range [1,0] from             ##
##               [0,0.4] where 0.4 is the maximum standard deviation           ##
##               possible                                                      ##
## - Signal:     five point strength scale representation of                   ##
##               Confidence                                                    ##
## The aggregations are at the following levels:                               ##
## - Zone/Measure         (unaggregated)                                       ##
## - Zone/Subindicator    (Measure->Subindicator)                              ##
## - Zone/Indicator       (Subindicator->Indicator)                            ##
## - Region/Subindicator  (Zone->Region)                                       ##
## - Region/Indicator     (Subindicator/Indicator)                             ##
## - WH/Subindicator      (Region->WH)                                         ##
## - WH/Indicator         (Subindicator->Indicator)                            ##
#################################################################################
source("50_bootstrapp.R")
module_boot()

## Stage 8
source("60_summaries.R")
module_summaries()

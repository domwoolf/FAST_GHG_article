if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else warning("Make sure this script is run from its source directory")

# source('convert_tables.r') # only need to run this file once
source('FAST-GHG.R') # function to rn the FAST-GHG model

# base results
if(!file.exists('merged_tables.csv')) source('merge_tables.r')
source('FAST-GHG_complete_grid_vectorised.R')
source('merge_spatial.r')
source('draw_maps.r')


# Monte carlo results
if(!file.exists('merged_tables_monte.csv')) source('merge_tables_monte.r')
source('FAST-GHG_monte.R')


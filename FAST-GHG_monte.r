# library(matrixStats)
library(data.table)

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  } else warning("Make sure this script is run from its source directory")
constants = as.list(fread('constants.csv')[, tibble::deframe(.SD[,1:2])])

# county level data for climate, soil, yield and fertilizer rate
county = fread('county.csv')
county[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]
county[, Soil := ifelse(soil.class =="Sandy", "Sandy", "Other")]
county = county[, c("STATEFP",  "STATE", "COUNTYFP", "NAME", "ALAND", "AWATER", 
                    "clay", "Soil",   "Temperature",  "Moisture",
                    "MaizeNrate", "WheatNrate", "SoybeanNrate", 
                    "MaizeYield", "WheatYield", "SoybeanYield")]
# reshape to provide Nrate and yield columns by Crop in long format.
county = melt(county, measure.vars = c("MaizeNrate", "MaizeYield",  "SoybeanNrate", "SoybeanYield", "WheatNrate", "WheatYield"))
county[, Crop := sub("Nrate|Yield", "", variable)]
county[, variable := sub("Maize|Soybean|Wheat", "", variable)]
county = dcast(county, STATEFP + STATE + COUNTYFP + NAME + ALAND + AWATER + clay + Soil + Temperature + Moisture + Crop ~ variable , value.var = "value")
setkey(county, STATEFP, COUNTYFP, Temperature, Moisture, Soil, Crop)

# add cover,crop and tillage columns to county, and merge in their respective bmp values per crop
bmp = fread('bmp.csv', select = c("COUNTYFP", "STATEFP",
                                  "Maize_BMP", "Wheat_BMP", "Soybean_BMP", 
                                  "corn.ha", "wheat.ha", "soy.ha"))
setkey(bmp, STATEFP, COUNTYFP)
bmp.long = melt(bmp,  c('STATEFP', 'COUNTYFP'), c('Maize_BMP', 'Wheat_BMP', 'Soybean_BMP'), variable.name = 'Crop')
bmp.long[, Crop := sub('_BMP', '', Crop)]
bmp.long[, Cover.crop := sub('_.+', '', value)]
bmp.long[, Tillage := sub('.+_', '', value)]
bmp.long[, value := NULL]
bmp.long[, is.bmp := TRUE]
setkey(bmp.long, STATEFP, COUNTYFP, Crop, Cover.crop, Tillage)
county[bmp.long, c("Cover.crop","Tillage") := .(i.Cover.crop, i.Tillage), on = c("STATEFP","COUNTYFP","Crop")]

# Read parameter values for all combinations of crop, cover-crop, tillage, soil, and climate
params = fread("merged_tables_monte.csv")
params[is.na(F_Y_CC), F_Y_CC := 0]
params[is.na(F_Y_T),  F_Y_T  := 0]
params[is.na(f_Nl),   f_Nl   := 0]
keys = c("Cover.crop", "Tillage", "Crop", "Temperature", "Moisture", "Soil")

# add randomised columns for reversal risk and intensification fraction
params[, R   := runif(.N, 0.2, 0.8)]
params[, f_i := runif(.N, 0.2, 0.8)]

#  convert key columns to factor to save memory allocation in large table
params[, (keys)      := lapply(.SD, as.factor), .SDcols=keys]
county[, Soil        := factor(Soil,        levels = levels(params$Soil))]
county[, Temperature := factor(Temperature, levels = levels(params$Temperature))]
county[, Moisture    := factor(Moisture,    levels = levels(params$Moisture))]
county[, Crop        := factor(Crop,        levels = levels(params$Crop))]
setkeyv(params, keys)
setkeyv(county, keys)

# Merge county and parameter tables to create large database of randomised bmp parameter values for every county
results = merge.data.table(county, params, allow.cartesian=TRUE)

# Run model (note that results data.table is also updated by reference)
all_fast = FAST_GHG(results)
gc()

# summarise GHG source values
all_fast[, .(mean = mean(value), sd = sd(value)), by=label]

# extract only total GHGs for each practice/location
totals = all_fast[label == 'Total'][, c('variable', 'label') := NULL]
setnames(totals, c('Cover.crop'), c('CC'))

# remove cartesian grid of results to save memory - we just use the totals going forwards
rm(all_fast)
gc()

# merge harvested area
ha = melt(bmp, c('COUNTYFP', 'STATEFP'), patterns("ha$"), variable.name = "Crop", value.name = "ha")
ha[, Crop := c(corn.ha = "Maize", wheat.ha = "Wheat", soy.ha = "Soybean")[Crop]]
totals[ha,  harvested_ha := i.ha, on = c('COUNTYFP', 'STATEFP', 'Crop')]

# multiply per ha mitigation by harvested area 
totals[, GHG.tot := value * harvested_ha]

# Mean and sd for Monte Carlo bmp values
totals[, I := 1:.N, by = .(STATEFP,COUNTYFP,Crop)] # add a dummy id variable to cast, so we can keep all rows without aggregating
BMP_Monte = totals[, sum(GHG.tot), by = I][, V1 / 1e6]
mean(BMP_Monte) 
sd(BMP_Monte)
boxplot(BMP_Monte)
hist(BMP_Monte, breaks=21)


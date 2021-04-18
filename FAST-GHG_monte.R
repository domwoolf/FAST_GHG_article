library(matrixStats)
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
bmp = fread('bmp.csv', select = c("COUNTYFP", "STATEFP",
                                  "Maize_BMP", "Wheat_BMP", "Soybean_BMP", 
                                  "corn.ha", "wheat.ha", "soy.ha"))

setkey(bmp, STATEFP, COUNTYFP)
# county[bmp, on=key(bmp)]

# Read parameter values for all combinations of crop, cover-crop, tillage, soil, and climate
params = fread("merged_tables_monte.csv")
keys = c("Cover.crop", "Tillage", "Crop", "Temperature", "Moisture", "Soil")

#  convert key columns to factor to save memory allocation in large table
params[, (keys)      := lapply(.SD, as.factor), .SDcols=keys]
county[, Soil        := factor(Soil,        levels = levels(params$Soil))]
county[, Temperature := factor(Temperature, levels = levels(params$Temperature))]
county[, Moisture    := factor(Moisture,    levels = levels(params$Moisture))]
county[, Crop        := factor(Crop,        levels = levels(params$Crop))]
setkeyv(params, keys)
setkeyv(county, intersect(keys, names(county)))

# add randomised columns for reversal risk and intensification fraction
params[, R   := runif(.N, 0.2, 0.8)]
params[, f_i := runif(.N, 0.2, 0.8)]

# exclude cover crops in dry climate
params = params[!(Cover.crop != 'None' & Moisture == 'Dry')]
params[is.na(F_Y_CC), F_Y_CC := 0]
params[is.na(F_Y_T),  F_Y_T  := 0]
params[is.na(f_Nl),   f_Nl   := 0]

# Merge county and parameter tables to create large database of every parameter combination for every county
results = merge.data.table(county, params, allow.cartesian=TRUE)

# Run model (note that results data.table is also updated by reference)
all_fast = FAST_GHG(results)
gc()

# flag which rows correspond to county BMP
bmp.long = melt(bmp,  c('STATEFP', 'COUNTYFP'), c('Maize_BMP', 'Wheat_BMP', 'Soybean_BMP'), variable.name = 'Crop')
bmp.long[, Crop := sub('_BMP', '', Crop)]
bmp.long[, Cover.crop := sub('_.+', '', value)]
bmp.long[, Tillage := sub('.+_', '', value)]
bmp.long[, value := NULL]
bmp.long[, is.bmp := TRUE]
setkey(bmp.long, STATEFP, COUNTYFP, Crop, Cover.crop, Tillage)
all_fast[bmp.long, is.bmp := i.is.bmp, on = key(bmp.long)]
all_fast[is.na(is.bmp), is.bmp := FALSE]

# summarise bmp values
all_fast[is.bmp==TRUE, .(mean = mean(value), sd = sd(value)), by=label]

# extract only total GHGs for each practice/location
totals = all_fast[label == 'Total'][, c('variable', 'label') := NULL]
setnames(totals, c('Cover.crop'), c('CC'))

# remove cartesian grid of results to save memory - we just use the totals going forwards
rm(all_fast)
gc()

# reshape to wide
totals[, I := 1:.N, by = .(STATEFP,COUNTYFP,Crop,CC,Tillage)] # add a dummy id variable to cast, so we can keep all rows without aggregating
totals = dcast(totals, I+STATEFP+COUNTYFP~Crop+CC+Tillage, value.var = 'value')

# merge bmp columns for each crop/county
totals[bmp, c("Maize_BMP", "Wheat_BMP", "Soybean_BMP") := 
            .(i.Maize_BMP, i.Wheat_BMP, i.Soybean_BMP), 
            on = c('STATEFP', 'COUNTYFP')]
totals[bmp, c("Maize_ha", "Wheat_ha", "Soybean_ha") := 
         .(i.corn.ha, i.wheat.ha, i.soy.ha), 
       on = c('STATEFP', 'COUNTYFP')]
totals[Maize_BMP=="", Maize_BMP := NA]
totals[Wheat_BMP=="", Wheat_BMP := NA]
totals[Soybean_BMP=="", Soybean_BMP := NA]

# find bmp value for each crop/county/sample
totals[!is.na(Maize_BMP), 
       Maize_BMP.val := get(paste0('Maize_', Maize_BMP)), 
       by = seq_len(nrow(totals[!is.na(Maize_BMP)]))]
totals[!is.na(Wheat_BMP), 
       Wheat_BMP.val := get(paste0('Wheat_', Wheat_BMP)), 
       by = seq_len(nrow(totals[!is.na(Wheat_BMP)]))]
totals[!is.na(Soybean_BMP), 
       Soybean_BMP.val := get(paste0('Soybean_', Soybean_BMP)), 
       by = seq_len(nrow(totals[!is.na(Soybean_BMP)]))]

# multiply per ha mitigation by harvested area 
totals[, Maize_BMP.tot := Maize_BMP.val * Maize_ha]
totals[, Wheat_BMP.tot := Wheat_BMP.val * Wheat_ha]
totals[, Soybean_BMP.tot := Soybean_BMP.val * Soybean_ha]
totals[, BMP.tot := rowSums(.SD, na.rm = TRUE), .SDcols  = patterns("BMP.tot")]

# Mean and sd for Monte Carlo bmp values
BMP_Monte = totals[, sum(BMP.tot), by = I][, V1 / 1e6]
mean(BMP_Monte) 
sd(BMP_Monte)
boxplot(BMP_Monte)
hist(BMP_Monte, breaks=21)





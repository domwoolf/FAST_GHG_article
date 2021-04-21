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

# average county values, per climate zone
Mode = function(x) names(sort(table(x), decreasing = TRUE)[1])
zones = county[, .(clay=mean(clay), Soil=Mode(Soil), Nrate=mean(Nrate, na.rm = TRUE), Yield=mean(Yield, na.rm = TRUE)), by = .(Temperature, Moisture, Crop)]
zones = zones[complete.cases(zones)]
setkey(zones,Temperature,Moisture,Soil,Crop)

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
params = fread("merged_tables_sensitivity.csv")
params[is.na(F_Y_CC), F_Y_CC := 0]
params[is.na(F_Y_T),  F_Y_T  := 0]
params[is.na(f_Nl),   f_Nl   := 0]
keys = c("Cover.crop", "Tillage", "Crop", "Temperature", "Moisture", "Soil")

#  convert key columns to factor to save memory allocation in large table
params[, (keys)      := lapply(.SD, as.factor), .SDcols=keys]
county[, Soil        := factor(Soil,        levels = levels(params$Soil))]
county[, Temperature := factor(Temperature, levels = levels(params$Temperature))]
county[, Moisture    := factor(Moisture,    levels = levels(params$Moisture))]
county[, Crop        := factor(Crop,        levels = levels(params$Crop))]
setkeyv(params, keys)
setkeyv(county, keys)

# Merge county and parameter tables to create large database of randomised bmp parameter values for every county
results = merge.data.table(zones, params, allow.cartesian=TRUE)

# Run model (note that results data.table is also updated by reference)
results = results[Cover.crop == 'None' | Moisture == 'Moist']
all_fast = FAST_GHG(results)
gc()



# plot results
# with climate...
results.summary = results[, .(Delta.GHG = mean(Delta.GHG)), by = .(Cover.crop,Tillage,Crop,Temperature,Moisture,sensitivity, sens_level)]
results.summary = dcast(results.summary, Cover.crop+Tillage+Crop +Temperature +Moisture+sensitivity~sens_level, value.var = 'Delta.GHG')

# without climate...
results.summary = results[, .(Delta.GHG = mean(Delta.GHG)), by = .(Cover.crop,Tillage,Crop,sensitivity, sens_level)]
results.summary = dcast(results.summary, Cover.crop+Tillage+Crop+sensitivity~sens_level, value.var = 'Delta.GHG')

ggplot(results.summary, aes(sensitivity, color=Crop)) +
  geom_errorbar(aes(ymax=high,ymin=low)) +
  coord_flip() +
  facet_grid(Cover.crop~Tillage)

# without climate or crop...
results.summary = copy(results)
results.summary = results[, .(Delta.GHG = scale(Delta.GHG)[,1], sensitivity, sens_level), by = .(Cover.crop,Tillage,Crop)]
results.summary[is.na(Delta.GHG), Delta.GHG := 0]
results.summary = results.summary[, .(Delta.GHG = mean(Delta.GHG)), by = .(sensitivity, sens_level)]
results.summary = dcast(results.summary, sensitivity~sens_level, value.var = 'Delta.GHG')
results.summary[, rng := abs(high-low)]
setorder(results.summary, rng)
results.summary[, sensitivity := factor(sensitivity, levels = sensitivity)]
ggplot(results.summary, aes(sensitivity)) +
  geom_errorbar(aes(ymax=high,ymin=low)) +
  coord_flip() 





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


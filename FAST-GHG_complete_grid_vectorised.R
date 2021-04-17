library(data.table)
plot_results = FALSE
save_results = FALSE
validate = FALSE
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  } else warning("Make sure this script is run from its source directory")
constants = as.list(fread('constants.csv')[, tibble::deframe(.SD[,1:2])])
R = 0.5 # Reversal risk

# county level data for climate, soil, yield and fertilizer rate
county = fread('county.csv')
county[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]
county[, Soil := ifelse(soil.class =="Sandy", "Sandy", "Other")]
county = county[, c("STATEFP",  "STATE", "COUNTYFP", "NAME", "ALAND", "AWATER", 
                    "clay", "Soil",   "Temperature",  "Moisture",
                    "MaizeNrate", "MaizeYield",  "SoybeanNrate", "SoybeanYield", "WheatNrate", "WheatYield")]

# reshape to provide Nrate and yield columns by Crop in long format.
county = melt(county, measure.vars = c("MaizeNrate", "MaizeYield",  "SoybeanNrate", "SoybeanYield", "WheatNrate", "WheatYield"))
county[, Crop := sub("Nrate|Yield", "", variable)]
county[, variable := sub("Maize|Soybean|Wheat", "", variable)]
county = dcast(county, STATEFP + STATE + COUNTYFP + NAME + ALAND + AWATER + clay + Soil + Temperature + Moisture + Crop ~ variable , value.var = "value")
setkey(county, STATEFP, COUNTYFP, Temperature, Moisture, Soil, Crop)

# Read parameter values for all combinations of crop, cover-crop, tillage, soil, and climate
params = fread("merged_tables.csv")
params = params[Cover.crop != "Mixed"]
keys = c("Cover.crop", "Tillage", "Crop", "Temperature", "Moisture", "Soil")
setkeyv(params, keys)

# Merge county and parameter tables to create large database of every parameter combination for every county
results = merge.data.table(county, params, allow.cartesian=TRUE)

# Run model (note that results data.table is also updated by reference)
all_fast = FAST_GHG(results)

# ----------------------------------------------------------
# GHG.vbls = c('Delta.CO2_SOC', 'Delta.CO2_F_T', 'Delta.CO2_F_CC', 'Delta.CO2_I_T', 
#              'Delta.CO2_I_CC', 'Delta.CO2_L', 'Delta.CO2_N', 'Delta.CO2e_N2O', 'Delta.GHG')
# GHG.labs = c('SOC', 'Fuel (tillage)', 'Fuel (cover crops)', 
#              'Inputs (tillage)', 'Inputs (cover crops)', 
#              'Leakage', 'N fertilizer production', 'N2O emissions', 'Total')
# all_fast = melt.data.table(results, 
#                            measure.vars = GHG.vbls, 
#                            id.vars = c("Temperature", "Moisture", "Soil", "Crop", 
#                                        "STATEFP", "STATE", "COUNTYFP", "NAME", 
#                                        "ALAND", "AWATER", "clay",
#                                        "Yield", "Nrate",
#                                        "Cover.crop", "Tillage"))
# setkey(all_fast, STATEFP, COUNTYFP, Crop, Cover.crop, Tillage, variable)
# all_fast[, label := GHG.labs[match(variable, GHG.vbls)]]  
# ----------------------------------------------------------

# extract only total GHGs for each practice/location
totals = all_fast[label == 'Total'][, c('variable', 'label') := NULL]

# reshape to wide
totals = dcast(totals, STATE+NAME+STATEFP+COUNTYFP ~ Crop+Cover.crop+Tillage, value.var = 'value')

# find BMP for each crop
wmax = function (x) 
{
  w = which.max(x)
  if (length(w)) w else NA
}
suppressWarnings(totals[, grep('BMP', names(totals)) := NULL])

#--- maize
Maize_cols = grep('Maize', names(totals), value = TRUE)
Maize_matrix = as.matrix(totals[,..Maize_cols])
Maize_BMP = apply(Maize_matrix, 1, wmax)
Maize_BMP = unname(unlist(Maize_BMP))
# Maize_BMP[is.na(Maize_BMP)] = grep("None_Conventional", Maize_cols)
totals$Maize_BMP = Maize_cols[Maize_BMP]
totals$Maize_BMP.val = Maize_matrix[cbind(seq(nrow(totals)), Maize_BMP)]
# totals[is.na(Yield)|is.na(Nrate), c("Maize_BMP", "Maize_BMP.val") := NA]

#--- wheat
Wheat_cols = grep('Wheat', names(totals), value = TRUE)
Wheat_matrix = as.matrix(totals[,..Wheat_cols])
Wheat_BMP = apply(Wheat_matrix, 1, wmax)
Wheat_BMP = unname(unlist(Wheat_BMP))
# Wheat_BMP[is.na(Wheat_BMP)] = grep("None_Conventional", Wheat_cols)
totals$Wheat_BMP = Wheat_cols[Wheat_BMP]
totals$Wheat_BMP.val = Wheat_matrix[cbind(seq(nrow(totals)), Wheat_BMP)]
# totals[is.na(Yield)|is.na(Nrate), c("Wheat_BMP", "Wheat_BMP.val") := NA]

#--- Soybean
Soybean_cols = grep('Soybean', names(totals), value = TRUE)
Soybean_matrix = as.matrix(totals[,..Soybean_cols])
Soybean_BMP = apply(Soybean_matrix, 1, wmax)
Soybean_BMP = unname(unlist(Soybean_BMP))
# Soybean_BMP[is.na(Soybean_BMP)] = grep("None_Conventional", Soybean_cols)
totals$Soybean_BMP = Soybean_cols[Soybean_BMP]
totals$Soybean_BMP.val = Soybean_matrix[cbind(seq(nrow(totals)), Soybean_BMP)]
# totals[is.na(Yield)|is.na(Nrate), c("Soybean_BMP", "Soybean_BMP.val") := NA]


### validation subsamples ---
if (validate) {
  validation = totals[sample(.N, 20)]
  fwrite(validation, 'validation_totals.csv')
  all_fast[, G :=.GRP, by = .(State, County, Crop, Cover.Crop, Tillage.Practice)]
  validation = all_fast[G %in% sample(unique(G), 20)]
  fwrite(validation, 'validation_all.csv')
}

# validation = all_fast[totals[grep('None',Soybean_BMP)][1], on=.(STATEFP , COUNTYFP)]
# validation[, c("STATE", "NAME", "Temperature", "Moisture", "Soil", "ALAND", "AWATER", 
#                "variable", "i.STATE", 
#                "i.NAME", "Maize_Legume_Conventional", "Maize_Legume_No-till", 
#                "Maize_Legume_Reduced-till", "Maize_Non-legume_Conventional", 
#                "Maize_Non-legume_No-till", "Maize_Non-legume_Reduced-till", 
#                "Maize_None_Conventional", "Maize_None_No-till", "Maize_None_Reduced-till", 
#                "Maize_BMP", "Maize_BMP.val",
#                "Soybean_Legume_Conventional", "Soybean_Legume_No-till", "Soybean_Legume_Reduced-till", 
#                "Soybean_Non-legume_Conventional", "Soybean_Non-legume_No-till", 
#                "Soybean_Non-legume_Reduced-till", "Soybean_None_Conventional", 
#                "Soybean_None_No-till", "Soybean_None_Reduced-till", "Wheat_Legume_Conventional", 
#                "Wheat_Legume_No-till", "Wheat_Legume_Reduced-till", "Wheat_Non-legume_Conventional", 
#                "Wheat_Non-legume_No-till", "Wheat_Non-legume_Reduced-till", 
#                "Wheat_None_Conventional", "Wheat_None_No-till", "Wheat_None_Reduced-till", 
#                "Wheat_BMP", "Wheat_BMP.val", "Soybean_BMP", "Soybean_BMP.val") := NULL]
# setkey(validation, label, Tillage)
# validation[Cover.crop=="Non-legume" & Crop=='Maize', value, by=.(label, Tillage)]
# validation[Crop=='Maize', value, by=.(label, Tillage, Cover.crop)]
# 
# validation.tot = totals[grep('None',Soybean_BMP)][1]
# validation[Crop=='Soybean', value, by=.(label, Tillage, Cover.crop)]
# 
# 


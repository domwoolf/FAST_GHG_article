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

# Run model (note that results data.table is not updated by reference, because it gets copied)
all_fast = FAST_GHG(results)

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
totals$Maize_BMP = Maize_cols[Maize_BMP]
totals$Maize_BMP.val = Maize_matrix[cbind(seq(nrow(totals)), Maize_BMP)]

#--- wheat
Wheat_cols = grep('Wheat', names(totals), value = TRUE)
Wheat_matrix = as.matrix(totals[,..Wheat_cols])
Wheat_BMP = apply(Wheat_matrix, 1, wmax)
Wheat_BMP = unname(unlist(Wheat_BMP))
totals$Wheat_BMP = Wheat_cols[Wheat_BMP]
totals$Wheat_BMP.val = Wheat_matrix[cbind(seq(nrow(totals)), Wheat_BMP)]

#--- Soybean
Soybean_cols = grep('Soybean', names(totals), value = TRUE)
Soybean_matrix = as.matrix(totals[,..Soybean_cols])
Soybean_BMP = apply(Soybean_matrix, 1, wmax)
Soybean_BMP = unname(unlist(Soybean_BMP))
totals$Soybean_BMP = Soybean_cols[Soybean_BMP]
totals$Soybean_BMP.val = Soybean_matrix[cbind(seq(nrow(totals)), Soybean_BMP)]

### validation subsamples ---
if (validate) {
  validation = totals[sample(.N, 20)]
  fwrite(validation, 'validation_totals.csv')
  all_fast[, G :=.GRP, by = .(State, County, Crop, Cover.Crop, Tillage.Practice)]
  validation = all_fast[G %in% sample(unique(G), 20)]
  fwrite(validation, 'validation_all.csv')
}


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
                    "MaizeNrate", "MaizeYield",  "SoybeanNrate", "SoybeanYield", "WheatNrate", "WheatYield")]

# reshape to provide Nrate and yield columns by Crop in long format.
county = melt(county, measure.vars = c("MaizeNrate", "MaizeYield",  "SoybeanNrate", "SoybeanYield", "WheatNrate", "WheatYield"))
county[, Crop := sub("Nrate|Yield", "", variable)]
county[, variable := sub("Maize|Soybean|Wheat", "", variable)]
county = dcast(county, STATEFP + STATE + COUNTYFP + NAME + ALAND + AWATER + clay + Soil + Temperature + Moisture + Crop ~ variable , value.var = "value")
setkey(county, STATEFP, COUNTYFP, Temperature, Moisture, Soil, Crop)

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

# Initialise columns to correct units and factors
# results[Cover.crop == 'None', F_Y_CC := 0.0]
# results[Cover.crop == 'None', Delta.SOC_CC := 0.0]
# results[, Rotation := ifelse(Crop == 'Maize', 'Continuous Maize', 'Other')]
# results[, clay := clay/100]     # convert from percent to fraction
# results[, F_Y_CC := F_Y_CC/100] # convert from percent to fraction
# results[, F_Y_T := F_Y_T/100]   # convert from percent to fraction
# results[, F_i := F_i/1000]      # convert units to Mg Co2e / kg grain
# results[, F_p := F_p/1000]      # convert units to Mg Co2e / kg grain

# Run model (note that results data.table is also updated by reference)
all_fast = FAST_GHG(results)
gc()
# # Table 11: Direct N2O emission factor for mineral fertilizer
# results[,                                      f_Nd := 0.0079] # kg N2O/ kg N
# results[Moisture == 'Moist',                   f_Nd := 0.0251]
# results[Crop == 'Maize' & Moisture == 'Moist', f_Nd := 0.085 * clay]
# 
# # Amount of N additions available to potentially leach
# results[,                  Nrate.leach := Nrate]
# results[Crop == 'Soybean', Nrate.leach := Nrate + 60]
# # Table 17: Change in N leaching due to practice
# results[Moisture == 'Dry',                                Delta.L := 0.0]
# results[Moisture == 'Moist',                              Delta.L := c(0.06,0.03,0.0)[match(Tillage, c("No-till", "Reduced-till", "Conventional"))]  * Nrate.leach]
# results[Moisture == 'Moist' & Cover.crop == 'Legume',     Delta.L := 0.1  * Nrate.leach]
# results[Moisture == 'Moist' & Cover.crop == 'Non-legume', Delta.L := 0.13 * Nrate.leach]
# 
# # Eq. 5: change in yield (kg grain / ha / yr)
# results[, Delta.Y := Yield * (F_Y_CC + F_Y_T)] 
# 
# # Table 15 (change in N input rate from each group of practices; kg N / ha / yr):
# results[,                           Delta.N_A := 0.0]
# results[Cover.crop == 'Legume',     Delta.N_A := 52 * constants$f_NUE -72 * Delta.SOC_CC - Delta.Y * f_Ng] # kg N / ha / yr 
# results[Cover.crop == 'Non-legume', Delta.N_A := (Delta.L - 22 - Delta.Y * f_Ng)]    # kg N / ha / yr
# results[,                           Delta.N_B := -72 * Delta.SOC_T]
# results[Cover.crop == 'None',       Delta.N_B := Delta.N_B + Delta.L - Delta.Y * f_Ng]
# results[,                           Delta.N_tot := Delta.N_A + Delta.N_B]
# 
# # Equation 9: Change in CO2-equivalent GHG emissions from nitrogen fertilizer production (Mg CO2e / ha / yr).
# results[, Delta.CO2_N := Delta.N_tot * constants$f_Np]
# 
# # Equation 8: Change in indirect nitrous oxide emissions (kg N2O / ha / yr)
# # results[, Delta.N2O_i := Delta.N_tot * (f_Nv + (1-f_up)*f_Nl) + Delta.ON * (f_ONv + (1-f_up)*f_ONl) + f_Nl * Delta.L]
# results[, Delta.N2O_i := Delta.N_tot * (f_Nv + (1-f_up)*f_Nl) + Delta.ON * f_Oni + f_Nl * Delta.L]
# 
# # Equation 7: Change in direct nitrous oxide emissions (kg N2O / ha / yr).
# results[, Delta.N2O_d := Delta.N_tot * f_Nd + Delta.ON * f_ONd]
# 
# # Eq. 6: Change in total nitrous oxide emissions (Mg N2O)
# results[, Delta.N2O := (Delta.N2O_d + Delta.N2O_i) / 1000]
# 
# # Eq. 4:  Note that ILUC is reversible, so we also apply the permanence-risk factor to it
# results[, Delta.CO2_L := Delta.Y * (F_p + (1 - f_i)*F_i*(1 - R))]
# 
# # Eq. 3:
# results[, Delta.SOC := f_100_CC * Delta.SOC_CC + f_100_T * Delta.SOC_T] # average annual change in SOC, over 100 years
# 
# # Eq. 2:
# results[, Delta.CO2_SOC := Delta.SOC * (1 - R) * 44/12]
# results[, Delta.CO2 := Delta.CO2_SOC + Delta.CO2_F_T + Delta.CO2_F_CC + Delta.CO2_I_T + Delta.CO2_I_CC + Delta.CO2_N + Delta.CO2_L]
# 
# # Eq. 1:
# results[, Delta.CO2e_N2O := constants$GWP_N2O * Delta.N2O]
# results[, Delta.GHG := Delta.CO2 + Delta.CO2e_N2O]
# 
# 
# # ----------------------------------------------------------
# GHG.labs = c(Delta.CO2_SOC  = 'SOC', 
#              Delta.CO2_F_T  = 'Fuel (tillage)', 
#              Delta.CO2_F_CC = 'Fuel (cover crops)', 
#              Delta.CO2_I_T  = 'Inputs (tillage)', 
#              Delta.CO2_I_CC = 'Inputs (cover crops)', 
#              Delta.CO2_L    = 'Leakage', 
#              Delta.CO2_N    = 'N fertilizer production', 
#              Delta.CO2e_N2O = 'N2O emissions', 
#              Delta.GHG      = 'Total')
# 
# all_fast = melt.data.table(results, measure.vars = names(GHG.labs), 
#                            id.vars = c("Temperature", "Moisture", "Soil", "Crop", "STATEFP", "STATE", 
#                                        "COUNTYFP", "NAME", "ALAND", "AWATER", "clay", 
#                                        "Cover.crop", "Tillage"))
# all_fast[, label := GHG.labs[variable]]
# ----------------------------------------------------------

# summary results ----------------------------------------------------------
library(ggplot2)
ggplot(results, aes(Delta.GHG)) +
  geom_density(aes(color=Crop)) +
  facet_grid(Crop~.)


# extract only total GHGs for each practice/location
totals = all_fast[label == 'Total'][, c('variable', 'label') := NULL]
setnames(totals, c('Cover.crop'), c('CC'))

# remove cartesian grid of results to save memory - we just use the totals going forwards
rm(all_fast)
gc()

# reshape to wide
# totals = dcast(totals, STATE+NAME+STATEFP+COUNTYFP+Temperature+Moisture+Soil~Crop+CC+Tillage, value.var = 'value', 
#                fun.aggregate = list(mean,sd))
totals[, I := 1:.N, by = .(STATEFP,COUNTYFP,Crop,CC,Tillage)] # add a dummy id variable to cast, so we can keep all rows without aggregating
totals = dcast(totals, I+STATEFP+COUNTYFP~Crop+CC+Tillage, value.var = 'value')

# merge bmp columns for each crop/county
bmp = fread('bmp.csv', select = c("COUNTYFP", "STATEFP",
                                  "Maize_BMP", "Wheat_BMP", "Soybean_BMP", 
                                  "corn.ha", "wheat.ha", "soy.ha"))
setkey(bmp, STATEFP, COUNTYFP)
totals[bmp, c("Maize_BMP", "Wheat_BMP", "Soybean_BMP") := 
            .(i.Maize_BMP, i.Wheat_BMP, i.Soybean_BMP), 
            on = c('STATEFP', 'COUNTYFP')]
totals[bmp, c("Maize_ha", "Wheat_ha", "Soybean_ha") := 
         .(i.corn.ha, i.wheat.ha, i.soy.ha), 
       on = c('STATEFP', 'COUNTYFP')]
# totals[, grep("value_sd_", names(totals)) := NULL]
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





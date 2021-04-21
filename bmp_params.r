# add flag to each treatment in results if it is bmp for that county
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
results[bmp.long, is.bmp := i.is.bmp, on = key(bmp.long)]
results[is.na(is.bmp), is.bmp := FALSE]

bmp_params = results[is.bmp==TRUE][, is.bmp := NULL]
fwrite(bmp_params, "bmp_params.csv")

# TO DO::: we need same parameter values reproduced for each parameter combination 
n = 100
montetabs = bmp_params[, .(
  Temperature, Moisture, Soil, clay, Cover.crop, Tillage, Nrate, Yield,
  Delta.CO2_I_CC = rnorm (n, Delta.CO2_I_CC, sd_Delta.CO2_I_CC),
  Delta.CO2_F_CC = rnorm (n, Delta.CO2_F_CC, sd_Delta.CO2_F_CC),
  Delta.CO2_I_T  = rnorm (n, Delta.CO2_I_T,  sd_Delta.CO2_I_T),
  Delta.CO2_F_T  = rnorm (n, Delta.CO2_F_T,  sd_Delta.CO2_F_T),
  f_100_CC       = rnorm (n, f_100_CC,       abs(f_100_CC * 0.1)),
  f_100_T        = rnorm (n, f_100_T,        abs(f_100_T * 0.1)),
  F_i            = rnorm (n, F_i,            abs(F_i * 0.2)),
  F_p            = rnorm (n, F_p,            abs(F_p * 0.2)),
  f_Ng           = rnorm (n, f_Ng,           se_f_Ng),
  f_Nv           = rlnorm(n, log(f_Nv),      sdlog_f_Nv),
  f_Nl           = rlnorm(n, log(f_Nl),      sdlog_f_Nl),
  f_ONd          = rlnorm(n, log(f_ONd),     logsd_f_Ond),
  f_Oni          = rlnorm(n, log(f_Oni),     logsd_f_Oni),
  f_up           = rnorm (n, f_up,           se_f_up),
  Delta.ON       = rnorm (n, Delta.ON,       abs(Delta.ON * 0.1)),
  Delta.SOC_CC   = rnorm (n, Delta.SOC_CC,   se_Delta.SOC_CC),
  Delta.SOC_T    = rnorm (n, Delta.SOC_T,    se_Delta.SOC_T),
  F_Y_T          = rnorm (n, F_Y_T,          se_F_Y_T),
  F_Y_CC         = rnorm (n, F_Y_CC,         se_F_Y_CC)
), by=.(STATEFP, COUNTYFP, Crop)]
montetabs = montetabs[, lapply(.SD, function(x) ifelse(is.nan(x), NA, x))] # replace NaN with NA
# fwrite(montetabs, "merged_tables_monte.csv")

############ do mc

all_fast = FAST_GHG(montetabs)
gc()


# extract only total GHGs for each practice/location
totals = all_fast[label == 'Total'][, c('variable', 'label') := NULL]
setnames(totals, c('Cover.crop'), c('CC'))

# reshape to wide
# totals[, I := 1:.N, by = .(STATEFP,COUNTYFP,Crop,CC,Tillage)] # add a dummy id variable to cast, so we can keep all rows without aggregating
# totals = dcast(totals, I+STATEFP+COUNTYFP~Crop+CC+Tillage, value.var = 'value')

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
# boxplot(BMP_Monte)
# hist(BMP_Monte, breaks=21)

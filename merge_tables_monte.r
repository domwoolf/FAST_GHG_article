library(data.table)
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else warning("Make sure this script is run from its source directory")
tables.path = 'tables/'
states = fread(paste0(tables.path, 'state.csv'))[, .(state = NAME)]
counties = fread(paste0(tables.path, 'county.csv'))[, .(state = STATE, county = NAME)]
setkey(states, state)
setkey(counties, state, county)

# load the data files emission factors and default parameter values
tab.names = list.files(tables.path, '.csv')
tabs = lapply(paste0(tables.path, tab.names), fread)
names(tabs) = sub('.csv$', '', tab.names)
tabs$state[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]
tabs$county[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]
param_tabs = tabs[!grepl("county|state", names(tabs))]

# add keys to all tables
keys = c("Cover.crop", "Tillage", "Crop", "Temperature", "Moisture", "Soil")
lapply(param_tabs, function (tab) {
  tab[, setdiff(keys, names(tab)) := NA_character_]
  tab[, (keys) := replace(.SD, .SD == "All", NA_character_), .SDcols = keys]
  setkeyv(tab, keys)
})

# Create a named list of all parameter keys, containing the unique values for each key
key_vals = rbindlist(param_tabs, fill = TRUE)[, list((sapply(.SD, unique))), .SDcols = keys]
key_vals = sapply(key_vals$V1, function(x) x[!is.na(x)])
names(key_vals) = keys

# Create a factorial grid of all key parameter combinations
all_keys = expand.grid(key_vals, stringsAsFactors = FALSE)
setDT(all_keys)
setkeyv(all_keys,keys)

# Merge all parameter tables to provide all parameter values for all combinations of keys
fill_match = function(x, y, keys) {
  m = lapply(seq_len(nrow(x)), function(i) {
    keycols = x[i, ..keys]
    valcols = x[i, .SD, .SDcols = setdiff(names(x), keys)]
    keycols = keycols[, .SD, .SDcols = which(!is.na(unlist(keycols)))]
    x.1 = cbind(keycols, valcols)
    setkeyv(x.1, names(keycols))
    merge(y, x.1)
  })
  m = rbindlist(m, use.names=TRUE)
  setkeyv(m, keys)
  return(m)
}
mergedtabs = lapply(param_tabs, fill_match, all_keys, keys)
mymerge = function(x,y) merge.data.table(x, y, all=TRUE) 
mergedtabs = Reduce(mymerge, mergedtabs)

n = 500
montetabs = mergedtabs[, .(
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
  F_Y_T          = rnorm (n, F_Y_T,          se_F_Y_CC),
  F_Y_CC         = rnorm (n, F_Y_CC,         se_F_Y_T)
), by=keys]
montetabs = montetabs[, lapply(.SD, function(x) ifelse(is.nan(x), NA, x))] # replace NaN with NA
fwrite(montetabs, "merged_tables_monte.csv")


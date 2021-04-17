library(data.table)

tables.path = '~/Box/projects/FAST-GHG/code/tables_orig'
states = fread(paste0(tables.path, 'state.csv'))[, .(state = NAME)]
counties = fread(paste0(tables.path, 'county.csv'))[, .(state = STATE, county = NAME)]
setkey(states, state)
setkey(counties, state, county)

# ---------------------------------------------------------------------------------------------
# load the data files emission factors and default parameter values
# ---------------------------------------------------------------------------------------------
#  function reads csv data from given file & path into a data.table with R-friendly column names
read.tab = function(fname, path = tables.path) {
  tab = fread(paste0(path, fname))
  # function to remove Latex formatting from column headers, to make them into R-friendly names
  # note that base::make.names does not make exactly what we want here
  normal.names = function(x) {
    x = gsub('\\$|\\\\|\\{|\\}|\\$','',x)
    gsub('textrm| ','.', x)
  }
  setnames(tab, normal.names(names(tab)))
}
tab.names = list.files(tables.path, '.csv')
tabs = lapply(tab.names, read.tab)
names(tabs) = sub('.csv$', '', tab.names)
tabs$state[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]
tabs$county[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]
# calculate table rows for mixed cover crops, based on average of legume and non-legume
tabs$ON = rbindlist(use.names = TRUE, list(
  data.table(Cover.Crop = 'None', Moisture = 'All', Delta.ON = 0.0),
  tabs$ON, 
  tabs$ON[, .(Cover.Crop = 'Mixed', Delta.ON = mean(Delta.ON)), Moisture]))
tabs$f_up = rbindlist(use.names = TRUE, list(
  tabs$f_up, 
  tabs$f_up[Cover.Crop %in% c('Legume', 'Non-legume'), .(Cover.Crop = 'Mixed', f_up = mean(f_up)), Tillage]))
tabs$CO2_CC = rbindlist(use.names = TRUE, list(
  data.table(Cover.crop = 'None', Tillage.Practice = 'All', Delta.CO2_I = 0.0, Delta.CO2_F = 0.0),
  tabs$CO2_CC, 
  tabs$CO2_CC[Cover.crop %in% c('Legume', 'Non-legume'), 
              .(Cover.crop = 'Mixed', Delta.CO2_I = mean(Delta.CO2_I), Delta.CO2_F = mean(Delta.CO2_F)), Tillage.Practice]))
tabs$CO2_T = rbind(tabs$CO2_T, list('Conventional', 'All', 0, 0))
tabs$yield_T = rbind(tabs$yield_T, data.table('Conventional', 'All', 'All', 'All', 0.0), use.names=FALSE)

tabs$yield_T = tabs$yield_T[Rotation != "Maize-Soybean"][, Rotation:=NULL]
tabs = tabs[!grepl("county|state|N_opt|f_e|SOC_rev|leakage|f_ONi", names(tabs))]
lapply(tabs, function(tab) {
  if ("Cover.Crop" %in% names(tab)) setnames(tab, "Cover.Crop" , "Cover.crop" )
  if ("Tillage.Practice" %in% names(tab)) setnames(tab, "Tillage.Practice" , "Tillage" )
})

export_path = "tabs/"

lapply(names(tabs), function(tab) {
  fwrite(tabs[[tab]], paste0(export_path, tab, ".csv"))
})

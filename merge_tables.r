library(data.table)
tables.path = 'tables/'
counties = fread(paste0(tables.path, 'county.csv'))[, .(state = STATE, county = NAME)]
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
tabs$county[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]

# Add keys to all tables
alltabs = tabs[!grepl("county|state|N_opt|f_e|SOC_rev|leakage|f_ONi", names(tabs))]
lapply(alltabs, function(tab) {
  if ("Cover.Crop" %in% names(tab)) setnames(tab, "Cover.Crop" , "Cover.crop" )
  if ("Tillage.Practice" %in% names(tab)) setnames(tab, "Tillage.Practice" , "Tillage" )
})
keys = c("Cover.crop", "Tillage", "Crop", "Temperature", "Moisture", "Soil")
lapply(alltabs, function (tab) {
  tab[, setdiff(keys, names(tab)) := NA]
  tab[, (keys) := replace(.SD, .SD == "All", NA), .SDcols = keys]
  tab[, (keys) := replace(.SD, .SD == "Soy", "Soybean"), .SDcols = keys]
  setkeyv(tab, keys)
})

# Merge all parameter tables
key_vals = rbindlist(alltabs, fill = TRUE)[, list((sapply(.SD, unique))), .SDcols = keys]
key_vals = sapply(key_vals$V1, function(x) x[!is.na(x)])
names(key_vals) = keys
all_keys = expand.grid(key_vals, stringsAsFactors = FALSE)
setDT(all_keys)
setkeyv(all_keys,keys)

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

mergedtabs = lapply(alltabs, fill_match, all_keys, keys)
mymerge = function(x,y) merge.data.table(x, y, all=TRUE) 
mergedtabs = Reduce(mymerge, mergedtabs)
fwrite(mergedtabs, "merged_tables_dist.csv")
mergedtabs[, (grep("^d_|^se_|^sd_|^logsd_|^sdlog_|Distribution", names(mergedtabs))) := NULL]
fwrite(mergedtabs, "merged_tables.csv")


library(data.table)
library(raster)
library(sf)
library(exactextractr)
library(matrixStats)

gis_path = "/media/dominic/DATA/gis/"
counties = st_read(paste0(gis_path, "US/tl_2018_us_county/tl_2018_us_county.shp"))
counties = st_transform(counties, "+proj=longlat +datum=WGS84 +no_defs")
contig.states = c("Alabama","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
states = st_read(paste0(gis_path, "US/tl_2018_us_state/tl_2018_us_state.shp"))
states = st_transform(states, "+proj=longlat +datum=WGS84 +no_defs")
states = states[states$NAME %in% contig.states,]
counties = counties[counties$STATEFP %in% states$STATEFP, ]
states$STATEFP    = as.integer(states$STATEFP)
counties$STATEFP  = as.integer(counties$STATEFP)
counties$COUNTYFP = as.integer(counties$COUNTYFP)

if (!file.exists('fast-ghg-counties.GPKG')){
  # merge model results with county polygons
  counties = merge(counties, totals[, -"NAME"], by = c('STATEFP', 'COUNTYFP'))

  #### merge total cropland area with county polygons 
  if (!file.exists('croparea.tif')) {
    lc_path = "/home/dominic/Box Sync/projects/SoilsRevealed_IPCC_shared/gis/hi_res/250m"
    cropland = rast(paste0(lc_path, '/lc_cropland.tif'))
    cropland = crop(cropland, counties)
    croparea = cropland * area(cropland, sum=FALSE)
    croparea = croparea / 1e4 # sq meter to ha
    writeRaster(croparea, 'croparea.tif')
  } else {
    croparea = raster('croparea.tif')
  }
  county_cropland_sum = exact_extract(croparea, counties, 'sum')
  counties$cropland_ha = county_cropland_sum
  counties$cropland_frac = counties$cropland_ha / (counties$ALAND / 1e4)

  # merge USDA agricultural districtcodes into counties sf 
  usda_districts = st_read('usda_agricultural_divisions/ASD_2012_500K.shp')
  usda_districts = st_transform(usda_districts, "+proj=longlat +datum=WGS84 +no_defs")
  usda_districts$DISTRICTFP  = as.integer(sub('..', '', usda_districts$STASD_A))
  counties = st_join(counties, usda_districts["DISTRICTFP"], largest=TRUE)
  counties$state_dist = paste(counties$STATEFP, counties$DISTRICTFP, sep = '_') 
  
  # save as gpkg vector file
  st_write(counties, 'fast-ghg-counties.GPKG', append=FALSE)
} else {
  counties = read_sf('fast-ghg-counties.GPKG') 
}

harvested_area = fread('HarvestedAcresCornSoyWheat/NASS_HarvestedAcres-2015-19-all.csv')
setnames(harvested_area, 
         c('State ANSI', 'County ANSI', 'Ag District', 'Ag District Code', 'Data Item'), 
         c('STATEFP', 'COUNTYFP', 'District',  'DISTRICTFP', 'Variable'))
harvested_area[, AcresNumeric := as.numeric(gsub(',', '', AcresHarvested))]
harvested_area[, ha := AcresNumeric * 0.404686]
harvested_area[, c('District', 'AcresHarvested', 'AcresNumeric') := NULL]
harvested_area = harvested_area[, .(ha = mean(ha, na.rm=T)), by = .(Commodity, Variable, STATEFP, DISTRICTFP, COUNTYFP, County)]
harvested_area[, sum(ha)/1e6, by = Commodity]
harvested_area = harvested_area[, .(ha = sum(ha, na.rm=T)), by = .(Commodity, STATEFP, DISTRICTFP, COUNTYFP, County)]
harvested_area[, sum(ha)/1e6, by = Commodity]
harvested_area[, state_dist := paste(STATEFP, DISTRICTFP, sep = '_')]
setkey(harvested_area, state_dist, COUNTYFP)

counties.dt = as.data.table(st_drop_geometry(counties))[, .(state_dist, COUNTYFP, cropland_ha)]
setkey(counties.dt, state_dist, COUNTYFP)

counties.harvested.corn = merge(harvested_area[Commodity=="CORN"], counties.dt, all=T)
counties.harvested.corn[, Commodity:=NULL]
counties.harvested.corn[is.na(COUNTYFP) | is.na(County), other.ha := max(c(0,ha), na.rm=T), by = .(state_dist)]
counties.harvested.corn[is.na(County), wt := pmax(0, cropland_ha, na.rm=T) / sum(c(0, cropland_ha), na.rm=T), by = state_dist]
counties.harvested.corn[, sw := sum(wt, na.rm=T), state_dist]
counties.harvested.corn[sw==0, other.ha := max(c(0, other.ha), na.rm=T), by = .(state_dist)]
counties.harvested.corn[sw==0 & !grepl('OTHER', County), wt := pmax(0, cropland_ha, na.rm=T) / sum(c(0, cropland_ha), na.rm=T), by = state_dist]
counties.harvested.corn[, filled.ha := wt * other.ha]
counties.harvested.corn = counties.harvested.corn[!is.na(COUNTYFP)]
counties.harvested.corn[, corn.ha := rowSums(cbind(filled.ha, ha), na.rm=TRUE)]
counties.harvested.corn = counties.harvested.corn[,.(state_dist, COUNTYFP, corn.ha)]
counties.dt = counties.dt[counties.harvested.corn]

counties.harvested.wheat = merge(harvested_area[Commodity=="WHEAT"], counties.dt, all=T)
counties.harvested.wheat[, Commodity:=NULL]
counties.harvested.wheat[is.na(COUNTYFP) | is.na(County), other.ha := max(c(0,ha), na.rm=T), by = .(state_dist)]
counties.harvested.wheat[is.na(County), wt := pmax(0, cropland_ha, na.rm=T) / sum(c(0, cropland_ha), na.rm=T), by = state_dist]
counties.harvested.wheat[, sw := sum(wt, na.rm=T), state_dist]
counties.harvested.wheat[sw==0, other.ha := max(c(0, other.ha), na.rm=T), by = .(state_dist)]
counties.harvested.wheat[sw==0 & !grepl('OTHER', County), wt := pmax(0, cropland_ha, na.rm=T) / sum(c(0, cropland_ha), na.rm=T), by = state_dist]
counties.harvested.wheat[, filled.ha := wt * other.ha]
counties.harvested.wheat = counties.harvested.wheat[!is.na(COUNTYFP)]
counties.harvested.wheat[, wheat.ha := rowSums(cbind(filled.ha, ha), na.rm=TRUE)]
counties.harvested.wheat = counties.harvested.wheat[,.(state_dist, COUNTYFP, wheat.ha)]
counties.dt = counties.dt[counties.harvested.wheat]

counties.harvested.soy = merge(harvested_area[Commodity=="SOYBEANS"], counties.dt, all=T)
counties.harvested.soy[, Commodity:=NULL]
counties.harvested.soy[is.na(COUNTYFP) | is.na(County), other.ha := max(c(0,ha), na.rm=T), by = .(state_dist)]
counties.harvested.soy[is.na(County), wt := pmax(0, cropland_ha, na.rm=T) / sum(c(0, cropland_ha), na.rm=T), by = state_dist]
counties.harvested.soy[, sw := sum(wt, na.rm=T), state_dist]
counties.harvested.soy[sw==0, other.ha := max(c(0, other.ha), na.rm=T), by = .(state_dist)]
counties.harvested.soy[sw==0 & !grepl('OTHER', County), wt := pmax(0, cropland_ha, na.rm=T) / sum(c(0, cropland_ha), na.rm=T), by = state_dist]
counties.harvested.soy[, filled.ha := wt * other.ha]
counties.harvested.soy = counties.harvested.soy[!is.na(COUNTYFP)]
counties.harvested.soy[, soy.ha := rowSums(cbind(filled.ha, ha), na.rm=TRUE)]
counties.harvested.soy = counties.harvested.soy[,.(state_dist, COUNTYFP, soy.ha)]
counties.dt = counties.dt[counties.harvested.soy]

counties = merge(counties, 
                 counties.dt[, .(state_dist, COUNTYFP, corn.ha, wheat.ha, soy.ha)],
                 on = c('state_dist', 'COUNTYFP'))
counties$maize_potential = counties$Maize_BMP.val * counties$corn.ha / 1000 # 1000 Mg CO2e per county
counties$wheat_potential = counties$Wheat_BMP.val * counties$wheat.ha / 1000
counties$soy_potential = counties$Soybean_BMP.val * counties$soy.ha / 1000

# save bmp results as csv
bmp = st_drop_geometry(counties)
setDT(bmp)
bmp[, c("COUNTYNS", "GEOID", 
        "NAMELSAD", "LSAD", "CLASSFP", "MTFCC", "CSAFP", "CBSAFP", "METDIVFP", 
        "FUNCSTAT", "INTPTLAT", "INTPTLON", 
        "Maize_Legume_Conventional", "Maize_Legume_No.till", "Maize_Legume_Reduced.till", 
        "Maize_Non.legume_Conventional", "Maize_Non.legume_No.till", 
        "Maize_Non.legume_Reduced.till", "Maize_None_Conventional", "Maize_None_No.till", 
        "Maize_None_Reduced.till", "Soybean_Legume_Conventional", "Soybean_Legume_No.till", 
        "Soybean_Legume_Reduced.till", "Soybean_Non.legume_Conventional", 
        "Soybean_Non.legume_No.till", "Soybean_Non.legume_Reduced.till", 
        "Soybean_None_Conventional", "Soybean_None_No.till", "Soybean_None_Reduced.till", 
        "Wheat_Legume_Conventional", "Wheat_Legume_No.till", "Wheat_Legume_Reduced.till", 
        "Wheat_Non.legume_Conventional", "Wheat_Non.legume_No.till", 
        "Wheat_Non.legume_Reduced.till", "Wheat_None_Conventional", "Wheat_None_No.till", 
        "Wheat_None_Reduced.till") := NULL]
bmp[, Maize_BMP := sub('Maize_', '', Maize_BMP)]
bmp[, Wheat_BMP := sub('Wheat_', '', Wheat_BMP)]
bmp[, Soybean_BMP := sub('Soybean_', '', Soybean_BMP)]
fwrite(bmp, 'bmp.csv')

# Summary results
# total mitigation potential in US from corn, wheat and soy
(tot.maize = sum(counties$maize_potential, na.rm = T))
(tot.wheat = sum(counties$wheat_potential, na.rm = T))
(tot.soy = sum(counties$soy_potential, na.rm = T))
(tot = (tot.maize + tot.wheat + tot.soy) / 1000) # Mt Co2e
usa.co2 = 5.1e3 # current Mt Co2 per year
paste("Mitigation potential =", round(tot / usa.co2 * 100, 2), "% of current emissions") # mitigation potential as percent of emissions
sum(counties$cropland_ha, na.rm = T) / 1e6
sum(counties$corn.ha, na.rm = T) / 1e6
sum(counties$soy.ha, na.rm = T) / 1e6
sum(counties$wheat.ha, na.rm = T) / 1e6
harvested_area[, sum(ha, na.rm=T) / 1e6, by = Commodity] 
harvested_area[, sum(ha, na.rm=T) / 1e6]
harvested_area[, sum(ha, na.rm=T)] / counties.dt[, sum(cropland_ha)]



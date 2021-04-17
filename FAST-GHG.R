FAST_GHG = function (results) {
  # exclude results for cover crops in dry climate
  results = results[Cover.crop == 'None' | Moisture == 'Moist'] 
  
  # Initialise columns to correct units and factors
  results[Cover.crop == 'None', F_Y_CC := 0.0]
  results[Cover.crop == 'None', Delta.SOC_CC := 0.0]
  results[, Rotation := ifelse(Crop == 'Maize', 'Continuous Maize', 'Other')]
  results[, clay := clay/100]     # convert from percent to fraction
  results[, F_Y_CC := F_Y_CC/100] # convert from percent to fraction
  results[, F_Y_T := F_Y_T/100]   # convert from percent to fraction
  results[, F_i := F_i/1000]      # convert units to Mg Co2e / kg grain
  results[, F_p := F_p/1000]      # convert units to Mg Co2e / kg grain
  
  # Table 11: Direct N2O emission factor for mineral fertilizer
  results[,                                      f_Nd := 0.0079] # kg N2O/ kg N
  results[Moisture == 'Moist',                   f_Nd := 0.0251]
  results[Crop == 'Maize' & Moisture == 'Moist', f_Nd := 0.085 * clay]
  # Amount of N additions available to potentially leach
  results[,                  Nrate.leach := Nrate]
  results[Crop == 'Soybean', Nrate.leach := Nrate + 60]
  # Table 17: Change in N leaching due to practice
  results[Moisture == 'Dry',                                Delta.L := 0.0]
  results[Moisture == 'Moist',                              Delta.L := c(0.06,0.03,0.0)[match(Tillage, c("No-till", "Reduced-till", "Conventional"))]  * Nrate.leach]
  results[Moisture == 'Moist' & Cover.crop == 'Legume',     Delta.L := 0.1  * Nrate.leach]
  results[Moisture == 'Moist' & Cover.crop == 'Non-legume', Delta.L := 0.13 * Nrate.leach]
  # Eq. 5: change in yield (kg grain / ha / yr)
  results[, Delta.Y := Yield * (F_Y_CC + F_Y_T)] 
  # Table 15 (change in N input rate from each group of practices; kg N / ha / yr):
  results[,                           Delta.N_A := 0.0]
  results[Cover.crop == 'Legume',     Delta.N_A := 52 * constants$f_NUE -72 * Delta.SOC_CC - Delta.Y * f_Ng] # kg N / ha / yr 
  results[Cover.crop == 'Non-legume', Delta.N_A := (Delta.L - 22 - Delta.Y * f_Ng)]    # kg N / ha / yr
  results[,                           Delta.N_B := -72 * Delta.SOC_T]
  results[Cover.crop == 'None',       Delta.N_B := Delta.N_B + Delta.L - Delta.Y * f_Ng]
  results[,                           Delta.N_tot := Delta.N_A + Delta.N_B]
  # Equation 9: Change in CO2-equivalent GHG emissions from nitrogen fertilizer production (Mg CO2e / ha / yr).
  results[, Delta.CO2_N := Delta.N_tot * constants$f_Np]
  # Equation 8: Change in indirect nitrous oxide emissions (kg N2O / ha / yr)
  # results[, Delta.N2O_i := Delta.N_tot * (f_Nv + (1-f_up)*f_Nl) + Delta.ON * (f_ONv + (1-f_up)*f_ONl) + f_Nl * Delta.L]
  results[, Delta.N2O_i := Delta.N_tot * (f_Nv + (1-f_up)*f_Nl) + Delta.ON * f_Oni + f_Nl * Delta.L]
  # Equation 7: Change in direct nitrous oxide emissions (kg N2O / ha / yr).
  results[, Delta.N2O_d := Delta.N_tot * f_Nd + Delta.ON * f_ONd]
  # Eq. 6: Change in total nitrous oxide emissions (Mg N2O)
  results[, Delta.N2O := (Delta.N2O_d + Delta.N2O_i) / 1000]
  # Eq. 4:  Note that ILUC is reversible, so we also apply the permanence-risk factor to it
  results[, Delta.CO2_L := Delta.Y * (F_p + (1-constants$f_i)*F_i*(1 - R))]
  # Eq. 3:
  results[, Delta.SOC := f_100_CC * Delta.SOC_CC + f_100_T * Delta.SOC_T] # average annual change in SOC, over 100 years
  # Eq. 2:
  results[, Delta.CO2_SOC := Delta.SOC * (1 - R) * 44/12]
  results[, Delta.CO2 := Delta.CO2_SOC + Delta.CO2_F_T + Delta.CO2_F_CC + Delta.CO2_I_T + Delta.CO2_I_CC + Delta.CO2_N + Delta.CO2_L]
  # Eq. 1:
  results[, Delta.CO2e_N2O := constants$GWP_N2O * Delta.N2O]
  results[, Delta.GHG := Delta.CO2 + Delta.CO2e_N2O]
  
  GHG.labs = c(Delta.CO2_SOC  = 'SOC', 
               Delta.CO2_F_T  = 'Fuel (tillage)', 
               Delta.CO2_F_CC = 'Fuel (cover crops)', 
               Delta.CO2_I_T  = 'Inputs (tillage)', 
               Delta.CO2_I_CC = 'Inputs (cover crops)', 
               Delta.CO2_L    = 'Leakage', 
               Delta.CO2_N    = 'N fertilizer production', 
               Delta.CO2e_N2O = 'N2O emissions', 
               Delta.GHG      = 'Total')
  
  all_fast = melt.data.table(results, measure.vars = names(GHG.labs), 
                             id.vars = c("STATEFP", "STATE", "COUNTYFP", "NAME", "ALAND", "AWATER", 
                                         "Temperature", "Moisture", "Soil", "clay", 
                                         "Crop", "Cover.crop", "Tillage"))
  all_fast[, label := GHG.labs[variable]]
  setkey(all_fast, STATEFP, COUNTYFP, Crop, Cover.crop, Tillage, variable)
  return(all_fast)
}

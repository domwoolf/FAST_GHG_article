library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else warning("Make sure this script is run from its source directory")

# total mitigation potential from all crops combined
counties$tot_potential = rowSums(st_drop_geometry(counties)[, c("maize_potential", "wheat_potential", "soy_potential")], na.rm = TRUE)

# Maps of total mitigation by county and crop
my_scale = scale_fill_viridis_c(expression(Gg~CO[2]*e~per~county),
                                trans = "sqrt",
                                limits = c(0, 120),
                                breaks = c(0,15,30,60,90,120),
                                na.value = "grey75",
                                guide = guide_colorbar(barwidth = 6, 
                                                       barheight = 0.5,  
                                                       direction='horizontal', 
                                                       title.position = 'top',
                                                       title.theme = element_text(size=8),
                                                       label.theme = element_text(size=6)))
my_theme = theme(legend.position="none",
                 axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank())

tot_map = ggplot(counties) +
  geom_sf(aes(fill = tot_potential), size = 0.1) +
  theme(axis.line=element_blank(),
        legend.position = c(0.05,0.05),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  ) +
  my_scale
tot_maize_map = ggplot(counties) +
  geom_sf(aes(fill = maize_potential), size = 0.1) +
  my_theme +
  my_scale
tot_wheat_map = ggplot(counties) +
  geom_sf(aes(fill = wheat_potential), size = 0.1) +
  my_theme +
  my_scale
tot_soy_map = ggplot(counties) +
  geom_sf(aes(fill = soy_potential), size = 0.1) +
  my_theme +
  my_scale
tot_all_map = plot_grid(tot_map, tot_maize_map, tot_wheat_map, tot_soy_map, 
                        ncol = 2, align = "hv", labels="auto")

# ---- Maps of annual mitigation rate per ha, by county and crop
my_scale = scale_fill_viridis_c(expression(Mg~CO[2]*e~ha^{-1}~yr^{-1}),
                                # trans = "sqrt",
                                limits = c(0.0, 0.52),
                                breaks = c(0.0,0.1,0.2,0.3,0.4,0.5),
                                na.value = "grey75",
                                guide = guide_colorbar(barwidth = 6, 
                                                       barheight = 0.5,  
                                                       direction='horizontal', 
                                                       title.position = 'top',
                                                       title.theme = element_text(size=7),
                                                       label.theme = element_text(size=5)))
my_theme = theme(legend.position="none",
                 axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 plot.margin = unit(c(0,0,0,0), "cm"))

val_maize_map = ggplot(counties) +
  geom_sf(aes(fill = Maize_BMP.val), size = 0.1) +
  my_theme +
  my_scale
val_wheat_map = ggplot(counties) +
  geom_sf(aes(fill = Wheat_BMP.val), size = 0.2) +
  my_theme +
  theme(legend.position = c(0.0,0.0)) +
  my_scale
val_soy_map = ggplot(counties) +
  geom_sf(aes(fill = Soybean_BMP.val), size = 0.2) +
  my_theme +
  my_scale
val_all_map = plot_grid(val_maize_map, val_wheat_map, val_soy_map, 
                        ncol = 3, align = "hv", labels="auto")
        
# ----- maps of BMP, by county and crop
# Create pretty labels of BMP practices
lab.levels = c("Non-legume, Conventional", "Legume, Conventional", "Legume, Reduced-till", 
               "Legume, No-till", "None, No-till", "None, Conventional")
lab.labels = c("Non-legume", "Legume", "Legume, Reduced-till", "Legume, No-till", "No-till", "Conventional")
make_lab = function(x) {
  bmp = st_drop_geometry(counties[paste0(x, "_BMP")])[,1]
  lab = sapply(strsplit(bmp, '_'), function(bmp) paste0(bmp[2:3], collapse=', '))
  lab[grep("NA", lab)] = NA_character_
  lab = lab.labels[match(lab, lab.levels)]
  lab = factor(lab, levels = lab.labels)
}
counties$Maize_BMP.lab = make_lab("Maize")
counties$Wheat_BMP.lab = make_lab("Wheat")
counties$Soybean_BMP.lab = make_lab("Soybean")

my_scale = scale_fill_viridis_d("", breaks = lab.labels,
                                drop=FALSE,
                                na.value = "grey75",
                                guide = guide_legend(nrow=1, byrow=TRUE))
my_theme = theme(legend.position="none",
                 legend.title = element_blank(), 
                 legend.text = element_text(size=5, margin = margin(r = 10, unit = "pt")),
                 legend.key.height = unit(0.1,"in"),
                 legend.key.width = unit(0.2,"in"),
                 legend.text.align = 0,
                 legend.spacing.x = unit(0.05,"in"),
                 axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 plot.margin = unit(c(0,0,0,0), "cm"))
bmp_maize_map = ggplot(counties) +
  geom_sf(aes(fill = Maize_BMP.lab), size = 0.1) +
  my_theme +
  theme(legend.title = element_blank(), legend.position = c(0.25,0)) + 
  my_scale
bmp_wheat_map = ggplot(counties) +
  geom_sf(aes(fill = Wheat_BMP.lab), size = 0.1) +
  my_theme +
  # theme(legend.title = element_blank(), legend.position = 'bottom') +
  my_scale 
bmp_soy_map = ggplot(counties) +
  geom_sf(aes(fill = Soybean_BMP.lab), size = 0.1) +  
  my_theme +
  # theme(legend.title = element_blank(), legend.position = 'bottom') +
  my_scale
bmp_all_map = plot_grid(bmp_maize_map, bmp_wheat_map, bmp_soy_map, 
                        ncol = 3, align = "hv", labels="auto")
bmp_ha_all_map = plot_grid(
  val_maize_map, val_wheat_map, val_soy_map, 
  bmp_maize_map, bmp_wheat_map, bmp_soy_map, 
  ncol = 3, align = "hv", labels="auto")


# Plot maps
if (plot_results) {
  print(tot_all_map)
  print(bmp_ha_all_map)
  # print(tot_map)
  # print(tot_maize_map)
  # print(tot_wheat_map)
  # print(tot_soy_map)
  # print(val_all_map)
  # print(val_maize_map)
  # print(val_wheat_map)
  # print(val_soy_map)
  # print(bmp_all_map)
  # print(bmp_maize_map)
  # print(bmp_wheat_map)
  # print(bmp_soy_map)
}

# Save maps to image file
if (save_results) {
  ggsave2('results/total_potential.png', tot_all_map,    width = 6, height = 3.5)
  ggsave2('results/bmp_ha.png',          bmp_ha_all_map, width = 6, height = 3.5)
  # ggsave2('results/ha_potential.png',    val_all_map,    width = 6, height = 1.75)
  # ggsave2('results/bmp.png',          bmp_all_map, width = 6, height = 1.75)
  # ggsave2('total_potential_vect.png', tot_map,       width = 6, height = 5)
  # ggsave2('maize_potential_vect.png', tot_maize_map, width = 6, height = 5)
  # ggsave2('wheat_potential_vect.png', tot_wheat_map, width = 6, height = 5)
  # ggsave2('soy_potential_vect.png',   tot_soy_map,   width = 6, height = 5)
  # ggsave2('Maize_BMP_val_vect.png',   val_maize_map, width = 6, height = 4)
  # ggsave2('Wheat_BMP_val_vect.png',   val_wheat_map, width = 6, height = 4)
  # ggsave2('Soybean_BMP_val_vect.png', val_soy_map,   width = 6, height = 4)
  # ggsave2('Maize_BMP_vect.png',       bmp_maize_map, width = 6, height = 4)
  # ggsave2('Wheat_BMP_vect.png',       bmp_wheat_map, width = 6, height = 4)
  # ggsave2('Soybean_BMP_vect.png',     bmp_soy_map,   width = 6, height = 4)
}

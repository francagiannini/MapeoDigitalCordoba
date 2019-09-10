source("Functions/FuncLib.R")

limiteProv <- read_sf("Datos/provCba/prov_cba_cg.shp")
baseDatos <- read.table("Datos/suelos.txt", header = TRUE)
baseDatos <- st_as_sf(baseDatos, coords = c("Xt","Yt"),  crs = 32720)

library(ggsn)
ggplot(baseDatos) +
  # annotation_map_tile(zoom = 10) +
  geom_sf(data = limiteProv, fill = NA, size = 0.4, color = "black") +
  geom_sf() + 
  # scalebar(baseDatos, dist = 50, dist_unit = "km",
           # transform = FALSE,height = 0.018, st.dist = 0.02, st.bottom = FALSE, st.size = 3) +
  # north(baseDatos, symbol = 3)
theme_map(baseDatos)


  
  
  # annotation_scale(location = "bl", bar_cols = c("black", "white"), 
  #                   height = unit(0.25,"cm"),
  #                  pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
  #                  text_pad = unit(0.15, "cm"), text_cex = 0.8, width_hint = 0.25)
  # 



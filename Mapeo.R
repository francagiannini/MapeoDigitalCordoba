source("Functions/FuncLib.R")

limiteProv <- read_sf("Datos/provCba/prov_cba_cg.shp")
baseDatos <- read.table("Datos/suelos.txt", header = TRUE)
baseDatos <- st_as_sf(baseDatos, coords = c("Xt","Yt"),  crs = 32720)

ggplot(baseDatos) +
  # annotation_map_tile(zoom = 10) +
  geom_sf(data = limiteProv, fill = NA, size = 0.4, color = "grey40") +
  geom_sf() +
  theme_map(baseDatos)


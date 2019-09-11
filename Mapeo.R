source("Functions/FuncLib.R")

######
limiteProv <- read_sf("Datos/provCba/prov_cba_cg.shp")
muestreoSuelo <- read.table("Datos/suelos.txt", header = TRUE)
muestreoSuelo <- st_as_sf(muestreoSuelo, coords = c("Xt","Yt"),  crs = 32720)


######
predichosModelosAtrazina <- read.table("Datos/predichosModelos/pred_tmediavsKd_capIII_IV.txt", header = T)
predichosModelosAtrazina <- st_as_sf(predichosModelosAtrazina, coords = c("Xt","Yt"),  crs = 32720)


ggplot(muestreoSuelo) +
  # annotation_map_tile(zoom = 10) +
  geom_sf(data = limiteProv, fill = NA, size = 0.4, color = "grey40") +
  geom_sf() +
  theme_map(limiteProv)

predichosModelosAtrazina <- dentroDe(predichosModelosAtrazina, limiteProv)
ggplot(predichosModelosAtrazina) +
  # annotation_map_tile(zoom = 10) +
  geom_sf(data = limiteProv, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = tmedia)) +
  theme_map(predichosModelosAtrazina) +
  scale_color_viridis_c(direction = -1)

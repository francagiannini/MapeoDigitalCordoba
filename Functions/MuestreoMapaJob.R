#### Carga paquetes y funciones ----
source("Functions/FuncLib.R")

#### Paramteros graficos
ANCHO <- 15.92
ALTO <- 10
#### Lectura de archivos -----
limitesArg <- read_sf("Datos/limites_arg/PROVINCIAS.shp")
limiteProv <- read_sf("Datos/provCba/prov_cba_cg.shp")

muestreoSuelo <- read.table("Datos/suelos.txt", header = TRUE)
muestreoSuelo <- st_as_sf(muestreoSuelo, coords = c("Xt","Yt"),  crs = 32720)


######
predichosModelos <- read.table("Datos/predichosModelos/pre_todo.txt", header = T)
predichosModelos <- st_as_sf(predichosModelos, coords = c("Xt","Yt"),  crs = 32720)
predichosModelos <- dentroDe(predichosModelos, limiteProv)

muestreoPlotMapa  <- ggplot(muestreoSuelo) +
  annotation_map_tile(zoom = 12) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf() +
  theme_map(muestreoSuelo)
save(muestreoPlotMapa, file = "Plots/muestreo.RData")
ggsave("Plots/muestreoMapabg.tiff", plot =  muestreoPlotMapa, device = "tiff", width = ANCHO, height = 11, units = "cm")
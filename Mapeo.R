#### Carga paquetes y funciones ----
source("Functions/FuncLib.R")

#### Paramteros graficos
GUARDARPLOT <- FALSE
VERPLOT <- TRUE
ANCHO <- 15.92
ALTO <- 11
#### Lectura de archivos -----
limitesArg <- read_sf("Datos/limites_arg/PROVINCIAS.shp")
limiteProv <- read_sf("Datos/provCba/prov_cba_cg.shp")

muestreoSuelo <- read.table("Datos/suelos.txt", header = TRUE)
muestreoSuelo <- st_as_sf(muestreoSuelo, coords = c("Xt","Yt"),  crs = 32720)


######
predichosModelos <- read.table("Datos/predichosModelos/pre_todo.txt", header = T)
predichosModelos <- st_as_sf(predichosModelos, coords = c("Xt","Yt"),  crs = 32720)
predichosModelos <- dentroDe(predichosModelos, limiteProv)


#### Puntos muestreo Plot ----
muestreoPlot  <- ggplot(muestreoSuelo) +
  # annotation_map_tile(zoom = 17) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf() +
  theme_map(muestreoSuelo)

if(VERPLOT) muestreoPlot
if(GUARDARPLOT) {ggsave("Plots/muestreo.tiff", plot = muestreoPlot, device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# muestreoPlotMapa  <- ggplot(muestreoSuelo) +
#   # annotation_map_tile(zoom = 17) +
#   geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
#   geom_sf() +
#   theme_map(muestreoSuelo)
# 
# if(VERPLOT) muestreoPlotMapa
# if(GUARDARPLOT) {ggsave("Plots/muestreoMapabg.tiff", plot =  muestreoPlotMapa, device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

#### Puntos Predichos Plot Kda_t_ratio ----
atrazinaPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Kda)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1)# +
  # labs(color = "Vida media (días)")

atrazinaPlotSD <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = sd_Kda)) +
  theme_map(predichosModelos) +
  scale_color_gradient( high = "#E129DB", low = "#F2F2F2")
  scale_color_gradient2( low = "#E129DB", mid = "#E1297F", high = "#F2F2F2")
  # scale_color_viridis_c(direction = -1)# +
  # labs(color = "D.E. Vida media (días)")


plotAtrazinaPredIC <- ggarrange(atrazinaPlot, atrazinaPlotSD,
          ncol = 1)

if(VERPLOT) plotAtrazinaPredIC
if(GUARDARPLOT) {ggsave("Plots/plotAtrazinaPredIC.tiff", plot =  plotAtrazinaPredIC, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}




#####################################
limitesArg <- read_sf("Datos/limites_arg/PROVINCIAS.shp")
# limitesArg[limitesArg$NAM == "CÓRDOBA", ]

datosCelia <- read.table("Datos/interpolacionCelia/CMAP_puntos.txt", sep = "\t", header = TRUE)
datosCelia <- st_as_sf(datosCelia, coords = c("Xt","Yt"),  crs = 32720 )
limiteProv <- st_transform(limiteProv, crs = st_crs(datosCelia))


plotCeliaPtosMuestreo <- ggplot(datosCelia) +
  geom_sf(data = limitesArg, fill = "#f5f5f2") +
  # annotation_map_tile(zoom = 10) +
  # geom_sf(data = limiteProv, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(data = datosCelia, aes(color = CMAP..ug.g.1.), size = 5) +
  theme_map(datosCelia) +
  scale_color_viridis_c(direction = -1,
                        limits = c(
                          0,
                          max(
                            max(predichosCelia$Predichos),
                            max(datosCelia$CMAP..ug.g.1.))) ) +
  coord_sf(xlim = st_bbox(limiteProv)[c(1,3)],
           ylim = st_bbox(limiteProv)[c(2,4)]) +
  labs(color =  bquote("CMAP (" *mu*g ~ g^-1*")"))

ggsave("Plots/PtosMuestreoCelia.tiff",plot = plotCeliaPtosMuestreo, device = "tiff", width = 25, height = 33, units = "cm")


predichosCelia <- read.table("Datos/interpolacionCelia/interpol_CMAP2.txt", sep = "\t", header = TRUE)
predichosCelia <- st_as_sf(predichosCelia, coords = c("X","Y"),  crs = 32720 )
predichosCelia$Predichos[predichosCelia$Predichos < 0] <- 0



plotCeliaPtosPredichos <- ggplot(predichosCelia) +
  geom_sf(data = limitesArg, fill = "#f5f5f2") +
  # annotation_map_tile(zoom = 10) +
  # geom_sf(data = limiteProv, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(data = predichosCelia, aes(color = Predichos), size = 2) +
  theme_map(predichosCelia) +
  scale_color_viridis_c(direction = -1,
                        limits = c(
    0,
    max(
      max(predichosCelia$Predichos),
      max(datosCelia$CMAP..ug.g.1.)))) +
  coord_sf(xlim = st_bbox(limiteProv)[c(1,3)],
           ylim = st_bbox(limiteProv)[c(2,4)]) +
  labs(color =  bquote("CMAP (" *mu*g ~ g^-1*")"))

ggsave("Plots/PtosPredichosCelia.tiff",plot = plotCeliaPtosPredichos, device = "tiff", width = 25, height = 33, units = "cm")

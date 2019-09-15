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

#### Puntos Predichos Plot Kda ----
kdaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Kda)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1)# +
  # labs(color = "Vida media (días)")

kdaPredPlotSD <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = sd_Kda)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")# +
  # labs(color = "D.E. Vida media (días)")

kdaPredPlotIC95 <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = IC95_Kda)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "cividis")# +
# labs(color = "D.E. Vida media (días)")

plotKdaPredSD <- ggarrange(kdaPredPlot, kdaPredPlotSD,
                                ncol = 1)

if(VERPLOT) plotKdaPredSD
if(GUARDARPLOT) {ggsave("Plots/Kda_Pred_SD.tiff", plot =  plotKdaPredSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}

plotKdaPredICSD <- ggarrange(kdaPredPlot, kdaPredPlotSD, kdaPredPlotIC95,
                                  ncol = 2, nrow = 2)
if(VERPLOT) plotKdaPredICSD
if(GUARDARPLOT) {ggsave("Plots/Kda_Pred_SD_IC.tiff", plot =  plotKdaPredICSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}



#### Puntos Predichos Plot Kdg ----
KdgPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Kdg)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1)# +
# labs(color = "Vida media (días)")

KdgPredPlotSD <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = sd_Kdg)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")# +
# labs(color = "D.E. Vida media (días)")

KdgPredPlotIC95 <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = IC95_Kdg)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "cividis")# +
# labs(color = "D.E. Vida media (días)")

plotKdgPredSD <- ggarrange(KdgPredPlot, KdgPredPlotSD,
                              ncol = 1)

if(VERPLOT) plotKdgPredSD
if(GUARDARPLOT) {ggsave("Plots/Kdga_Pred_SD.tiff", plot =  plotKdgPredSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}

plotKdgPredICSD <- ggarrange(KdgPredPlot, KdgPredPlotSD, KdgPredPlotIC95,
                                ncol = 2, nrow = 2)
if(VERPLOT) plotKdgPredICSD
if(GUARDARPLOT) {ggsave("Plots/Kdg_Pred_SD_IC.tiff", plot =  plotKdgPredICSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}


#### Puntos Predichos Plot tmedia ----
tmediaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = tmedia)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1)# +
# labs(color = "Vida media (días)")

tmediaPredPlotSD <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = sd_tmedia)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")# +
# labs(color = "D.E. Vida media (días)")

tmediaPredPlotIC95 <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = IC95_tmedia)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "cividis")# +
# labs(color = "D.E. Vida media (días)")

plottmediaPredSD <- ggarrange(tmediaPredPlot, tmediaPredPlotSD,
                           ncol = 1)

if(VERPLOT) plottmediaPredSD
if(GUARDARPLOT) {ggsave("Plots/tmedia_Pred_SD.tiff", plot =  plottmediaPredSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}

plottmediaPredICSD <- ggarrange(tmediaPredPlot, tmediaPredPlotSD, tmediaPredPlotIC95,
                             ncol = 2, nrow = 2)
if(VERPLOT) plottmediaPredICSD
if(GUARDARPLOT) {ggsave("Plots/tmedia_Pred_SD_IC.tiff", plot =  plottmediaPredICSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}




#### Puntos Predichos GUS ----
GUSPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = GUS)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1)# +
# labs(color = "Vida media (días)")



if(VERPLOT) GUSPredPlot
if(GUARDARPLOT) {ggsave("Plots/GUSPredPlot_Pred.tiff", plot =  GUSPredPlot, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}


#### Puntos Predichos Kda_t_ratio  ----
Kda_t_ratioPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Kda_t_ratio)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1)# +
# labs(color = "Vida media (días)")



if(VERPLOT) Kda_t_ratioPredPlot
if(GUARDARPLOT) {ggsave("Plots/Kda_t_ratioPredPlot_Pred.tiff", plot =  Kda_t_ratioPredPlot, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}

# Capítulo VI
# Mapa Zonas sobre KOCa
# Mapa Zonas sobre KOCg 
# Mapa Zonas sobre vida media
# Mapa Zonas sobre GUS
# Mapa Zonas sobre KOCa 
# Mapa catKOCa ----
CATKOCaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = CATKOCa)) +
  theme_map(predichosModelos) +
  scale_color_viridis_d(direction = -1)# +
# labs(color = "Vida media (días)")

if(VERPLOT) CATKOCaPredPlot
if(GUARDARPLOT) {ggsave("Plots/CATKOCaPredPlot_Pred.tiff", plot =  CATKOCaPredPlot, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}


# Mapa catKOCg  ----
# Mapa catvidamedia ----
# Mapa catGUS ----
CATGUSPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = CATGUS)) +
  theme_map(predichosModelos) +
  scale_color_viridis_d(direction = -1)# +
# labs(color = "Vida media (días)")

if(VERPLOT) CATGUSPredPlot
if(GUARDARPLOT) {ggsave("Plots/CATGUSPredPlot_Pred.tiff", plot =  CATGUSPredPlot, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}



# Mapa limitante dren ----
# Mapa pp ----
# Mapa vientos ----
# Mapa riesgo erosión hidrica ----
# Mapa riesgo de erosión eólica ----
# Mapa presencia de bt ----

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

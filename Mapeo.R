#### Carga paquetes y funciones ----
source("Functions/FuncLib.R")

#### Paramteros graficos
GUARDARPLOT <- TRUE
VERPLOT <- FALSE
ANCHO <- 15.92
ALTO <- 11
#### Lectura de archivos -----
limitesArg <- read_sf("Datos/limites_arg/PROVINCIAS.shp")
limiteProv <- read_sf("Datos/provCba/prov_cba_cg.shp")

muestreoSuelo <- read.table("Datos/suelos.txt", header = TRUE)
muestreoSuelo <- st_as_sf(muestreoSuelo, coords = c("Xt","Yt"),  crs = 32720)

zonasCordoba <- read.table("Datos/zonas_capI.txt", header = TRUE, na.strings = ".")
zonasCordoba <- st_as_sf(zonasCordoba, coords = c("X","Y"),  crs = 32720)

predichosModelos <- read.table("Datos/predichosModelos/pre_todo.txt", header = T, fileEncoding = "ISO-8859-1")
predichosModelos <- st_as_sf(predichosModelos, coords = c("Xt","Yt"),  crs = 32720)
predichosModelos <- dentroDe(predichosModelos, limiteProv)

predichosModelos$CATGUS <- factor(predichosModelos$CATGUS, levels = c("Alto", "Moderado", "Bajo"))
predichosModelos$CATtmedia <- factor(predichosModelos$CATtmedia, levels = c("Alta", "Media", "Baja"))
predichosModelos$CATKOCg <- factor(predichosModelos$CATKOCg, levels = c("Elevado", "Moderado", "Débil"))

zonasCap5 <- read.table("Datos/zonas_cap_5.txt", header = TRUE, sep = "\t")
zonasCap5 <- st_as_sf(zonasCap5, coords = c("Xt","Yt"),  crs = 32720)
zonasCap5$Patron <- factor(zonasCap5$Patron, 
                           levels = c("Alto Kd y Alta vida media",
                                    "Alto Kd y baja vida media",
                                    "Baja Kd y Baja vida media",
                                    "Bajo Kd y Alta vida media",
                                    "No significativa"),
                           labels = c("Alto Kd y Alta vida media",
                                      "Alto Kd y Baja vida media",
                                      "Bajo Kd y Baja vida media",
                                      "Bajo Kd y Alta vida media",
                                      "No significativa"))


#### CAPITULO 1 ====
#### Puntos muestreo Plot ----
muestreoPlot  <- ggplot(muestreoSuelo) +
  # annotation_map_tile(zoom = 17) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf() +
  theme_map(muestreoSuelo)

if(VERPLOT) muestreoPlot
if(GUARDARPLOT) {ggsave("Plots/muestreo.tiff", plot = muestreoPlot, 
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# muestreoPlotMapa  <- ggplot(muestreoSuelo) +
#   # annotation_map_tile(zoom = 17) +
#   geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
#   geom_sf() +
#   theme_map(muestreoSuelo)
# 
# if(VERPLOT) muestreoPlotMapa
# if(GUARDARPLOT) {ggsave("Plots/muestreoMapabg.tiff", plot =  muestreoPlotMapa, device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

#### mapa de zonas a CP clásicas b CP espaciales ----
CPClasicasPlot <- ggplot(zonasCordoba) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Zones_CPAclasico, fill = Zones_CPAclasico)) +
  theme_map(zonasCordoba) +
  scale_color_viridis_d(direction = -1, option = "inferno", begin = 0.4, na.translate = F) +
  scale_fill_viridis_d(direction = -1, option = "inferno", begin = 0.4, na.translate = F) +
  labs(color = "Zona", fill =  "Zona")

CPMultispatiPlot <- ggplot(zonasCordoba) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Zones_multispati, fill = Zones_multispati)) +
  theme_map(zonasCordoba) +
  scale_color_viridis_d(direction = -1, option = "inferno", begin = 0.4) +
  scale_fill_viridis_d(direction = -1, option = "inferno", begin = 0.4) +
  labs(color = "Zona", fill =  "Zona")

plotZonasComonLegend <- ggarrange(CPClasicasPlot, CPMultispatiPlot,
                                  ncol = 2,  labels="auto", common.legend = TRUE,
                                  legend = "bottom",
                                  font.label = list(size = 11, color = "black", 
                                                    face = "plain", family = familiaTexto))


if(VERPLOT) plotZonasComonLegend
if(GUARDARPLOT) {ggsave("Plots/Zonas.tiff", plot =  plotZonasComonLegend, 
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}



#### CAPITULO 3 ====
#### Puntos Predichos Plot Kda ----


kdaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Kda)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno") +
  labs(color = expression(atop('Kd atrazina', '('*L~kg^{-1}*')')))


kdaPredPlotSD <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = sd_Kda)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "viridis") +
  labs(color = "D.E. Kd atrazina")

# kdaPredPlotIC95 <-  ggplot(predichosModelos) +
#   geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
#   geom_sf(aes(color = IC95_Kda)) +
#   theme_map(predichosModelos) +
#   scale_color_viridis_c(direction = -1, option = "cividis")# +
# # labs(color = "D.E. Vida media (días)")

plotKdaPredSD <- ggarrange(kdaPredPlot, kdaPredPlotSD,
                                ncol = 1)

if(VERPLOT) plotKdaPredSD
if(GUARDARPLOT) {ggsave("Plots/Kda_Pred_SD.tiff", plot =  plotKdaPredSD, 
                        device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}

# plotKdaPredICSD <- ggarrange(kdaPredPlot, kdaPredPlotSD, 
#                              kdaPredPlotIC95,
#                                   ncol = 2, nrow = 2)
# if(VERPLOT) plotKdaPredICSD
# if(GUARDARPLOT) {ggsave("Plots/Kda_Pred_SD_IC.tiff", plot =  plotKdaPredICSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}



#### Puntos Predichos Plot Kdg ----
KdgPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Kdg)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")+
  labs(color = expression(atop('Kd glifosato', '('*L~kg^{-1}*')')))

KdgPredPlotSD <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = sd_Kdg)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "viridis") +
  labs(color = "D.E. Kd glifosato")

# KdgPredPlotIC95 <-  ggplot(predichosModelos) +
#   geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
#   geom_sf(aes(color = IC95_Kdg)) +
#   theme_map(predichosModelos) +
#   scale_color_viridis_c(direction = -1, option = "cividis")# +
# # labs(color = "D.E. Vida media (días)")

plotKdgPredSD <- ggarrange(KdgPredPlot, KdgPredPlotSD,
                              ncol = 1)

if(VERPLOT) plotKdgPredSD
if(GUARDARPLOT) {ggsave("Plots/Kdg_Pred_SD.tiff", plot =  plotKdgPredSD, 
                        device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}

# plotKdgPredICSD <- ggarrange(KdgPredPlot, KdgPredPlotSD, KdgPredPlotIC95,
#                                 ncol = 2, nrow = 2)
# if(VERPLOT) plotKdgPredICSD
# if(GUARDARPLOT) {ggsave("Plots/Kdg_Pred_SD_IC.tiff", plot =  plotKdgPredICSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}


#### Puntos Predichos Plot KOCa ----
KOCaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = KOCa)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")+
  labs(color = "Koc atrazina")


if(VERPLOT) KOCaPredPlot
if(GUARDARPLOT) {ggsave("Plots/KocA_Pred.tiff", plot =  KOCaPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


#### Puntos Predichos Plot KOCg ----
KOCgPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = KOCg)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")+
  labs(color = "Koc gligosato")


if(VERPLOT) KOCgPredPlot
if(GUARDARPLOT) {ggsave("Plots/KocG_Pred.tiff", plot =  KOCgPredPlot, 
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}



#### CAPITULO 4 ====
#### Puntos Predichos Plot tmedia ----
tmediaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = tmedia)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno", limits = c(NA, 60)) +
  labs(color = "Vida media (días)")

tmediaPredPlotSD <-  ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = sd_tmedia)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "viridis") +
  labs(color = "D.E. Vida media")

# tmediaPredPlotIC95 <-  ggplot(predichosModelos) +
#   geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
#   geom_sf(aes(color = IC95_tmedia)) +
#   theme_map(predichosModelos) +
#   scale_color_viridis_c(direction = -1, option = "cividis")# +
# # labs(color = "D.E. Vida media (días)")

plottmediaPredSD <- ggarrange(tmediaPredPlot, tmediaPredPlotSD,
                           ncol = 1)

if(VERPLOT) plottmediaPredSD
if(GUARDARPLOT) {ggsave("Plots/tmedia_Pred_SD.tiff", plot =  plottmediaPredSD, 
                        device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}

# plottmediaPredICSD <- ggarrange(tmediaPredPlot, tmediaPredPlotSD, tmediaPredPlotIC95,
#                              ncol = 2, nrow = 2)
# if(VERPLOT) plottmediaPredICSD
# if(GUARDARPLOT) {ggsave("Plots/tmedia_Pred_SD_IC.tiff", plot =  plottmediaPredICSD, device = "tiff", width = ANCHO, height = ALTO*2, units = "cm")}


#### CAPITULO 5 ====
# Mapa GUS ----
GUSPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = GUS)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")

if(VERPLOT) GUSPredPlot
if(GUARDARPLOT) {ggsave("Plots/GUS_Pred.tiff", plot =  GUSPredPlot, device = "tiff", 
                        width = ANCHO, height = ALTO, units = "cm")}



# Mapa Lisa ----
lisaPredPlot <- ggplot(zonasCap5) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color =  Patron, fill =  Patron)) +
  theme_map(zonasCap5) +
  scale_color_viridis_d(direction = -1, option = "inferno") +
  scale_fill_viridis_d(direction = -1, option = "inferno")# +
# labs(color = "Vida media (días)")



if(VERPLOT) lisaPredPlot
if(GUARDARPLOT) {ggsave("Plots/lisaPlot.tiff", plot =  lisaPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa ZonaMultsipatiKd_t ----
zonasKdT_multispati_Plot <- ggplot(zonasCap5) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color =  Zonas_multispat_kd_t, fill =  Zonas_multispat_kd_t)) +
  theme_map(zonasCap5) +
  scale_color_viridis_d(direction = -1, option = "inferno", begin = 0.4) +
  scale_fill_viridis_d(direction = -1, option = "inferno", begin = 0.4) +
  labs(color = "Zonas", fill = "Zonas")


zonasKoc_multispati_Plot <- ggplot(zonasCap5) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color =  Zonas_multispati_koc_t, fill =  Zonas_multispati_koc_t)) +
  theme_map(zonasCap5) +
  scale_color_viridis_d(direction = -1, option = "inferno", begin = 0.4) +
  scale_fill_viridis_d(direction = -1, option = "inferno", begin = 0.4) +
  labs(color = "Zonas", fill = "Zonas")


zonas_Kdt_Koc_plot <- ggarrange(zonasKdT_multispati_Plot, zonasKoc_multispati_Plot,
          ncol = 2)


if(VERPLOT) zonas_Kdt_Koc_plot
if(GUARDARPLOT) {ggsave("Plots/zonas_Kdt_Koc_Plot.tiff", plot =  zonas_Kdt_Koc_plot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa Lisa ----
lisaPredPlot <- ggplot(zonasCap5) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color =  Patron, fill =  Patron)) +
  theme_map(zonasCap5) +
  scale_color_viridis_d(direction = -1, option = "inferno") +
  scale_fill_viridis_d(direction = -1, option = "inferno")# +
# labs(color = "Vida media (días)")



if(VERPLOT) lisaPredPlot
if(GUARDARPLOT) {ggsave("Plots/lisaPlot.tiff", plot =  lisaPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# Mapa Kdt ratio ---- 
Kda_t_ratioPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = Kda_t_ratio)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")# +
# labs(color = "Vida media (días)")



if(VERPLOT) Kda_t_ratioPredPlot
if(GUARDARPLOT) {ggsave("Plots/Kda_t_ratioPredPlot_Pred.tiff", plot =  Kda_t_ratioPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}





#### CAPITULO 6 ====
# Mapa Zonas sobre KOCa ----
KOCaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = KOCa)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")# +
# labs(color = "Vida media (días)")


if(VERPLOT) KOCaPredPlot
if(GUARDARPLOT) {ggsave("Plots/KOCaPredPlot_Pred.tiff", plot =  KOCaPredPlot, 
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# Mapa Zonas sobre KOCg ----
KOCgPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = KOCg)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")# +
# labs(color = "Vida media (días)")


if(VERPLOT) KOCgPredPlot
if(GUARDARPLOT) {ggsave("Plots/KOCgPredPlot_Pred.tiff", plot =  KOCgPredPlot, 
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa Zonas sobre vida media ----
tmediaPredPlot

# Mapa Zonas sobre GUS ----
GUSPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = GUS)) +
  theme_map(predichosModelos) +
  scale_color_viridis_c(direction = -1, option = "inferno")# +
# labs(color = "Vida media (días)")



if(VERPLOT) GUSPredPlot
if(GUARDARPLOT) {ggsave("Plots/GUSPredPlot_Pred.tiff", plot =  GUSPredPlot, 
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa catKOCa ----
CATKOCaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = CATKOCa, fill = CATKOCa)) +
  theme_map(predichosModelos) +
  scale_color_viridis_d(direction = -1, option = "inferno") +
  scale_fill_viridis_d(direction = -1, option = "inferno") 
# labs(color = "Vida media (días)")

if(VERPLOT) CATKOCaPredPlot
if(GUARDARPLOT) {ggsave("Plots/CATKOCaPredPlot_Pred.tiff", plot =  CATKOCaPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# Mapa catKOCg ----  
table(predichosModelos$CATKOCg)

CATKOCgPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = CATKOCg, fill = CATKOCg)) +
  theme_map(predichosModelos) +
  scale_color_viridis_d(direction = -1, option = "inferno") +
  scale_fill_viridis_d(direction = -1, option = "inferno") 
# labs(color = "Vida media (días)")

if(VERPLOT) CATKOCgPredPlot
if(GUARDARPLOT) {ggsave("Plots/CATKOCgPredPlot_Pred.tiff", plot =  CATKOCgPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# Mapa catvidamedia ----
CATtmediaPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = CATtmedia, fill = CATtmedia)) +
  theme_map(predichosModelos) +
  scale_color_viridis_d(direction = -1, option = "inferno") +
  scale_fill_viridis_d(direction = -1, option = "inferno") 
# labs(color = "Vida media (días)")

if(VERPLOT) CATtmediaPredPlot
if(GUARDARPLOT) {ggsave("Plots/CATtmediaPredPlot_Pred.tiff", plot =  CATtmediaPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# Mapa catGUS ----
CATGUSPredPlot <- ggplot(predichosModelos) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color = CATGUS, fill = CATGUS)) +
  theme_map(predichosModelos) +
  scale_color_viridis_d(direction = -1, option = "inferno") +
  scale_fill_viridis_d(direction = -1, option = "inferno") + 
  labs(color = "Vida media (días)")

if(VERPLOT) CATGUSPredPlot
if(GUARDARPLOT) {ggsave("Plots/CATGUSPredPlot_Pred.tiff", plot =  CATGUSPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# Mapa limitante dren ----
# Mapa pp ----
# Mapa vientos ----
# Mapa riesgo erosión hidrica ----
# Mapa riesgo de erosión eólica ----
# Mapa presencia de bt ----



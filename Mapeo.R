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

# Datos erosion eolica Uso suelo

riesgoEE <- LeerRecortTransf("Datos/riesgo/EE_Pot_1950-2000_wgs84/")
riesgoEE$CLASE_EP <- factor(riesgoEE$CLASE_EP, 
                            levels = c(">150",
                                       "100-150",              
                                       "50-100",
                                       "20-50",
                                       "8-20",
                                       "0-8"
                            ))
UsoSuelo <- LeerRecortTransf("Datos/riesgo/LUS_v2_n2/LUS_v2_n2.shp")

# Lectura Limitantes
limitantes <- read_sf("Datos/riesgo/limitantes/limitantes_edaficas.shp", options = "ENCODING=ISO-8859-1")
limitantes <- st_transform(limitantes, st_crs(predichosModelos))

subSetlimitantes <- limitantes[!limitantes$textura %in% c(
  # "Areno franco en superficie Ud",   
  # "Areno franco en superficie Us",   
  # "Arenoso en superficie Ud",        
  # "Arenoso en superficie Us",       
  "Bañados y lagunas",            
  "Capital",                         
  # "Franco arenoso en superficie Ud", 
  # "Franco arenoso en superficie Us",
  # "Franco en superficie Ud",         
  # "Franco en superficie Us",         
  # "Franco limoso en superficie Ud", 
  # "Franco limoso en superficie Us",
  "Lagunas",
  "Médanos",
  "Roca",
  "Salinas"               
), ]

subSetlimitantes$drenaje <- factor(subSetlimitantes$drenaje, levels = c("Excesivamente drenado",
                                                                        "Algo excesivamente drenado",
                                                                        "Bien a algo excesivamente drenado",
                                                                        "Bien drenado",
                                                                        "Moderadamente bien drenado",
                                                                        "Imperfectamente drenado",
                                                                        "Pobremente drenado",
                                                                        "Muy pobremente drenado"))



subSetlimitantes$alcalinida <- factor(subSetlimitantes$alcalinida, levels = c("No sódico",
                                                                              "Levemente sódico",
                                                                              "Sódico en profundidad",
                                                                              "Sódico en el subsuelo",
                                                                              "Sódico desde superficie"))



subSetlimitantes$prof_efect <- factor(subSetlimitantes$prof_efect, levels = c("Muy somero (< de 30 cm)",
                                                                              "Somero (60-30 cm)",
                                                                              "Algo somero (90-60 cm)",
                                                                              "Poco profundo (120- 90 cm)",
                                                                              "Profundo (> de 120 cm)"))


subSetlimitantes$salinidad <- factor(subSetlimitantes$salinidad, levels = c("Fuertemente salino",
                                                                            "Moderadamente salino",
                                                                            "Levemente salino",
                                                                            "No salino"))  


### Cartas suelos
cartasSuelos <- read_sf("Datos/cartas_suelos_cba/Suelos_geointa_f4.shp")
cartasSuelos<- st_transform(cartasSuelos, st_crs(predichosModelos))
# levels(factor(cartasSuelos$Ggru_1d))


#### DEM y Cursos Agua

limiteCba <- limitesArg[limitesArg$NAM == "CÓRDOBA",]
elevation <- raster("Datos/DEM_yderivados_cba/dtm_elevation_merit.dem_m_250m_s0..0cm_2017_v1.0.tif")

elevation <- crop(elevation, st_transform(limiteCba,st_crs(elevation)))
elevation <- mask(elevation,st_zm(limiteCba))
elevation <- projectRaster(elevation, crs = st_crs(predichosModelos)$proj4string)

rastersf <- st_as_sf(gplot_data(elevation), coords = c("x","y"), crs = crs(elevation))
rastersf <- rastersf[ !is.na(rastersf$value), ]
rastersf <- st_transform(rastersf, st_crs(predichosModelos))

cuerposAguaMuestra <- cuerposAgua[sample(nrow(cuerposAgua), 200) , ]
elevationMuestra <- rastersf[sample(nrow(rastersf), 200) , ]


limiteCbaTrans <- st_transform(limiteCba, st_crs(predichosModelos))


DEMPlot <- ggplot() +
  geom_sf(data=limiteCbaTrans) +
  geom_sf(data = rastersf, aes(color= value)) +
  theme_map(limiteCbaTrans) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  labs(color = "Elevación")


if(VERPLOT) DEMPlot
if(GUARDARPLOT) {ggsave("Plots/DEM.tiff", plot = DEMPlot, 
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


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

if(VERPLOT) zonasKdT_multispati_Plot
if(GUARDARPLOT) {ggsave("Plots/zonas_Kdt_Plot.tiff", plot =  zonasKdT_multispati_Plot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}



zonasKoc_multispati_Plot <- ggplot(zonasCap5) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color =  Zonas_multispati_koc_t, fill =  Zonas_multispati_koc_t)) +
  theme_map(zonasCap5) +
  scale_color_viridis_d(direction = -1, option = "inferno", begin = 0.4) +
  scale_fill_viridis_d(direction = -1, option = "inferno", begin = 0.4) +
  labs(color = "Zonas", fill = "Zonas")

if(VERPLOT) zonasKoc_multispati_Plot
if(GUARDARPLOT) {ggsave("Plots/zonas_Koc_Plot.tiff", plot =  zonasKoc_multispati_Plot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa Lisa ----
lisaPredPlot <- ggplot(zonasCap5) +
  geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
  geom_sf(aes(color =  Patron, fill =  Patron)) +
  theme_map(zonasCap5) +
  scale_color_manual(
    values = c(scales::viridis_pal(option = "inferno", direction = -1)(5)[-5], "grey70" ),
    breaks = levels(zonasCap5$Patron),
    aesthetics = c("colour", "fill")
    )
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
  scale_color_viridis_d(direction = 1, option = "inferno", begin = 0.4) +
  scale_fill_viridis_d(direction = 1, option = "inferno", begin = 0.4) + 
  labs(color = "GUS", fill = "GUS")

if(VERPLOT) CATGUSPredPlot
if(GUARDARPLOT) {ggsave("Plots/CATGUSPredPlot_Pred.tiff", plot =  CATGUSPredPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa carta suelo ----


cartaSueloPlot <- ggplot(cartasSuelos) +
  geom_sf(aes(color = Ggru_1d, fill = Ggru_1d)) +
  geom_sf(data = limitesArg, fill = NA) +  
  theme_map(cartasSuelos) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill"), end = 0.9, direction = -1, na.translate=FALSE) +
  labs(color = "Suelo", fill = "Suelo")

if(VERPLOT) cartaSueloPlot
if(GUARDARPLOT) {ggsave("Plots/CartaSuelo.tiff", plot = cartaSueloPlot, 
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# Mapa limitante textura ----



texturaLimitantesPlot <- ggplot(subSetlimitantes) +
  geom_sf(aes(color = textura, fill = textura)) +
  geom_sf(data = limitesArg, fill = NA) +  
  theme_map(subSetlimitantes) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill"), end = 0.9, direction = -1) +
  labs(color = "Textura", fill = "Textura")

if(VERPLOT) texturaLimitantesPlot
if(GUARDARPLOT) {ggsave("Plots/texturaLimitante.tiff", plot =  texturaLimitantesPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa limitante drenaje ----
drenajeLimitante <- ggplot(subSetlimitantes) +
  geom_sf(aes(color = drenaje, fill = drenaje)) +
  geom_sf(data = limitesArg, fill = NA) +  
  theme_map(subSetlimitantes) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill"), end = 0.9, direction = -1) +
  labs(color = "Drenaje", fill = "Drenaje")

if(VERPLOT) drenajeLimitante
if(GUARDARPLOT) {ggsave("Plots/drenajeLimitante.tiff", plot =  drenajeLimitante,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa limitante alcalinida ----
alcalinidadLimitante <- ggplot(subSetlimitantes) +
  geom_sf(aes(color = alcalinida, fill = alcalinida)) +
  geom_sf(data = limitesArg, fill = NA) +  
  theme_map(subSetlimitantes) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill"), end = 0.9, direction = -1) +
  labs(color = "Alcalinidad", fill = "Alcalinidad")

if(VERPLOT) alcalinidadLimitante
if(GUARDARPLOT) {ggsave("Plots/alcalinidadLimitante.tiff", plot =  alcalinidadLimitante,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


# Mapa limitante prof_efect ----
prof_efectLimitante <- ggplot(subSetlimitantes) +
  geom_sf(aes(color = prof_efect, fill = prof_efect)) +
  geom_sf(data = limitesArg, fill = NA) +  
  theme_map(subSetlimitantes) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill"), end = 0.9, direction = -1) +
  labs(color = "Profundidad", fill = "Profundidad")

if(VERPLOT) prof_efectLimitante
if(GUARDARPLOT) {ggsave("Plots/prof_efectLimitante.tiff", plot =  prof_efectLimitante,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}




# Mapa limitante salinidad ----
salinidadLimitante <- ggplot(subSetlimitantes) +
  geom_sf(aes(color = salinidad, fill = salinidad)) +
  geom_sf(data = limitesArg, fill = NA) +  
  theme_map(subSetlimitantes) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill"), end = 0.9, direction = -1) +
  labs(color = "Salinidad", fill = "Salinidad")

if(VERPLOT) salinidadLimitante
if(GUARDARPLOT) {ggsave("Plots/salinidadLimitante.tiff", plot =  salinidadLimitante,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}






# Mapa pp ----
# Mapa vientos ----
# Mapa riesgo erosión hidrica ----

# Mapa riesgo de erosión eólica ----


erosionEolicaPlot <- ggplot(riesgoEE) +
  geom_sf(aes(color = CLASE_EP, fill = CLASE_EP)) +
  geom_sf(data = limitesArg, fill = NA) + 
  theme_map(riesgoEE) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill")) +
  labs(color = "Erosión Eólica", fill = "Erosión Eólica")

if(VERPLOT) erosionEolicaPlot
if(GUARDARPLOT) {ggsave("Plots/ErosionEolica.tiff", plot =  erosionEolicaPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa LISA y Erosion
erosionEolicaPlot <- ggplot(riesgoEE) +
  
  geom_sf(data = limitesArg, fill = NA) + 
  theme_map(riesgoEE) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill")) +
  labs(color = "Erosión Eólica", fill = "Erosión Eólica")

if(VERPLOT) erosionEolicaPlot
if(GUARDARPLOT) {ggsave("Plots/ErosionEolica.tiff", plot =  erosionEolicaPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}


### LISA y ErosionEolica ----

# ggplot(zonasCap5) +
#   geom_sf(data = limitesArg, fill = NA, size = 0.4, color = "grey40") +
#   geom_sf(data = riesgoEE, aes(color = CLASE_EP, fill = CLASE_EP), alpha = 0.9)  + 
#   geom_sf(aes(color =  Patron, fill =  Patron), alpha = 0.2) +
#   theme_map(zonasCap5) +
#   scale_fill_viridis_d(aesthetics = c("color", "fill")) +
#   scale_fill_viridis_d(aesthetics = c("size"))



# Mapa Uso de Suelo ----



usoSueloPlot <- ggplot(UsoSuelo) +
  geom_sf(aes(color = Nivel_1, fill = Nivel_1)) +
  geom_sf(data = limitesArg, fill = NA) + 
  theme_map(UsoSuelo) + 
  scale_fill_viridis_d(aesthetics = c("color", "fill")) +
  labs(color = "Usos", fill = "Usos")

if(VERPLOT) usoSueloPlot
if(GUARDARPLOT) {ggsave("Plots/UsoSuelo.tiff", plot =  usoSueloPlot,
                        device = "tiff", width = ANCHO, height = ALTO, units = "cm")}

# Mapa presencia de bt ----



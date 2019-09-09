library(sf)
library(ggplot2)
library(ggspatial)
library(ggmap)
register_google(key = "[your key]", write = TRUE)

baseDatos <- read.table("Datos/suelos.txt", header = TRUE)
baseDatos <- st_as_sf(baseDatos, coords = c("Xt","Yt"),  crs = 32720)

ggplot(baseDatos) +
  annotation_map_tile(zoom = 7) +
  geom_sf() + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = 0, pad_y = 1, #unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  # coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +
  xlab("Longitud") + ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))


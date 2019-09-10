library(sf)
library(ggplot2)
library(ggspatial)
library(ggmap)

baseDatos <- read.table("Datos/suelos.txt", header = TRUE)
baseDatos <- st_as_sf(baseDatos, coords = c("Xt","Yt"),  crs = 32720)

ggplot(baseDatos) +
  annotation_map_tile(zoom = 10) +
  geom_sf() + 
  annotation_scale(location = "br") + #, width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "grid", 
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitud") + ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5))  +
  coord_sf(crs = 32720)


ggdraw() +
  draw_plot(plot_fr) +
  draw_plot(plot_wolrd, x = 0.15, y = 0.14, width = .25, height = .25)
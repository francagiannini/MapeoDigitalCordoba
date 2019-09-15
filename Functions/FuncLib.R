library(sf)
library(ggplot2)
library(ggspatial)
library(ggmap)
library(ggsn)
library(ggpubr)

options(OutDec= ",")

windowsFonts(Times=windowsFont("TT Times New Roman"))

dentroDe <- function(datos, limitesProvincia) {
  estandentro <- function(x,y) {sapply(st_intersects(x,y), function(z) if (length(z)==0) F else T)}

  
  limitesProvincia <- st_transform(limitesProvincia, st_crs(datos))
  # st_join(predichosModelos, limitesProvincia, join = st_within)
  datos[estandentro(datos, limitesProvincia),]
  
}


theme_map <- function(p,...) {
  # browser()
  list(
   scalebar(p,dist = 50, dist_unit = "km",border.size = 0.8,
            transform = FALSE,height = 0.018, st.dist = 0.03, 
            st.bottom = TRUE, st.size = 3,  family = "Times",
            x.max = st_bbox(p)[3]-st_bbox(p)[3]*0.3) ,
    annotation_north_arrow(location = "tr", which_north = "grid",
                           height = unit(0.75, "cm"), width = unit(0.56, "cm"),
                           style = north_arrow_orienteering(line_width = 1, line_col = "black",
                                                            fill = c("white", "black"), text_col = "black",
                                                            text_family = "Times",
                                                            text_face = NULL, text_size = 8, text_angle = 0)),
    scale_x_continuous(
      labels = function(x) {
        pos <- sign(x) + 2
        dir <- c("O", "", "E")
         x <- format(abs(x), big.mark = ".", small.mark = ",", scientific = FALSE)
        tout <- paste0(x, "\u00b0", dir[pos])
        tout
      }),
    scale_y_continuous(
      labels = function(x) {
        pos <- sign(x) + 2
        dir <- c("S", "", "N")
        x <- format(abs(x), big.mark = ".", small.mark = ",", scientific = FALSE)
        tout <- paste0(x, "\u00b0", dir[pos])
        tout
      }),
    xlab("Longitud"), 
    ylab("Latitud"),
    theme_minimal(),
    theme(
      text = element_text(size = 12, family = "Times", color = "#22211d"),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = gray(0.6), size = 0.3, linetype = "dashed"),
      panel.grid.minor = element_blank(),
      # plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f5", color = NA), 
      legend.background = element_rect(fill = "#f5f5f5", color = NA),
      panel.border = element_blank(),
      ...
    ),
     coord_sf(xlim = c(st_bbox(p)[1], st_bbox(p)[3]+st_bbox(p)[3]*0.02),
               ylim = st_bbox(p)[c(2,4)])
  )
}

library(sf)
library(ggplot2)
library(ggspatial)
library(ggmap)
library(ggsn)


dentroDe <- function(datos, limitesProvincia) {
  estandentro <- function(x,y) {sapply(st_intersects(x,y), function(z) if (length(z)==0) F else T)}

  
  limitesProvincia <- st_transform(limitesProvincia, st_crs(datos))
  # st_join(predichosModelos, limitesProvincia, join = st_within)
  datos[estandentro(datos, limitesProvincia),]
  
}

theme_map <- function(p,...) {
  # browser()
  list(
   scalebar(p,dist = 50, dist_unit = "km",border.size = 1.2,
            transform = FALSE,height = 0.018, st.dist = 0.02, st.bottom = TRUE, st.size = 5) ,
   # north(p,symbol = 10, scale = 0.15),
    # annotation_scale(location = "bl", bar_cols = c("black", "white"), 
    #                  line_width = 2, height = unit(0.25,"cm"),
    #                  pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
    #                  text_pad = unit(0.15, "cm"), text_cex = 0.8, width_hint = 0.25),
    annotation_north_arrow(location = "tr", which_north = "grid",
                           height = unit(1, "cm"), width = unit(0.75, "cm"),
                           style = north_arrow_orienteering(line_width = 1, line_col = "black",
                                                            fill = c("white", "black"), text_col = "black", text_family = "",
                                                            text_face = NULL, text_size = 8, text_angle = 0)),
    scale_x_continuous(
      labels = function(x) {
        pos <- sign(x) + 2
        dir <- c("O", "", "E")
        tout <- paste0(abs(x), "\u00b0", dir[pos])
        tout
      }),
    scale_y_continuous(
      labels = function(x) {
        pos <- sign(x) + 2
        dir <- c("S", "", "N")
        tout <- paste0(abs(x), "\u00b0", dir[pos])
        tout
      }),
    xlab("Longitud"), 
    ylab("Latitud"),
    theme_minimal(),
    theme(
      text = element_text(size = 23,family = "Ubuntu Regular", color = "#22211d"),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = gray(0.5), size = 0.5, linetype = "dashed"),
      panel.grid.minor = element_blank(),
      # plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
  )
}

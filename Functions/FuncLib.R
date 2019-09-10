library(sf)
library(ggplot2)
library(ggspatial)
library(ggmap)

theme_map <- function(p,...) {
  # browser()
  list(
   
   scalebar(p,dist = 50, dist_unit = "km",
             transform = FALSE,height = 0.018, st.dist = 0.02, st.bottom = FALSE, st.size = 3) ,
   north(p,symbol = 3),
    # annotation_scale(location = "bl", bar_cols = c("black", "white"), 
    #                  line_width = 2, height = unit(0.25,"cm"),
    #                  pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
    #                  text_pad = unit(0.15, "cm"), text_cex = 0.8, width_hint = 0.25),
    # annotation_north_arrow(location = "tr", which_north = "grid", 
    #                        height = unit(1, "cm"), width = unit(0.75, "cm"),
    #                        style = north_arrow_orienteering(line_width = 1, line_col = "black",
    #                                                         fill = c("white", "black"), text_col = "black", text_family = "",
    #                                                         text_face = NULL, text_size = 8, text_angle = 0)),
    scale_x_continuous(
      labels = function(x) {
        pos <- sign(x) + 2
        dir <- c("*O", "", "*E")
        tout <- paste0(abs(x), "*degree", dir[pos])
        return(parse(text=tout))
      }),
    scale_y_continuous(
      labels = function(x) {
        pos <- sign(x) + 2
        dir <- c("*S", "", "*N")
        tout <- paste0(abs(x), "*degree", dir[pos])
        return(parse(text=tout))
      }),
    xlab("Longitud"), 
    ylab("Latitud"),
    theme_minimal(),
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      # axis.line = element_blank(),
      # axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks = element_blank(),
      # axis.title.x = element_blank(),
      # axis.title.y = element_blank(),
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

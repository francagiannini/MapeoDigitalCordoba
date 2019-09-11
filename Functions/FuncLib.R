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


scalebar <- function (data = NULL, location = "bottomright", dist = NULL, 
                      dist_unit = NULL, transform = NULL, dd2km = NULL, model = NULL, 
                      height = 0.02, st.dist = 0.02, st.bottom = TRUE, st.size = 5, 
                      st.color = "black", box.fill = c("black", "white"), box.color = "black", 
                      border.size = 1, x.min = NULL, x.max = NULL, y.min = NULL, 
                      y.max = NULL, anchor = NULL, facet.var = NULL, facet.lev = NULL, 
                      st.inherit = TRUE) 
{
  # browser()
  if (is.null(data)) {
    if (is.null(x.min) | is.null(x.max) | is.null(y.min) | 
        is.null(y.max)) {
      stop("If data is not defined, x.min, x.max, y.min and y.max must be.")
    }
    data <- data.frame(long = c(x.min, x.max), lat = c(y.min, 
                                                       y.max))
  }
  if (is.null(transform)) {
    stop("transform should be logical.")
  }
  if (any(class(data) %in% "sf")) {
    xmin <- sf::st_bbox(data)["xmin"]
    xmax <- sf::st_bbox(data)["xmax"]
    ymin <- sf::st_bbox(data)["ymin"]
    ymax <- sf::st_bbox(data)["ymax"]
  }
  else {
    if (any(startsWith(colnames(data), "lat")) & any(startsWith(colnames(data), 
                                                                "long"))) {
      xmin <- min(data$long)
      xmax <- max(data$long)
      ymin <- min(data$lat)
      ymax <- max(data$lat)
    }
    else {
      stop("'", substitute(data), "' must have columns with names that start with 'lat' and 'long'")
    }
  }
  if (location == "bottomleft") {
    if (is.null(anchor)) {
      x <- xmin
      y <- ymin
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- 1
  }
  if (location == "bottomright") {
    if (is.null(anchor)) {
      x <- xmax
      y <- ymin
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- -1
  }
  if (location == "topleft") {
    if (is.null(anchor)) {
      x <- xmin
      y <- ymax
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- 1
  }
  if (location == "topright") {
    if (is.null(anchor)) {
      x <- xmax
      y <- ymax
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- -1
  }
  if (!st.bottom) {
    st.dist <- y + (ymax - ymin) * (height + st.dist)
  }
  else {
    st.dist <- y - (ymax - ymin) * st.dist
  }
  height <- y + (ymax - ymin) * height
  if (dist_unit == "m") {
    dist <- dist/1000
    dist_unit0 <- "m"
    dist_unit <- "Km"
  }
  if (!is.null(dd2km)) {
    if (dd2km) {
      transform <- TRUE
    }
    cat("dd2km is deprecated. Use ggsn::transform instead.")
  }
  if (transform) {
    break1 <- maptools::gcDestination(lon = x, lat = y, bearing = 90 * 
                                        direction, dist = dist, dist.units = dist_unit, model = model)[1, 
                                                                                                       1]
    break2 <- maptools::gcDestination(lon = x, lat = y, bearing = 90 * 
                                        direction, dist = dist * 2, dist.units = dist_unit, 
                                      model = model)[1, 1]
  }
  else {
    if (location == "bottomleft" | location == "topleft") {
      if (exists("dist_unit0") | (!exists("dist_unit0") & 
                                  dist_unit == "Km")) {
        break1 <- x + dist * 1000
        break2 <- x + dist * 2000
      }
      else if (dist_unit == "nm") {
        break1 <- x + dist * 1852
        break2 <- x + dist * 1852 * 2
      }
      else if (dist_unit == "mi") {
        break1 <- x + dist * 1609.34
        break2 <- x + dist * 1609.34 * 2
      }
      else {
        break1 <- x + dist
        break2 <- x + dist
      }
    }
    else {
      if (exists("dist_unit0") | (!exists("dist_unit0") & 
                                  dist_unit == "Km")) {
        break1 <- x - dist * 1000
        break2 <- x - dist * 2000
      }
      else if (dist_unit == "nm") {
        break1 <- x - dist * 1852
        break2 <- x - dist * 1852 * 2
      }
      else if (dist_unit == "mi") {
        break1 <- x - dist * 1609.34
        break2 <- x - dist * 1609.34 * 2
      }
      else {
        break1 <- x - dist
        break2 <- x - dist
      }
    }
  }
  out_of_range <- function(low, n, high) {
    n < low | n > high
  }
  if (out_of_range(xmin, break1, xmax) | out_of_range(xmin, 
                                                      break2, xmax)) {
    stop("The requested scalebar distance (", substitute(dist), 
         " ", substitute(dist_unit), ") is too large to fit on the map.\n  Try reducing it.")
  }
  box1 <- data.frame(x = c(x, x, rep(break1, 2), x), y = c(y, 
                                                           height, height, y, y), group = 1)
  box2 <- data.frame(x = c(rep(break1, 2), rep(break2, 2), 
                           break1), y = c(y, rep(height, 2), y, y), group = 1)
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    for (i in 1:length(facet.var)) {
      if (any(class(data) == "sf")) {
        if (!is.factor(data[, facet.var[i]][[1]])) {
          data[, facet.var[i]] <- factor(data[, facet.var[i]][[1]])
        }
        box1[, facet.var[i]] <- factor(facet.lev[i], 
                                       levels(data[, facet.var[i]][[1]]))
        box2[, facet.var[i]] <- factor(facet.lev[i], 
                                       levels(data[, facet.var[i]][[1]]))
      }
      else {
        if (!is.factor(data[, facet.var[i]])) {
          data[, facet.var[i]] <- factor(data[, facet.var[i]])
        }
        box1[, facet.var[i]] <- factor(facet.lev[i], 
                                       levels(data[, facet.var[i]]))
        box2[, facet.var[i]] <- factor(facet.lev[i], 
                                       levels(data[, facet.var[i]]))
      }
    }
  }
  if (exists("dist_unit0")) {
    legend <- cbind(text = c(0, dist * 1000, dist * 2000), 
                    row.names = NULL)
  }
  else {
    legend <- cbind(text = c(0, dist, dist * 2), row.names = NULL)
  }
  gg.box1 <- geom_polygon(data = box1, aes(x, y), fill = utils::tail(box.fill, 
                                                                     1), color = utils::tail(box.color, 1), size = border.size)
  gg.box2 <- geom_polygon(data = box2, aes(x, y), fill = box.fill[1], 
                          color = box.color[1], size = border.size)
  x.st.pos <- c(box1[c(1, 3), 1], box2[3, 1])
  if (location == "bottomright" | location == "topright") {
    x.st.pos <- rev(x.st.pos)
  }
  label <- NULL
  if (exists("dist_unit0")) {
    legend2 <- cbind(data[1:3, ], x = unname(x.st.pos), y = unname(st.dist), 
                     label = paste0(legend[, "text"], c("", "", "m")))
  }
  else {
    legend2 <- cbind(data[1:3, ], x = unname(x.st.pos), y = unname(st.dist), 
                     label = paste0(legend[, "text"], c("", "", dist_unit)))
  }
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    for (i in 1:length(facet.var)) {
      if (any(class(data) == "sf")) {
        legend2[, facet.var[i]] <- factor(facet.lev[i], 
                                          levels(data[, facet.var[i]][[1]]))
      }
      else {
        legend2[, facet.var[i]] <- factor(facet.lev[i], 
                                          levels(data[, facet.var[i]]))
      }
    }
  }
  else if (!is.null(facet.var) & is.null(facet.lev)) {
    facet.levels0 <- unique(as.data.frame(data)[, facet.var])
    facet.levels <- unlist(unique(as.data.frame(data)[, facet.var]))
    legend2 <- do.call("rbind", replicate(length(facet.levels), 
                                          legend2, simplify = FALSE))
    if (length(facet.var) > 1) {
      facet.levels0 <- expand.grid(facet.levels0)
      legend2[, facet.var] <- facet.levels0[rep(row.names(facet.levels0), 
                                                each = 3), ]
    }
    else {
      legend2[, facet.var] <- rep(facet.levels0, each = 3)
    }
  }
  if (!st.inherit) {
    legend2 <- legend2[, c("x", "y", "label")]
  }
  gg.legend <- geom_text(data = legend2, aes(x, y, label = label), 
                         size = st.size, color = st.color, inherit.aes = st.inherit)
  return(list(gg.box1, gg.box2, gg.legend))
}




theme_map <- function(p,...) {
  # browser()
  list(
   scalebar(p,dist = 50, dist_unit = "Km",border.size = 1.2,
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
        # return(parse(text=tout))
      }),
    scale_y_continuous(
      labels = function(x) {
        pos <- sign(x) + 2
        dir <- c("S", "", "N")
        tout <- paste0(abs(x), "\u00b0", dir[pos])
        tout
        # return(parse(text=tout))
      }),
    xlab("Longitud"), 
    ylab("Latitud"),
    theme_minimal(),
    theme(
      text = element_text(size = 16,family = "Ubuntu Regular", color = "#22211d"),
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

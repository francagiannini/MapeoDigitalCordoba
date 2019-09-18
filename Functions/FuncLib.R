library(sf)
library(ggplot2)
library(ggspatial)
library(ggmap)
library(ggsn)
library(ggpubr)
library(raster)

options(OutDec= ",")


if(.Platform$OS.type == "windows") {
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  familiaTexto <- "Times"
} else {
  library(extrafont)
  loadfonts(device = "postscript", quiet = TRUE)
  loadfonts(device = "pdf", quiet = TRUE)
  familiaTexto <- "Times New Roman"
}


scalebar <- function (data = NULL, location = "bottomright", dist = NULL, 
                      dist_unit = NULL, transform = NULL, dd2km = NULL, model = NULL, 
                      height = 0.02, st.dist = 0.02, st.bottom = TRUE, st.size = 5, 
                      st.color = "black", box.fill = c("black", "white"), 
                      box.color = "black", border.size = 1, x.min = NULL, 
                      x.max = NULL, y.min = NULL, y.max = NULL, anchor = NULL, 
                      facet.var = NULL, facet.lev = NULL, ...) 
{
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
    xmin <- min(data$long)
    xmax <- max(data$long)
    ymin <- min(data$lat)
    ymax <- max(data$lat)
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
    dist_unit <- "km"
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
                                  dist_unit == "km")) {
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
                                  dist_unit == "km")) {
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
                     label = paste0(legend[, "text"], c("", 
                                                        "", "m")))
  }
  else {
    legend2 <- cbind(data[1:3, ], x = unname(x.st.pos), y = unname(st.dist), 
                     label = paste0(legend[, "text"], c("", 
                                                        "", dist_unit)))
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
  gg.legend <- geom_text(data = legend2, aes(x, y, label = label), 
                         size = st.size, color = st.color, ...)
  return(list(gg.box1, gg.box2, gg.legend))
}


LeerRecortTransf <- function(archivo, 
                             recorte = limitesArg[limitesArg$NAM == "CÓRDOBA",], 
                             crs = st_crs(predichosModelos), ...) {
  
  archivoSf <- read_sf(archivo, ...)
  archivoSf <- st_intersection(archivoSf, recorte)
  archivoSf <- st_transform(archivoSf, crs)
  return(archivoSf)
  
}


dentroDe <- function(datos, limitesProvincia) {
  estandentro <- function(x,y) {sapply(st_intersects(x,y), function(z) if (length(z)==0) F else T)}

  
  limitesProvincia <- st_transform(limitesProvincia, st_crs(datos))
  # st_join(predichosModelos, limitesProvincia, join = st_within)
  datos[estandentro(datos, limitesProvincia),]
  
}


theme_map <- function(p,...) {
  
  if(st_crs(p)$epsg != 32720) {warning("Puede haber errores de escalas debido a que epsg != 32720")}
  # browser()
  list(
   scalebar(p,dist = 50, dist_unit = "km",border.size = 0.8,
            transform = FALSE,height = 0.018, st.dist = 0.03,
            st.bottom = TRUE, st.size = 3,  #family = familiaTexto,
            x.max = st_bbox(p)[3]-st_bbox(p)[3]*0.3) ,
    annotation_north_arrow(location = "tr", which_north = "grid",
                           height = unit(0.75, "cm"), width = unit(0.56, "cm"),
                           style = north_arrow_orienteering(line_width = 1, line_col = "black",
                                                            fill = c("white", "black"), text_col = "black",
                                                            text_family = familiaTexto,
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
      text = element_text(size = 12, family = familiaTexto, color = "#22211d"),
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






#' Transform raster as data.frame to be later used with ggplot
#' Modified from rasterVis::gplot
#'
#' @param x A Raster* object
#' @param maxpixels Maximum number of pixels to use
#'
#' @details rasterVis::gplot is nice to plot a raster in a ggplot but
#' if you want to plot different rasters on the same plot, you are stuck.
#' If you want to add other information or transform your raster as a
#' category raster, you can not do it. With `SDMSelect::gplot_data`, you retrieve your
#' raster as a tibble that can be modified as wanted using `dplyr` and
#' then plot in `ggplot` using `geom_tile`.
#' If Raster has levels, they will be joined to the final tibble.
#'
#' @export

gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}


# con <- out
# 
# model <- "FLake"
# var <- "Ts"
# 
# model <- "GLM"
# var <- "temp"
# var <- "evap"
# 
# model <- "GOTM"
# var <- "temp"
# var <- "eps"
# var <- "precip"
# 
# model <- "Simstrat"
# var <- "T"
# var <- "eps"
# var <- "TotalIceH"
# var <- "Qvert"
# 
# model <- "MyLake"
# var <- "Tzt"
# var <- "Qst"


plot_raw <- function(con, model, var, xlim = NULL, zlim = NULL) {
  
  if(model == "FLake") {
    df <- data.frame(Date = con[[model]][["Date"]], value = con[[model]][["out"]][[var]])
  } else if(model %in% c("GLM", "GOTM")) {
    mat <- ncdf4::ncvar_get(con[[model]][["nc"]], varid = var)
    
    if(length(dim(mat)) == 2) {
      if(nrow(mat) > nrow(con[[model]][["layers"]])) {
        layers <- ncdf4::ncvar_get(con[[model]][["nc"]], varid = "zi")
        layers <- apply(layers, 2, \(x){diff(c(x, 0))})
        layers[nrow(layers), ] <- layers[1, ]
      } else {
        layers <- as.matrix(con[[model]][["layers"]])
      }
      
      depth <- apply(layers, 2, \(x) {cumsum(c(x))})
      df <- data.frame(Date = rep(con[[model]][["Date"]], each = nrow(mat)), 
                       depth = c(depth),
                       lyr_thk = c(layers), value = c(mat)) %>% 
        na.exclude()
    } else if(length(dim(mat)) == 1) {
      df <- data.frame(Date = con[[model]][["Date"]], value = c(mat))
    }
  } else if(model == "Simstrat") {
    file <- con[[model]][["ref_table"]][["file"]][con[[model]][["ref_table"]][["var"]] == var]
    mat <- readr::read_csv(file, show_col_types = FALSE, col_select = -1, col_names = FALSE, skip = 1)
    if(ncol(mat) == 1) {
      df <- data.frame(Date = con[[model]][["Date"]], value = unlist(mat))
    } else {
      mat <- as.matrix(t(mat))
      depth <- as.matrix(con[[model]][["layers"]])
      layers <- apply(depth, 2, \(x) {
        mid <- abs(mean(diff(x)))
        diff(c(0, x[order(x)] + mid))
      })

      df <- data.frame(Date = rep(con[[model]][["Date"]], each = nrow(mat)), 
                       depth = c(depth),
                       lyr_thk = c(layers), value = c(mat))
      
    }
  } else if(model == "MyLake") {
    mat <- con[[model]][["out"]][[var]]
    mat <- mat[nrow(mat):1, ]
    depth <- as.matrix(con[[model]][["layers"]])
    layers <- apply(depth, 2, \(x) {
      mid <- mean(diff(x))
      diff(c(0, x + mid))
    })
    df <- data.frame(Date = rep(con[[model]][["Date"]], each = nrow(mat)), 
                     depth = c(depth),
                     lyr_thk = c(layers), value = c(mat))
    
  }
  
  if(is.null(xlim)) {
    xlim = range(df[["Date"]])
  }
  if(is.null(zlim)) {
    zlim = range(df[["value"]], na.rm = TRUE)
  }
  
  my.cols <- RColorBrewer::brewer.pal(11, "Spectral")
  
  if(ncol(df) == 2) {
    p <- ggplot(df) +
      geom_line(aes(Date, value)) +
      ylab(var)
  } else {
    p <- ggplot(df) +
      geom_col(aes(x = Date, y = lyr_thk, fill = value), position = 'stack', width = 1) +
      scale_fill_gradientn(colors = rev(my.cols), name = var, limits = zlim) +
      scale_colour_gradientn(colors = rev(my.cols), name = var, limits = zlim)
  }
  
  p <- p + coord_cartesian(xlim = xlim)
  
  return(p)
}
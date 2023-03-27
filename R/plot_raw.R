#' Plot raw model output
#'
#' Plot a variable from the raw model output.
#'
#' @name plot_raw
#' @param con list; output from `connect_output()`.
#' @inheritParams export_config
#' @param var string; short variable name within the model. Found in the "var" column in `model_var_dic`.
#' @param xlim Date; pairs for min and max limits for the x-axis. Default is NULL and will use the range in the dataset.
#' @param zlim Date; pairs for min and max limits for the Z-axis. Default is NULL and will use the range in the dataset.
#' @param var_lab string; label to be used for y-axis (2-D plot) or z-axis (3-D plot). Default is NULL and will use `var` value.
#' @return list
#' @import ncdf4
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export

plot_raw <- function(con, model, var, xlim = NULL, zlim = NULL, var_lab = NULL) {
  
  require(readr)
  
  if(model == "FLake") {
    df <- data.frame(Date = con[[model]][["Date"]], value = con[[model]][["out"]][[var]])
  } else if(model %in% c("GLM", "GOTM")) {
    idx <- which(con[[model]][["ref_table"]][["var"]] == var)
    out_file <- basename(con[[model]][["ref_table"]][["file"]][idx])
    if(out_file == "lake.csv") {
      df <- data.frame(Date = con[[model]][["out"]][["Date"]], value = con[[model]][["out"]][[var]])
    } else if(out_file == "output.nc") {
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
  if(is.null(var_lab)) {
    var_lab <- var
  }
  
  my.cols <- RColorBrewer::brewer.pal(11, "Spectral")
  
  if(ncol(df) == 2) {
    p <- ggplot(df) +
      geom_line(aes(Date, value)) +
      ylab(var_lab)
  } else {
    p <- ggplot(df) +
      geom_col(aes(x = Date, y = lyr_thk, fill = value), position = 'stack', width = 1) +
      scale_fill_gradientn(colors = rev(my.cols), name = var_lab, limits = zlim) +
      scale_colour_gradientn(colors = rev(my.cols), name = var_lab, limits = zlim) +
      ylab("Depth from bottom (m)")
  }
  
  p <- p + coord_cartesian(xlim = xlim)
  
  return(p)
}

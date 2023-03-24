#' Connect model output
#'
#' Load and connect to model outputs.
#'
#' @name connect_output
#' @inheritParams export_config
#' @return List with either the data loaded (FLake, MtLake) or netCDF conenctions (GLM, GOTM) or list of files to be read (Simstrat).
#' @importFrom dplyr case_when mutate pull
#' @importFrom readr read_csv
#' @import ncdf4
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
connect_output <- function(model, config_file, folder = ".") {

  original_tz <- Sys.getenv("TZ")
  on.exit({
    Sys.setenv(TZ = original_tz)
  })
  
  Sys.setenv(TZ = "UTC")
  
  mods <- lapply(model, \(m) {
    out_file <- dplyr::case_when(m == "FLake" ~ file.path(folder, m, "output", "output.dat"),
                          m == "GLM" ~ file.path(folder, m, "output", "output.nc"),
                          m == "GOTM" ~ file.path(folder, m, "output", "output.nc"),
                          m == "MyLake" ~ file.path(folder, m, "output", "output.RData"),
                          m == "Simstrat" ~ file.path(folder, m, "output", "T_out.dat")
    )
    lst <- list(out = NULL,
                nc = NULL,
                Date = NULL,
                layers = NULL,
                ref_table = NULL)
    
    if(!file.exists(out_file)) {
      message("No ", out_file, " present.")
      return(NULL)
    }
    
    if(m %in% c("GLM", "GOTM")) {
      nc <- ncdf4::nc_open(out_file)
      on.exit(ncdf4::nc_close(nc))
      
      ref <- data.frame(var = names(nc$var), longname = NA, units = NA, dims = NA)
      for(v in 1:nrow(ref)) {
        ref$longname[v] <- nc$var[[v]]$longname
        ref$units[v] <- nc$var[[v]]$units
        ref$dims[v] <- nc$var[[v]]$ndims
      }
      lst$ref_table <- ref
    }
    
    if(m == "FLake") {
      nml_file <- file.path(folder, m, "flake.nml")
      met_file <- suppressWarnings(glmtools::get_nml_value(arg_name = "meteofile", nml_file = nml_file))
      met_file <- file.path(folder, m, met_file)
      met_file <- gsub(",", "", met_file)
      
      out_hour <- lubridate::hour(get_yaml_value(config_file, "time", "start"))
      
      met <- read.delim(met_file, header = FALSE)
      lst$Date <- as.Date(as.POSIXct(met[, ncol(met)], tz = "UTC") + (out_hour * 60 * 60))
      lst$out <- read.table(out_file, header = TRUE, skip = 1, stringsAsFactors = FALSE)
      lst$ref_table <- data.frame(var = names(lst$out)) 
    } else if(m == "GLM") {
      nlayers <- ncdf4::ncvar_get(nc, "NS")
      hours.since  <- ncdf4::ncvar_get(nc, "time")
      date.start <- as.POSIXct(gsub("hours since ","",ncdf4::ncatt_get(nc,'time','units')$value))
      lst$Date <- as.POSIXct(hours.since * 3600 + date.start) %>% as.Date()
      lyrs <- ncdf4::ncvar_get(nc, "z")
      lyrs[lyrs > 1000000] <- NA
      lst$layers <- apply(lyrs, 2, \(x) {diff(c(0, x))})
    } else if(m == "GOTM") {
      out.steps <- ncdf4::ncvar_get(nc, "time")
      date.start <- ncdf4::ncatt_get(nc,'time','units')$value %>%
        gsub("seconds since ","",.) %>%
        as.POSIXct %>% as.Date()
      lst$Date <- seq.Date(date.start, by = 1, length.out = length(out.steps))
      lst$layers <- data.frame(ncdf4::ncvar_get(nc, "h")) # lyrs

    } else if (m == "Simstrat") {
      ### Convert decimal days to yyyy-mm-dd HH:MM:SS
      par_file <- file.path(folder, get_yaml_value(config_file, "config_files", "Simstrat"))
      timestep <- get_json_value(file.path(folder, par_file), "Simulation", "Timestep s")
      reference_year <- get_json_value(file.path(folder, par_file), "Simulation", "Start year")
      
      fils <- list.files(file.path(m, "output"), full.names = TRUE)
      if(length(fils) == 0) {
        stop("No output files in ", file.path(m, "output"))
      }
      lst$ref_table <- data.frame(var = gsub("_out.dat", "", basename(fils)), file = fils)
      lst$Date <- readr::read_csv(file.path(folder, "Simstrat", "output", "T_out.dat"), 
                               col_select = 1, show_col_types = FALSE, col_names = "date", skip = 1) %>% 
        dplyr::mutate(date = date * 3600 * 24) %>% 
        dplyr::mutate(date = lubridate::as_datetime(date, origin = paste0(reference_year, "-01-01"))) %>% 
        dplyr::mutate(date = as.Date(date)) %>% 
        dplyr::pull()
      
      depths <- readr::read_csv(file.path(folder, "Simstrat", "output", "T_out.dat"), 
                                col_select = -1, show_col_types = FALSE, col_names = FALSE, n_max = 1) %>% 
        as.numeric() %>% 
        abs()
      lst$layers <- as.data.frame(matrix(rep(depths, length(lst$Date)), ncol = length(lst$Date)))

    } else if (m == "MyLake") {
      load(out_file)
      lst$out <- res
      lst$layers <- as.data.frame(matrix(rep(res$zz, length(res$tt)), ncol = length(res$tt)))
      lst$ref_table <- data.frame(var = names(res))
      lst$Date <- as.Date(as.POSIXct((as.numeric(res$tt) - 719529) * 86400, origin = "1970-01-01"))
    }
    
    return(lst)
    
  })
  names(mods) <- model
  
  chk <- sapply(mods, is.null)
  if(all(chk)) {
    stop("No output for ", paste0(model, collapse = ", "), " in '", folder, "'")
  }
  
  nc_mods <- c("GLM", "GOTM")[(c("GLM", "GOTM") %in% names(mods))]
  
  if(length(nc_mods) > 0) {
    for(m in nc_mods) {
      mods[[m]]$nc <- ncdf4::nc_open(file.path(folder, m, "output", "output.nc"))
    }
  }
  
  return(mods)
}

#' @name close_nc
#' @noRd
close_nc <- function(con) {
  mods <- c("GLM", "GOTM")
  mods <- mods[which(mods %in% names(con))]
  if(length(mods) > 0) {
    for(m in mods) {
      ncdf4::nc_close(con[[m]][["nc"]])
    }
  }
}

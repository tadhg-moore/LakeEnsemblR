#'Creates directories for each model
#'
#'Creates directories with file setups for each model, based on the master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")
#'@param folder folder
#'@keywords methods
#' @examples
#' \dontrun{
#' }
#'
#'
#'@export

export_dirs <- function(config_file, model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                          folder = "."){
  # Set working directory
  oldwd <- getwd()
  setwd(folder)
  
  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
  })
  
  # check model input
  model <- check_models(model)
  
  sapply(model, function(m) {
    dir.create(paste0(m, "/output"), recursive = TRUE)
    # Read the model config file from config_file, and write it to the model directory
    temp_fil <- get_yaml_value(config_file, "config_files", m)
    if(!file.exists(temp_fil)){
      get_template(template = paste0(m, "_config"), folder = folder, filename = temp_fil)
    }
  })
  
  message("export_dirs complete!")
}

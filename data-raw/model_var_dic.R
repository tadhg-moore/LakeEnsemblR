wd <- here::here()

library(LakeEnsemblR)
library(gotmtools)
require(magrittr)
template_folder <- system.file("extdata/feeagh", package = "LakeEnsemblR")
setwd(template_folder) # Change working directory to example folder

# Set config file
masterConfigFile <- "LakeEnsemblR.yaml"
config_file <- "LakeEnsemblR.yaml"
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")

# 1. Example - export configuration settings
export_config(config_file = config_file,
              model = model)

run_ensemble(config_file = config_file,
             model = model, parallel = TRUE)

out <- connect_output(model = model, config_file = config_file)


refs <- lapply(seq_along(out), \(m, x) {
  ref_table <- out[[x]]$ref_table #$model <- m[x]
  ref_table[["model"]] <- m[x]
  return(ref_table)
}, m = names(out))
names(refs) <- names(out)

df <- dplyr::full_join(refs$FLake, refs$GLM) %>% 
  dplyr::full_join(refs$GOTM) %>% 
  dplyr::full_join(refs$Simstrat) %>% 
  dplyr::full_join(refs$MyLake)

df <- df %>% 
  dplyr::relocate(model, everything()) %>% 
  dplyr::mutate(plot = FALSE)
head(df)

setwd(wd)

write.csv(df, "data-raw/model_var_dic_draft.csv", row.names = FALSE)

# Edit manually and save as model_var_dic.csv

model_var_dic <- read.csv("data-raw/model_var_dic.csv")
model_var_dic
usethis::use_data(model_var_dic, overwrite = TRUE)

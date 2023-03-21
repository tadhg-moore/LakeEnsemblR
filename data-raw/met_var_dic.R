# Meteorological variable dictionary ----
met_var_dic <- read.csv("data-raw/met_var_dic.csv")
usethis::use_data(met_var_dic, overwrite = TRUE)

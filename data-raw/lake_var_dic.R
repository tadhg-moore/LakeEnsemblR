# Lake variable dictionary ----
lake_var_dic <- read.csv("data-raw/lake_var_dic.csv")
lake_var_dic
usethis::use_data(lake_var_dic, overwrite = TRUE)

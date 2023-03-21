# Meteorological input descriptions ----
met_input <- read.csv("data-raw/met_input.csv")
names(met_input) <- c("Description", "Units", "Column Name", "Status")
usethis::use_data(met_input, overwrite = TRUE)

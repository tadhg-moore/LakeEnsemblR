#' Write yaml list to file
#' 
#' @name write_yaml
#' @param yaml list; loaded using `configr::read.config()`
#' @param file filepath; to file that is written
#' @noRd

write_yaml <- function(yaml, file) {
  
  require(yaml)
  
  cfg <- readLines(file)
  comments <- strsplit(cfg, "#") %>%
    sapply(., \(x) {
      if(length(x) == 2) {
        paste0("   # ", trimws(x[[2]]))
      } else {
        ""
      }
    })
  
  configr::write.config(config.dat = yaml, file.path = file,
                        write.type = "yaml", indent = 3)
  tst <- readLines(file) %>% 
    gsub("no", "false", .) %>% 
    gsub("yes", "true", .) %>% 
    gsub("'NULL'", "NULL", .) %>% 
    gsub("~", "NULL", .) %>% 
    # paste0(., comments) %>% 
    writeLines(., file)
}

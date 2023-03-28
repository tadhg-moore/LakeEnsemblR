#' Generate UI elements from LER configuration file
#' 
#' @name yaml_to_ui
#' @inheritParams export_config
#' 

yaml_to_ui <- function(config_file) {
  
  require(configr)
  require(shiny)
  
  yaml <- configr::read.config(config_file)
  ler_sections <- c("location", "time", "config_files", "observations", "input",
                    "inflows", "outflows", "output")
  ui_list <- lapply(ler_sections, function(v) {
    tagList(
      tags$h3(gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                  v,
                  perl = TRUE)),
      if(v %in% c("location", "config_files", "inflows", "outflows", "output")) {
        lapply(seq_along(yaml[[v]]), \(m, i) {
          div(style="display:inline-block;",
              create_ui(inputId = paste0(v, "_", m[i]), label = m[i], value = yaml[[v]][[m[i]]]))
        }, m = names(yaml[[v]]))
      } else if(v == "time") {
        lapply(seq_along(yaml[[v]]), \(m, i) {
          div(style="display:inline-block;",
              create_ui(inputId = paste0(v, "_", m[i]), label = m[i], value = yaml[[v]][[m[i]]]))
        }, m = names(yaml[[v]]))
      } else if(v %in% c("observations")) {
        lapply(seq_along(yaml[[v]]), \(m, i) {
          lapply(seq_along(yaml[[v]][[m[i]]]), \(n, j) {
            div(style="display:inline-block;", tags$em(m[i]),
                create_ui(inputId = paste0(v, "_", m[i], "_", n[j]), label = n[j], value = yaml[[v]][[m[i]]][[n[j]]]))
          }, n = names(yaml[[v]][[m[i]]]))
        }, m = names(yaml[[v]]))
      } else if(v == "input") {
        lapply(seq_along(yaml[[v]]), \(m, i) {
          if(m[i] == "light") {
            lapply(seq_along(yaml[[v]][["light"]][["Kw"]]), \(n, j) {
              div(style="display:inline-block;", tags$em(m[i]),
                  create_ui(inputId = paste0(v, "_", m[i], "_Kw_", n[j]), label = paste0(n[j], " Kw"),
                            value = yaml[[v]][["light"]][["Kw"]][[n[j]]]))
            }, n = names(yaml[[v]][["light"]][["Kw"]]))
          } else {
            lapply(seq_along(yaml[[v]][[m[i]]]), \(n, j) {
              div(style="display:inline-block;", tags$em(m[i]),
                  create_ui(inputId = paste0(v, "_", m[i], "_", n[j]), label = n[j], value = yaml[[v]][[m[i]]][[n[j]]]))
            }, n = names(yaml[[v]][[m[i]]]))
          }
        }, m = names(yaml[[v]]))
      },
      tags$br()
    )
  })
  
  return(ui_list)
  
  # output$cfg_scaling_factors <- renderUI({
  #   lapply(seq_along(yaml[["scaling_factors"]]), \(m, i) {
  #     lapply(seq_along(yaml[["scaling_factors"]][[m[i]]]), \(n, j) {
  #       div(style="display:inline-block;", tags$em(m[i]),
  #           create_ui(inputId = paste0(m[i], "_", n[j]), label = n[j], value = yaml[["scaling_factors"]][[m[i]]][[n[j]]]))
  #     }, n = names(yaml[["scaling_factors"]][[m[i]]]))
  #   }, m = names(yaml[["scaling_factors"]]))    
  # })
  
}

get_ui_id <- function(config_file) {
  require(configr)

  yaml <- configr::read.config(config_file)
  ler_sections <- c("location", "time", "config_files", "observations", "input",
                    "inflows", "outflows", "output")
  id_list <- lapply(ler_sections, function(v) {
      if(v %in% c("location", "config_files", "inflows", "outflows", "output")) {
        lapply(seq_along(yaml[[v]]), \(m, i) {
          paste0(v, "/", m[i])
        }, m = names(yaml[[v]]))
      } else if(v == "time") {
        lapply(seq_along(yaml[[v]]), \(m, i) {
          if(m[i] %in% c("start", "stop")) {
            paste0(v, "/", m[i], "/", c("date", "time"))
          } else {
            paste0(v, "/", m[i])
          }
        }, m = names(yaml[[v]]))
      } else if(v %in% c("observations")) {
        lapply(seq_along(yaml[[v]]), \(m, i) {
          lapply(seq_along(yaml[[v]][[m[i]]]), \(n, j) {
            paste0(v, "/", m[i], "/", n[j])
          }, n = names(yaml[[v]][[m[i]]]))
        }, m = names(yaml[[v]]))
      } else if(v == "input") {
        lapply(seq_along(yaml[[v]]), \(m, i) {
          if(m[i] == "light") {
            lapply(seq_along(yaml[[v]][["light"]][["Kw"]]), \(n, j) {
              paste0(v, "/", m[i], "/Kw/", n[j])
            }, n = names(yaml[[v]][["light"]][["Kw"]]))
          } else {
            lapply(seq_along(yaml[[v]][[m[i]]]), \(n, j) {
              paste0(v, "/", m[i], "/", n[j])
            }, n = names(yaml[[v]][[m[i]]]))
          }
        }, m = names(yaml[[v]]))
      }
  })
  unlist(id_list)
  
  return(unlist(id_list))
}


#' Create UI widgets based on values passed
#' @inheritParams shiny::textInput

create_ui <- function(inputId, label, value) {
  
  require(lubridate)
  require(shinyTime)
  require(shiny)
  
  if(is.logical(value)) {
    radioButtons(inputId = inputId, label = label, choices = c(TRUE, FALSE), selected = value, inline = FALSE)
  } else if(is.null(value)) {
    textInput(inputId = inputId, label = label, value = value)
  } else if (length(value) > 1) {
    # selectInput(inputId = inputId, label = label, choices = value, selected = value, multiple = TRUE)
    selectizeInput(inputId = inputId, label = label, choices = value, selected = value, 
                   multiple = TRUE, options = list(create = TRUE,
                                  createOnBlur = TRUE))
  } else if(is.numeric(value)) {
    numericInput(inputId = inputId, label = label, value = value)
  } else if (!is.na(lubridate::parse_date_time(value, orders = c("ymd HMS"), quiet = TRUE))) {
    tagList(
      div(style = "display:inline-block;",
          dateInput(inputId = paste0(inputId, "_date"), label = paste0(label, " date"), value = value, format = "yyyy-mm-dd"),
          shinyTime::timeInput(inputId = paste0(inputId, "_time"), label = paste0(label, " time"),
                               value = format(as.POSIXct(value, tz = "UTC"), format = "%H:%M:%S"))
      )
    )
  # } else if(label == "file") {
  #   fileInput(inputId = inputId, label = label, multiple = FALSE, accept = ".csv", placeholder = value)
  } else if(is.character(value)) {
    textInput(inputId = inputId, label = label, value = value)
  } else if(file.exists(value)) {
    textInput(inputId = inputId, label = label, value = value)
  }
}

#' Inspect model output within a Shiny application
#'
#' Loads raw model output and allows for inspection withing a Shiny application.
#'
#' @name inspect_output
#' @inheritParams export_config
#' @return Launches a shiny application
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
inspect_output <- function(model, config_file) {
  
  require(shiny)
  require(shinydashboard)

  # model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  

  # on.exit({
  #   LakeEnsemblR:::close_nc(out)
  # })
  
  cfg <- readLines(config_file)
  yaml <- configr::read.config(config_file)

  # lakename <- basename(lake_dir)
  
  # ref_table <- nc[[model]]$ref_table
  # UI ----
  ui <- dashboardPage(
    dashboardHeader(title = "LakeEnsemblR"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Model output", tabName = "model_output"),
        menuItem(text = "Configuration", tabName = "configuration")
        # menuItem("Input", tabName = "input"),
        # menuItem("LER output", tabName = "ler_output")
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML("
        .skin-blue .main-header .logo {
                              background-color: #18188c;
                              }
        .skin-blue .main-header .logo:hover {
        background-color: #18188c;
        }
        .skin-blue .main-header .navbar {
        background-color: #18188c;
        }
        .shinytime-hours , .shinytime-mins , .shinytime-secs {
        padding-right: 6px;
        }
                              " ))),
      tabItems(
        tabItem(tabName = "configuration",
                h2("Configuration"),
                uiOutput("ler_ui"),
                actionButton("save_yaml", "Save file", icon = icon("save")),
                br(),

                uiOutput("text"),
                verbatimTextOutput("config_file")
                ),
        tabItem(tabName = "input",
                h2("input")
                ),
        tabItem(tabName = "model_output",
                fluidRow(
                  column(3,
                         actionButton("open_output", "Open model output"),
                         uiOutput("model_sel"),
                         uiOutput("var_sel")
                         # conditionalPanel("input.open_output > 0",
                         #                  actionButton("close_output", "Close output"))
                  ),
                  column(9,
                         # h4(lakename),
                         plotOutput("plot2d", height = "400px"),
                         uiOutput("date_slider"),
                         br(),
                         p(tags$b("Table 1."), " List of model output variables."),
                         tableOutput("model_vars")
                  )
                )
        ),
        tabItem(tabName = "ler_output",
                h2("LER output")
                )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    lst <- reactiveValues(out = NULL, var1 = NULL, var1_lab = NULL, ref_table = NULL)
    
    observeEvent(input$open_output, {
      lst$out <- connect_output(model = model, config_file = config_file)
      for(m in model) {
        lst$out[[m]]$ref_table <- model_var_dic[model_var_dic$model == m, ]
      }
    })
    # observeEvent(input$close_output, {
    #   LakeEnsemblR:::close_nc(lst$out)
    #   for(i in names(lst)) {
    #     lst[[i]] <- NULL
    #   }
    # })
    
    output$model_sel <- renderUI({
      req(!is.null(lst$out))
      radioButtons("model", label = "Model", choices = names(lst$out))
    })
    
    output$var_sel <- renderUI({
      req(!is.null(lst$out))
      choices <- lst$ref_table$longname[lst$ref_table$plot]
      selectInput("var1", "Select variable", 
                  choices = choices)
    })
    
    output$date_slider <- renderUI({
      req(!is.null(lst$out))
      req(!is.null(input$model))
      start <- min(lst$out[[input$model]][["Date"]])
      end <- max(lst$out[[input$model]][["Date"]])
      sliderInput("xlim",
                  "Dates:",
                  min = start,
                  max = end,
                  value = c(start, end),
                  timeFormat="%Y-%m-%d", width = "100%")
      
    })
    
    
    observeEvent(input$model, {
      lst$var1 <- NULL
      lst$ref_table <- lst$out[[input$model]]$ref_table

    })
    
    observe({
      req(length(input$model) == 1)
      lst$var1 <- lst$ref_table$var[lst$ref_table$longname == input$var1]
      lst$var1_lab <- paste0(lst$ref_table$longname[lst$ref_table$longname == input$var1],
                             "\n[", lst$ref_table$units[lst$ref_table$longname == input$var1], "]")
    })
    
    output$plot2d <- renderPlot({
      validate(
        need(!is.null(lst$out), "Click 'Open model output'")
      )
      validate(
        need(length(lst$var1) == 1 & !is.null(lst$var1), "Select a variable.")
      )
      plot_raw(con = lst$out, model = input$model, var = lst$var1, xlim = input$xlim, var_lab = lst$var1_lab) +
        theme_bw(base_size = 14)
    })
    
    output$model_vars <- renderTable({
      validate(
        need(!is.null(lst$out), "")
      )
      validate(
        need(!is.null(lst$ref_table), "Select a model.")
      )
      lst$ref_table[lst$ref_table$plot, c("longname", "var", "units", "model")]
    })
    
    output$config_file <- renderText(paste0(cfg, collapse = "\n"))
    
    # Render LER yaml file to UI elements ----
    output$ler_ui <- renderUI({
      yaml_to_ui(config_file = config_file)
    })
    
    observeEvent(input$save_yaml, {
      ui_id <- get_ui_id(config_file = config_file)
      lst <- strsplit(ui_id, "/")
      ui_id <- gsub("/", "_", ui_id)
      unlist(lapply(lst, length))
      for(i in seq_along(lst)) {
        if(lst[[i]][[2]] %in% c("start", "stop")) {
          yaml$time$start <- paste(input[["time_start_date"]], 
                                   format(input[["time_start_time"]], "%H:%M:%S"))
          yaml$time$stop <- paste(input[["time_stop_date"]], 
                                  format(input[["time_stop_time"]], "%H:%M:%S"))
        } else {
          if(length(input[[ui_id[i]]]) > 1) {
            inp <- input[[ui_id[i]]]
          } else if(input[[ui_id[i]]] %in% c("TRUE", "FALSE")) {
            inp <- as.logical(input[[ui_id[i]]])
          } else if(input[[ui_id[i]]] == "") {
            inp <- "NULL"
          } else {
            inp <- input[[ui_id[i]]]
          }
          print(inp)
          if(length(lst[[i]]) == 2) {
            yaml[[lst[[i]][1]]][[lst[[i]][2]]] <- inp
          } else if(length(lst[[i]]) == 3) {
            print(lst[[i]])
            yaml[[lst[[i]][1]]][[lst[[i]][2]]][[lst[[i]][3]]] <- inp
          } else if(length(lst[[i]]) == 4) {
            yaml[[lst[[i]][1]]][[lst[[i]][2]]][[lst[[i]][3]]][[lst[[i]][4]]] <- inp
          }
        }
      }
      
      
      write_yaml(yaml = yaml, file = config_file)
      
    })

  }
  shinyApp(ui, server)
}

#' Inspect model output within a Shiny application
#'
#' Loads raw model output and allows for inspection withing a Shiny application.
#'
#' @name run_ler_shiny
#' @inheritParams export_config
#' @return Launches a shiny application
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
run_ler_shiny <- function(model, config_file) {
  
  require(shiny)
  require(shinydashboard)

  # model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  

  # on.exit({
  #   LakeEnsemblR:::close_nc(out)
  # })
  
  # cfg <- readLines(config_file)
  # yaml <- configr::read.config(config_file)

  # lakename <- basename(lake_dir)
  
  # ref_table <- nc[[model]]$ref_table
  # UI ----
  ui <- dashboardPage(
    dashboardHeader(title = "LakeEnsemblR"),
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Configuration", tabName = "configuration"),
        menuItem("Model output", tabName = "model_output")#,
                 # menuSubItem(text = "sub1", tabName = "sub1", icon = icon("line-chart"))
                 # )
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
                div(style = "display:inline-block;",
                    actionButton("save_yaml", "Save configuration", icon = icon("save")),
                    actionButton("export_config", "Export configuration", icon = icon("file-export"))
                    ),
                div(style = "display:inline-block;",
                  actionButton("run_ensemble", "Run ensemble", icon = icon("person-running")),
                  checkboxInput("parallel", "Run in parallel?", value = TRUE)
                ),
                fluidRow(
                  column(6,
                         h2("Configuration"),
                         uiOutput("ler_ui")
                         ),
                  column(6,
                         h2("YAML file"),
                         verbatimTextOutput("config_file")
                         )
                  ),
                # box(title = "Configuration", status = "primary", solidHeader = TRUE, 
                #     collapsible = FALSE,
                #     ),
                # box(title = "YAML file", solidHeader = TRUE, collapsible = TRUE,
                #     ),
                br(),

                ),
        tabItem(tabName = "sub1",
                h1("Sub1")
                ),
        tabItem(tabName = "input",
                h2("input")
                ),
        tabItem(tabName = "model_output",
                fluidRow(
                  column(3,
                         actionButton("open_output", "Open model output"),
                         uiOutput("model_sel"),
                         uiOutput("var_sel"),
                         conditionalPanel("input.open_output > 0",
                                          actionButton("close_output", "Close output"))
                  ),
                  column(9,
                         plotOutput("plot1", height = "400px"),
                         uiOutput("date_slider"),
                         br(),
                         conditionalPanel("input.open_output > 0",
                                          p(tags$b("Table 1."), " List of model output variables."),
                                          tableOutput("model_vars")
                                          )
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
    
    lst <- reactiveValues(out = NULL, var1 = NULL, var1_lab = NULL, plot1 = NULL,
                          ref_table = NULL, cfg = readLines(config_file),
                          yaml = configr::read.config(config_file))
    
    observeEvent(input$open_output, {
      lst$out <- connect_output(model = model, config_file = config_file)
      for(m in model) {
        lst$out[[m]]$ref_table <- model_var_dic[model_var_dic$model == m, ]
      }
    })
    observeEvent(input$close_output, {
      LakeEnsemblR:::close_nc(lst$out)
      lst$out <- NULL
      lst$var1 <- NULL
    })
    
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
      req(!is.null(lst$out) & length(lst$var1) == 1 & !is.null(lst$var1) &
            !is.null(lst$var1_lab))
      lst$plot1 <- plot_raw(con = lst$out, model = input$model, var = lst$var1,
                            xlim = input$xlim,
                            var_lab = lst$var1_lab) +
        theme_bw(base_size = 14)
    })
    
    output$plot1 <- renderPlot({
      validate(
        need(!is.null(lst$out), "Click 'Open model output'")
      )
      validate(
        need(length(lst$var1) == 1 & !is.null(lst$var1), "Select a variable.")
      )
      # plot_raw(con = lst$out, model = input$model, var = lst$var1, 
      #          xlim = input$xlim, var_lab = lst$var1_lab) +
      #   theme_bw(base_size = 14)
      validate(
        need(!is.null(lst$plot1), "Variable not available in model output.")
      )
      lst$plot1
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
    
    output$config_file <- renderText({
      paste0(lst$cfg, collapse = "\n")
    })
    
    # Render LER yaml file to UI elements ----
    output$ler_ui <- renderUI({
      yaml_to_ui(config_file = config_file)
    })
    
    observeEvent(input$save_yaml, {
      ui_id <- get_ui_id(config_file = config_file)
      id_lst <- strsplit(ui_id, "/")
      ui_id <- gsub("/", "_", ui_id)
      unlist(lapply(id_lst, length))
      for(i in seq_along(id_lst)) {
        if(id_lst[[i]][[2]] %in% c("start", "stop")) {
          lst$yaml$time$start <- paste(input[["time_start_date"]], 
                                   format(input[["time_start_time"]], "%H:%M:%S"))
          lst$yaml$time$stop <- paste(input[["time_stop_date"]], 
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
          if(length(id_lst[[i]]) == 2) {
            lst$yaml[[id_lst[[i]][1]]][[id_lst[[i]][2]]] <- inp
          } else if(length(id_lst[[i]]) == 3) {
            lst$yaml[[id_lst[[i]][1]]][[id_lst[[i]][2]]][[id_lst[[i]][3]]] <- inp
          } else if(length(id_lst[[i]]) == 4) {
            lst$yaml[[id_lst[[i]][1]]][[id_lst[[i]][2]]][[id_lst[[i]][3]]][[id_lst[[i]][4]]] <- inp
          }
        }
      }
      write_yaml(yaml = lst$yaml, file = config_file)
      lst$cfg <- readLines(config_file)
    })
    
    observeEvent(input$export_config, {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Exporting model configuration...",
                   detail = "This may take a while. This window will disappear
                     when it is completed.", value = 0.01)
      for(i in 1:9) {
        args <- rep(FALSE, 9)
        args[i] <- TRUE
        export_config(config_file = config_file, model = model, dirs = args[1], 
                      time = args[2], location = args[3], output_settings = args[4], 
                      meteo = args[5], init_cond = args[6], extinction = args[7], 
                      flow = args[8], model_parameters = args[9])
        progress$set(value = i/9)
      }
    })
    
    observeEvent(input$run_ensemble, {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Running ensemble of models...",
                   detail = "This may take a while. This window will disappear
                     when it is completed.", value = 0.2)
      run_ensemble(config_file = config_file, model = model, parallel = input$parallel)
      progress$set(value = 1)
    })

  }
  shinyApp(ui, server)
}

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
  
  # lakename <- basename(lake_dir)
  
  # ref_table <- nc[[model]]$ref_table
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
                              " ))),
      tabItems(
        tabItem(tabName = "configuration",
                h2("Configuration"),
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
                         plotOutput("plot2d", height = "700px"),
                         uiOutput("date_slider"),
                         br(),
                         p(tags$b("Table 1."), " List of model output variables."),
                         tableOutput("model_vars")
                         # dateRangeInput("xlim", "Date range", start = start, end = end, min = start, max = end, width = "100%")
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
    
  }
  shinyApp(ui, server)
}

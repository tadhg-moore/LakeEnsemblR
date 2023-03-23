#' Inspect model output within a Shiny application
#'
#' Loads raw model output and allows for inspection withing a Shiny application.
#'
#' @name inspect_output
#' @inheritParams export_config
#' @return Launches a shiny application
#' @import shiny
#' @import shinydashboard
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
inspect_output <- function(model, config_file) {
  
  # require(shiny)
  # require(magrittr)
  # require(dplyr)
  
  # model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  
  out <- connect_output(model = model, config_file = config_file)
  # close_nc(out)
  
  start <- as.Date(min(out[[1]]$Date))
  end <- as.Date(max(out[[1]]$Date))
  
  # lakename <- basename(lake_dir)
  
  # ref_table <- nc[[model]]$ref_table
  ui <- dashboardPage(
    dashboardHeader(title = "LakeEnsemblR"),
    dashboardSidebar(
      # sidebarMenuOutput("menu")
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
                              background-color: #18188c;
                              }
        .skin-blue .main-header .logo:hover {
        background-color: #18188c;
        }
        .skin-blue .main-header .navbar {
        background-color: #18188c;
        }
                              '))),
      tabsetPanel(
        tabPanel("Configuration"),
        tabPanel("Input"),
        tabPanel("Model output",
                 fluidRow(
                   column(3,
                          radioButtons("model", label = "Model", choices = model),
                          uiOutput("var_sel")
                   ),
                   column(9,
                          # h4(lakename),
                          plotOutput("plot2d", height = "700px"),
                          sliderInput("xlim",
                                      "Dates:",
                                      min = start,
                                      max = end,
                                      value = c(start, end),
                                      timeFormat="%Y-%m-%d", width = "100%")
                          # dateRangeInput("xlim", "Date range", start = start, end = end, min = start, max = end, width = "100%")
                   )
                 )
        ),
        tabPanel("LER output")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    lst <- reactiveValues(var2d = NULL, var1d = NULL, plot2d = NULL, plot1d = NULL, ref_table = NULL)
    
    output$var_sel <- renderUI({
      
      choices <- lst$ref_table$var
      selectInput("var1d", "Select variable", 
                  choices = choices)
      
    })
    observeEvent(input$model, {
      lst$var1d <- NULL
    })
    
    observe({
      req(length(input$model) == 1)
      lst$ref_table <- out[[input$model]]$ref_table
      lst$var1d <- input$var1d
    })
    
    output$plot2d <- renderPlot({
      plot_raw(con = out, model = input$model, var = input$var1d, xlim = input$xlim)
    })
    
  }
  shinyApp(ui, server)
}

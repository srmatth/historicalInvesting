#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_ui <- function(id){
  ns <- NS(id)
  mainPanel(
    fluidRow(
      style = "padding-top: 15px",
      column(
        width = 5,
        offset = 1,
        shinydashboard::valueBoxOutput(outputId = ns("cur_val"), width = 12)
      ),
      column(
        width = 5,
        shinydashboard::valueBoxOutput(outputId = ns("principle"), width = 12)
      )
    ),
    shinyjs::hidden(
      fluidRow(
        id = ns("progress"),
        style = "height:400px; vertical-align:middle;",
        col_6(
          offset = 3,
          br(),
          br(),
          br(),
          br(),
          shinyWidgets::progressBar(
            id = ns("prog"),
            value = 0,
            title = "Initializaing Data Setup",
            striped = TRUE
          )
        )
      )
    ),
    plotly::plotlyOutput(outputId = ns("plt")),
    shinyjs::hidden(
      uiOutput(outputId = ns("facet"))
    ),
    mod_btns_ui(ns("btns_ui_1"))
  )
}
    
#' body Server Function
#'
#' @noRd 
mod_body_server <- function(input, output, session, rv){
  ns <- session$ns
  
  #### Call Other Modules ----
  
  # The module that creates the customization boxes at the bottom
  callModule(mod_btns_server, "btns_ui_1", rv)
  
  #### Render Outputs ----
  
  # The box showing the principal amount that has been invested
  output$principle <- shinydashboard::renderValueBox({
    req(rv$data())
    validate(
      need(
        nrow(rv$data()) > 0,
        message = " "
      )
    )
    rv$data() %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(principle = sum(tot_amt)) %>%
      dplyr::ungroup() %>%
      dplyr::slice_max(date) %>%
      dplyr::pull(principle) %>%
      scales::dollar(accuracy = 1) %>%
      value_box(
        subtitle = "Amount Contributed",
        width = 12,
        background = "#942911",
        icon = icon("hand-holding-usd")
      )
  })
  # The box showing the current value of the given positions
  output$cur_val <- shinydashboard::renderValueBox({
    req(rv$data())
    validate(
      need(
        nrow(rv$data()) > 0,
        message = " "
      )
    )
    rv$data() %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(curr_value = sum(curr_value)) %>%
      dplyr::ungroup() %>%
      dplyr::slice_max(date) %>%
      dplyr::pull(curr_value) %>%
      scales::dollar(accuracy = 1) %>%
      value_box(
        subtitle = "Current Value",
        width = 12,
        background = "#942911",
        icon = icon("chart-line")
      )
  })
  # The plot that shows the basic line chart
  output$plt <- plotly::renderPlotly({
    req(rv$plt())
    plotly::ggplotly(rv$plt(), tooltip = "label") %>% 
      plotly::config(displayModeBar = FALSE)
  })
  # The plot that shows the faceted line chart (hidden by default)
  output$facet <- renderUI({
    req(rv$facet(), rv$ncol())
    plt_panels <- ggplot2::ggplot_build(rv$facet())$data[[1]]$PANEL %>% 
      unique() %>% 
      length()
    plt_nrow <- ceiling(plt_panels / rv$ncol())
    plotly::plotlyOutput(
      outputId = ns("facet_plt"),
      width = "100%",
      height = 250 * plt_nrow
    )
  })
  output$facet_plt <- plotly::renderPlotly({
    req(rv$facet(), rv$ncol())
    plotly::ggplotly(rv$facet(), tooltip = "label") %>%
      plotly::config(displayModeBar = FALSE)
  })
 
}
    
## To be copied in the UI
# mod_body_ui("body_ui_1")
    
## To be copied in the server
# callModule(mod_body_server, "body_ui_1")
 

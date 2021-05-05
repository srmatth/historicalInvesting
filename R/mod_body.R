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
    plotly::plotlyOutput(outputId = ns("plt")),
    mod_btns_ui(ns("btns_ui_1"))
  )
}
    
#' body Server Function
#'
#' @noRd 
mod_body_server <- function(input, output, session, rv){
  ns <- session$ns
  
  mod_btns_server("btns_ui_1", rv)
  
  output$principle <- shinydashboard::renderValueBox({
    req(rv$data())
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
  output$cur_val <- shinydashboard::renderValueBox({
    req(rv$data())
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
  
  output$plt <- plotly::renderPlotly({
    req(rv$data())
      p <- rv$data() %>%
        dplyr::mutate(date = lubridate::ymd(date)) %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(curr_value = sum(curr_value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          Date = stringr::str_c(date, "\nValue: ", scales::dollar(curr_value, accuracy = .01))
        ) %>%
        ggplot2::ggplot() +
        ggplot2::aes(
          x = date, 
          y = curr_value,
          label = Date
        ) +
        ggplot2::geom_line(color = "#942911") +
        ggplot2::theme_classic() +
        ggplot2::labs(
          title = "Value Over Time of Entire Portfolio"
        ) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Value ($)") +
        ggplot2::scale_y_continuous(
          labels = scales::dollar_format(accuracy = 1)
        ) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(
            fill = "transparent"
          ),
          panel.background = ggplot2::element_rect(
            fill = "transparent"
          )
        )
    plotly::ggplotly(p, tooltip = "label") %>% 
      plotly::config(displayModeBar = F)
  })
 
}
    
## To be copied in the UI
# mod_body_ui("body_ui_1")
    
## To be copied in the server
# callModule(mod_body_server, "body_ui_1")
 

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
    h1("Simulation for the Given Positions"),
    plotOutput(outputId = ns("plt"))
  )
}
    
#' body Server Function
#'
#' @noRd 
mod_body_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$plt <- renderPlot({
    req(rv$data())
    rv$data() %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(curr_value = sum(curr_value)) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = date, y = curr_value) +
      ggplot2::geom_line()
  })
 
}
    
## To be copied in the UI
# mod_body_ui("body_ui_1")
    
## To be copied in the server
# callModule(mod_body_server, "body_ui_1")
 

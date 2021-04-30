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
    h1("This is the main panel"),
    br(),
    br(),
    h3("This is a smaller heading")
  )
}
    
#' body Server Function
#'
#' @noRd 
mod_body_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_body_ui("body_ui_1")
    
## To be copied in the server
# callModule(mod_body_server, "body_ui_1")
 

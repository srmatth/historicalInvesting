#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  rv <- reactiveValues()
  # List the first level callModules here
  callModule(mod_sidebar_server, "sidebar_ui_1", rv = rv)
  callModule(mod_body_server, "body_ui_1", rv = rv)
}

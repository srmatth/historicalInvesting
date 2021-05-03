#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  sidebarPanel(
    h2("Enter Positions:"),
    h5(""),
    actionButton(
      inputId = ns("add"),
      label = "Add Position"
    )
  )
}
    
#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(input, output, session, rv){
  ns <- session$ns
  
  all_positions <- reactiveValues()
  
  position_1 <- callModule(mod_ticker_server, paste0("position_", 1))
  
  observe({
    all_positions[['1']] <- position_1()
    insertUI(
      selector = "h5",
      where    = "beforeEnd",
      ui       = tagList(mod_ticker_ui(paste0("position_", 1)))
    )
  })
  
  observeEvent(input$add, {
    
    btn <- sum(input$add, 1)
    
    insertUI(
      selector = "h5",
      where    = "beforeEnd",
      ui       = tagList(mod_ticker_ui(paste0("position_", btn)))
    )
    
    new_position <- callModule(mod_ticker_server, paste0("position_", btn))
    
    observeEvent(new_position(), {
      tmpFilters[[paste0("'", btn, "'")]] <- new_position()
    })
    
  })
 
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")
 

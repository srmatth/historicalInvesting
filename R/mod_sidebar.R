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
    hr(style = "color:black"),
    h5(""),
    fluidRow(
      class = "ticker-input",
      col_6(
        style = "padding: 4px",
        actionButton(
          inputId = ns("add"),
          label = "Add Position"
        )
      ),
      col_6(
        style = "padding: 4px",
        actionButton(
          inputId = ns("refresh"),
          label = "Remove All Positions"
        )
      )
    ),
    br(),
    fluidRow(
      class = "ticker-input",
      col_4(
        style = "padding: 4px",
        offset = 4,
        actionButton(
          inputId = ns("go"),
          label = "Run Simulation"
        )
      )
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
  insertUI(
    selector = "h5",
    where    = "beforeEnd",
    ui       = tagList(mod_ticker_ui(ns(paste0("position_", 1))))
  )
  
  observe({
    all_positions[["pos_1"]] <- position_1()
  })
  
  observeEvent(input$add, {
    
    btn <- sum(input$add, 1)
    
    insertUI(
      selector = "h5",
      where    = "beforeEnd",
      ui       = tagList(mod_ticker_ui(ns(paste0("position_", btn))))
    )
    
    new_position <- callModule(mod_ticker_server, paste0("position_", btn))
    
    observeEvent(new_position(), {
      all_positions[[paste0("pos_", btn)]] <- new_position()
    })
    
  })
  
  rv$data <- eventReactive(input$go, {
    l <- rvtl(all_positions)
    purrr::map_dfr(
      .x = l,
      .f = run_simulation
    )
  })
 
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")
 

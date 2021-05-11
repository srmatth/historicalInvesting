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
    # This h5 is where we will attach the position inputs
    h5(""),
    br(),
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
    ),
    br(),
    # Hidden warning to alert the user if a ticker was not found 
    # on Yahoo Finance
    shinyjs::hidden(
      fluidRow(
        id = ns("warning"),
        style = "background-color: #acc3b6",
        col_8(
          offset = 2,
          style = "float:left",
          p(
            HTML('<i class="fas fa-exclamation-triangle"></i>'),
            tags$em(
              stringr::str_c(
                "Warning: Data was not found for all the tickers you entered. ",
                "View position charts to see which tickers were included."
              )
            ),
            style = "font-family: Arial; font-size: 11pt;"
          )
        )
      )
    ),
    br()
  )
}
    
#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(input, output, session, rv, outer_session){
  ns <- session$ns
  
  # Reactive values list to house the positions
  all_positions <- reactiveValues()
  # Create the first position by default when the app starts
  position_1 <- callModule(mod_ticker_server, paste0("position_", 1))
  insertUI(
    selector = "h5",
    where    = "beforeEnd",
    ui       = tagList(mod_ticker_ui(ns(paste0("position_", 1))))
  )
  # Add the first position to the positions list
  observe({
    all_positions[["pos_1"]] <- position_1()
  })
  # To run when the user adds an additional position
  observeEvent(input$add, {
    
    btn <- sum(input$add, input$refresh, 1)
    
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
  # When the run simulation button is clicked, create the data
  # And update the progress bar at the same time
  rv$data <- eventReactive(input$go, {
    l <- rvtl(all_positions)
    sp500 <- get_historical_prices("%5EGSPC")
    d <- purrr::map2_dfr(
      .x = l,
      .y = 1:length(l),
      .f = ~{
        shinyWidgets::updateProgressBar(
          session = outer_session,
          id = "body_ui_1-prog",
          value = .y / length(l) * 100,
          title = stringr::str_c("Downloading Data for ", .x$ticker)
        )
        run_simulation(.x, sp500)
      }
    )
    if (dplyr::n_distinct(d$ticker) != length(l)) {
      shinyjs::show(ns("warning"), asis = TRUE)
    }
    shinyjs::hide("body_ui_1-progress", asis = TRUE)
    shinyWidgets::updateProgressBar(
      session = outer_session,
      id = "body_ui_1-prog",
      value = 0,
      title = stringr::str_c("Initializing Data Setup")
    )
    d
  })
  # Get rid of all inputs when the user resets all positions
  observeEvent(input$refresh, {
    shinyjs::hide(ns("warning"), asis = TRUE)
    btn <- sum(input$add, input$refresh, 1)
    all_positions <<- NULL
    
    removeUI(
      selector = ".ticker-group",
      multiple = TRUE,
      immediate = TRUE
    )
    
    all_positions <<- rv()
    
    insertUI(
      selector = "h5",
      where    = "beforeEnd",
      ui       = tagList(mod_ticker_ui(ns(paste0("position_", btn))))
    )
    
    newest_position <- callModule(mod_ticker_server, paste0("position_", btn))
    
    observeEvent(newest_position(), {
      all_positions[[paste0("pos_", btn)]] <- newest_position()
    })
  })
  # Shows/hides necessary when the user clicks "run simulation"
  observeEvent(input$go, {
    shinyjs::hide(ns("warning"), asis = TRUE)
    shinyjs::hide("body_ui_1-facet", asis = TRUE)
    shinyjs::hide("body_ui_1-plt", asis = TRUE)
    shinyjs::show("body_ui_1-progress", asis = TRUE)
    shinyjs::show("body_ui_1-btns_ui_1-buttons", asis = TRUE)
    shinyjs::click("body_ui_1-btns_ui_1-reg", asis = TRUE)
  })
 
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")
 

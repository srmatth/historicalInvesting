#' ticker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ticker_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      fluidRow(
        class = "ticker-input",
        col_4(
          style = "margin: 5px, 0",
          textInput(
            inputId = ns("ticker"),
            label = "Ticker",
            value = NULL,
            placeholder = "e.g. AAPL"
          )
        ),
        col_4(
          style = "margin: 5px, 0",
          dateInput(
            inputId = ns("start_date"),
            label = "Start Date",
            value = NULL
          )
        ),
        col_4(
          style = "margin: 5px, 0",
          shinyWidgets::autonumericInput(
            inputId = ns("start_amt"),
            label = "Amount",
            value = 1000,
            currencySymbol = "$",
            currencySymbolPlacement = "p",
            digitGroupSeparator = ",",
            decimalCharacter = ".",
            decimalPlaces = 2
          )
        )
      ),
      fluidRow(
        class = "ticker-input",
        div(
          h4("Additional Deposits", style = "margin:0"),
          style = "text-align:center; margin:0; padding:0"
        ),
        col_4(
          style = "margin: 5px, 0",
          offset = 2,
          selectInput(
            inputId = ns("freq"),
            label = "Deposit Every...",
            choices = c(
              "Never",
              "Day",
              "Week",
              "2 Weeks",
              "Month",
              "Quarter",
              "6 Months",
              "Year"
            )
          )
        ),
        col_4(
          style = "margin: 5px, 0",
          shinyWidgets::autonumericInput(
            inputId = ns("add_amt"),
            label = "Amount",
            value = 0,
            currencySymbol = "$",
            currencySymbolPlacement = "p",
            digitGroupSeparator = ",",
            decimalCharacter = ".",
            decimalPlaces = 2
          )
        ),
        col_4(
          offset = 4,
          shinyWidgets::prettyCheckbox(
            inputId = ns("reinvest"),
            label = "Reinvest Dividends",
            outline = TRUE,
            value = TRUE,
            icon = icon("check")
          )
        )
      ),
      hr(style = "color:black"),
      br()
    )
  )
}
    
#' ticker Server Function
#'
#' @noRd 
mod_ticker_server <- function(input, output, session){
  ns <- session$ns
  
  reactive({
    l <- list(
        ticker = input$ticker,
        start_date = input$start_date,
        start_amt = input$start_amt,
        freq = input$freq,
        add_amt = input$add_amt,
        reinvest = input$reinvest
    )
    
    return(l)
  })
 
}
    
## To be copied in the UI
# mod_ticker_ui("ticker_ui_1")
    
## To be copied in the server
# callModule(mod_ticker_server, "ticker_ui_1")
 

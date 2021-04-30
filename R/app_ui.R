#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      div(
        class = "custom-header",
        h1("Historical Stock Simulations")
      ),
      sidebarLayout(
        sidebarPanel = mod_sidebar_ui("sidebar_ui_1"),
        mainPanel = mod_body_ui("body_ui_1")
      ),
      div(
        class = "custom-footer",
        p(
          "Stock price data is obtained from ",
          tags$a(
            href = "https://finance.yahoo.com/",
            "Yahoo Finance"
          ),
          " and dividend data is obtained from ",
          tags$a(
            href = "https://www.dividendinformation.com/",
            "Dividend Information."
          ),
          br(),
          "This in no way constitutes financial advice.  ",
          "Past success does not guarantee future results.",
          br(),
          stringr::str_c("Version ", sessionInfo()$otherPkgs$historicalInvesting$Version)
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'historicalInvesting'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


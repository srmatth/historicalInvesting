#' btns UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_btns_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      fluidRow(
        id = ns("buttons"),
        # Customization for the basic line chart
        col_6(
          col_12(
            class = "chart-opts",
            col_6(
              actionButton(
                inputId = ns("reg"),
                label = "View Line Chart"
              )
            ),
            col_6(
              style = "text-align:right",
              actionButton(
                inputId = ns("reg_show"),
                label = HTML('Show Options   <i class="fas fa-plus"></i>'),
                class = "expand"
              ),
              shinyjs::hidden(
                actionButton(
                  inputId = ns("reg_hide"),
                  label = HTML('Hide Options   <i class="fas fa-minus"></i>'),
                  class = "expand"
                )
              )
            ),
            shinyjs::hidden(
              col_12(
                id = ns("reg_opts"),
                br(),
                textInput(
                  inputId = ns("reg_line_color"),
                  label = "Line Color",
                  value = "#942911"
                ),
                textInput(
                  inputId = ns("reg_bg_color"),
                  label = "Background Color",
                  value = "transparent"
                ),
                textInput(
                  inputId = ns("reg_font_family"),
                  label = "Font Family",
                  value = "Arial"
                ),
                textInput(
                  inputId = ns("reg_font_color"),
                  label = "Font Color",
                  value = "black"
                ),
                textInput(
                  inputId = ns("reg_title"),
                  label = "Plot Title",
                  value = "Value of Portfolio Over Time"
                ),
                shinyWidgets::prettyCheckbox(
                  inputId = ns("reg_sp500"),
                  label = "Include S&P 500 Overlay",
                  outline = TRUE,
                  value = FALSE,
                  icon = icon("check")
                ),
                shinyjs::hidden(
                  textInput(
                    inputId = ns("reg_sp500_color"),
                    label = "S&P 500 Line Color",
                    value = "#9D8420"
                  )
                ),
                shinyWidgets::prettyCheckbox(
                  inputId = ns("reg_contributed"),
                  label = "Include Amount Contributed Overlay",
                  outline = TRUE,
                  value = FALSE,
                  icon = icon("check")
                ),
                shinyjs::hidden(
                  textInput(
                    inputId = ns("reg_contributed_color"),
                    label = "Contributed Line Color",
                    value = "gray"
                  )
                ),
                textInput(
                  inputId = ns("reg_filename"),
                  label = "Download File Name (no extension)",
                  value = "Historical Chart"
                )
              )
            ),
            col_6(
              offset = 3,
              style = "padding-top:15px",
              downloadButton(
                outputId = ns("reg_download"),
                label = "Download PNG",
                icon = icon("download")
              )
            )
          )
        ),
        # Customization for the faceted line chart
        col_6(
          col_12(
            class = "chart-opts",
            col_6(
              actionButton(
                inputId = ns("facet"),
                label = "View Position Charts"
              )
            ),
            col_6(
              style = "text-align:right",
              actionButton(
                inputId = ns("facet_show"),
                label = HTML('Show Options   <i class="fas fa-plus"></i>'),
                class = "expand"
              ),
              shinyjs::hidden(
                actionButton(
                  inputId = ns("facet_hide"),
                  label = HTML('Hide Options   <i class="fas fa-minus"></i>'),
                  class = "expand"
                )
              )
            ),
            shinyjs::hidden(
              col_12(
                id = ns("facet_opts"),
                br(),
                textInput(
                  inputId = ns("facet_line_color"),
                  label = "Line Color",
                  value = "#942911"
                ),
                textInput(
                  inputId = ns("facet_bg_color"),
                  label = "Background Color",
                  value = "transparent"
                ),
                textInput(
                  inputId = ns("facet_font_family"),
                  label = "Font Family",
                  value = "Arial"
                ),
                textInput(
                  inputId = ns("facet_font_color"),
                  label = "Font Color",
                  value = "black"
                ),
                textInput(
                  inputId = ns("facet_title"),
                  label = "Plot Title",
                  value = "Value of Positions Over Time"
                ),
                numericInput(
                  inputId = ns("facet_ncol"),
                  label = "Number of Columns for Plots",
                  min = 1,
                  max = 4,
                  value = 2,
                  step = 1
                ),
                shinyWidgets::prettyCheckbox(
                  inputId = ns("facet_sp500"),
                  label = "Include S&P 500 Overlay",
                  outline = TRUE,
                  value = FALSE,
                  icon = icon("check")
                ),
                shinyjs::hidden(
                  textInput(
                    inputId = ns("facet_sp500_color"),
                    label = "S&P 500 Line Color",
                    value = "#9D8420"
                  )
                ),
                shinyWidgets::prettyCheckbox(
                  inputId = ns("facet_contributed"),
                  label = "Include Amount Contributed Overlay",
                  outline = TRUE,
                  value = FALSE,
                  icon = icon("check")
                ),
                shinyjs::hidden(
                  textInput(
                    inputId = ns("facet_contributed_color"),
                    label = "Contributed Line Color",
                    value = "gray"
                  )
                ),
                textInput(
                  inputId = ns("facet_filename"),
                  label = "Download File Name (no extension)",
                  value = "Historical Chart by Ticker"
                )
              )
            ),
            col_6(
              offset = 3,
              style = "padding-top:15px",
              downloadButton(
                outputId = ns("facet_download"),
                label = "Download PNG",
                icon = icon("download")
              )
            )
          )
        )
      )
    )
  )
}

#' btns Server Functions
#'
#' @noRd 
mod_btns_server <- function(input, output, session, rv){
    ns <- session$ns
    
    #### Observers ----
    
    # To trigger when the chart options are shown
    observeEvent(input$reg_show, {
      shinyjs::hide(ns("reg_show"), asis = TRUE)
      shinyjs::show(ns("reg_hide"), asis = TRUE)
      shinyjs::show(ns("reg_opts"), asis = TRUE)
    })
    # To trigger when the char options are hidden
    observeEvent(input$reg_hide, {
      shinyjs::hide(ns("reg_hide"), asis = TRUE)
      shinyjs::hide(ns("reg_opts"), asis = TRUE)
      shinyjs::show(ns("reg_show"), asis = TRUE)
    })
    # To show/hide the line color for the contributed amount
    observe({
      if (input$reg_contributed) {
        shinyjs::show(ns("reg_contributed_color"), asis = TRUE)
      } else {
        shinyjs::hide(ns("reg_contributed_color"), asis = TRUE)
      }
    })
    # To show/hide the line color for the S&P500 overlay
    observe({
      if (input$reg_sp500) {
        shinyjs::show(ns("reg_sp500_color"), asis = TRUE)
      } else {
        shinyjs::hide(ns("reg_sp500_color"), asis = TRUE)
      }
    })
    # To show/hide the color of the contributed amounts
    observe({
      if (input$facet_contributed) {
        shinyjs::show(ns("facet_contributed_color"), asis = TRUE)
      } else {
        shinyjs::hide(ns("facet_contributed_color"), asis = TRUE)
      }
    })
    # To show/hide the color of the S&P500 overlay
    observe({
      if (input$facet_sp500) {
        shinyjs::show(ns("facet_sp500_color"), asis = TRUE)
      } else {
        shinyjs::hide(ns("facet_sp500_color"), asis = TRUE)
      }
    })
    # To trigger when the facet chart options are shown
    observeEvent(input$facet_show, {
      shinyjs::hide(ns("facet_show"), asis = TRUE)
      shinyjs::show(ns("facet_hide"), asis = TRUE)
      shinyjs::show(ns("facet_opts"), asis = TRUE)
    })
    # To trigger when the facet chart options are hidden
    observeEvent(input$facet_hide, {
      shinyjs::hide(ns("facet_hide"), asis = TRUE)
      shinyjs::hide(ns("facet_opts"), asis = TRUE)
      shinyjs::show(ns("facet_show"), asis = TRUE)
    })
    # To run when the user clicks the "View Chart by Position" button
    observeEvent(input$facet, {
      shinyjs::hide("body_ui_1-plt", asis = TRUE)
      shinyjs::show("body_ui_1-facet", asis = TRUE)
    })
    # To run when the user clicks the View Chart button
    observeEvent(input$reg, {
      shinyjs::hide("body_ui_1-facet", asis = TRUE)
      shinyjs::show("body_ui_1-plt", asis = TRUE)
    })
    
    #### Reactive Values ----
    
    # The basic line plot, created with any specified customization
    rv$plt <- eventReactive(input$reg, {
      req(rv$data())
      validate(
        need(
          nrow(rv$data()) > 1,
          message = "None of the tickers entered returned any data!"
        )
      )
      p <- rv$data() %>%
        dplyr::mutate(date = lubridate::ymd(date)) %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          curr_value = sum(curr_value), 
          tot_amt = sum(tot_amt),
          curr_sp500 = sum(curr_sp500)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          contrib_overlay = input$reg_contributed,
          sp500_overlay = input$reg_sp500,
          Date = stringr::str_c(
            date, 
            "\nValue: ", 
            scales::dollar(curr_value, accuracy = .01),
            dplyr::if_else(
              contrib_overlay, 
              stringr::str_c("\nAmount Contributed: ", scales::dollar(tot_amt, accuracy = 0.01)),
              rep(" ", times = nrow(.))
            ),
            dplyr::if_else(
              sp500_overlay, 
              stringr::str_c("\nS&P 500 Value: ", scales::dollar(curr_sp500, accuracy = 0.01)),
              rep(" ", times = nrow(.))
            )
          )
        ) %>%
        ggplot2::ggplot() +
        ggplot2::aes(
          x = date, 
          y = curr_value,
          label = Date
        ) +
        ggplot2::geom_line(color = input$reg_line_color) +
        ggplot2::theme_classic() 
      if (input$reg_contributed) {
        p <- p +
          ggplot2::geom_step(
            ggplot2::aes(x = date, y = tot_amt),
            color = input$reg_contributed_color
          )
      }
      if (input$reg_sp500) {
        p <- p +
          ggplot2::geom_line(
            ggplot2::aes(x = date, y = curr_sp500),
            color = input$reg_sp500_color
          )
      }
      p +
        ggplot2::labs(
          title = input$reg_title
        ) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Value ($)") +
        ggplot2::scale_y_continuous(
          labels = scales::dollar_format(accuracy = 1)
        ) +
        ggplot2::theme(
          text = ggplot2::element_text(
            family = input$reg_font_family,
            color = input$reg_font_color
          ),
          axis.text = ggplot2::element_text(
            family = input$reg_font_family,
            color = input$reg_font_color
          ),
          axis.line = ggplot2::element_line(
            color = input$reg_font_color
          ),
          plot.background = ggplot2::element_rect(
            fill = input$reg_bg_color
          ),
          panel.background = ggplot2::element_rect(
            fill = input$reg_bg_color
          ),
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    })
    # The facet plot, also created with any customization
    rv$facet <- eventReactive(input$facet, {
      req(rv$data())
      validate(
        need(
          nrow(rv$data()) > 1,
          message = "None of the tickers entered returned any data!"
        )
      )
      p <- rv$data() %>%
        dplyr::mutate(date = lubridate::ymd(date)) %>%
        dplyr::mutate(
          contrib_overlay = input$facet_contributed,
          sp500_overlay = input$facet_sp500,
          Date = stringr::str_c(
            date, 
            "\nValue: ", 
            scales::dollar(curr_value, accuracy = .01),
            dplyr::if_else(
              contrib_overlay, 
              stringr::str_c("\nAmount Contributed: ", scales::dollar(tot_amt, accuracy = 0.01)),
              rep(" ", times = nrow(.))
            ),
            dplyr::if_else(
              sp500_overlay, 
              stringr::str_c("\nS&P 500 Value: ", scales::dollar(curr_sp500, accuracy = 0.01)),
              rep(" ", times = nrow(.))
            )
          )
        ) %>%
        ggplot2::ggplot() +
        ggplot2::aes(
          x = date, 
          y = curr_value,
          label = Date
        ) +
        ggplot2::geom_line(color = input$facet_line_color) 
      if (input$facet_contributed) {
        p <- p +
          ggplot2::geom_step(
            ggplot2::aes(x = date, y = tot_amt),
            color = input$facet_contributed_color
          )
      }
      if (input$facet_sp500) {
        p <- p +
          ggplot2::geom_line(
            ggplot2::aes(x = date, y = curr_sp500),
            color = input$reg_sp500_color
          )
      }
      p +
        ggplot2::theme_classic() +
        ggplot2::labs(
          title = input$facet_title
        ) +
        ggplot2::facet_wrap(~ticker, ncol = input$facet_ncol) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Value ($)") +
        ggplot2::scale_y_continuous(
          labels = scales::dollar_format(accuracy = 1)
        ) +
        ggplot2::theme(
          text = ggplot2::element_text(
            family = input$facet_font_family,
            color = input$facet_font_color
          ),
          axis.text = ggplot2::element_text(
            family = input$facet_font_family,
            color = input$facet_font_color
          ),
          axis.line = ggplot2::element_line(
            color = input$facet_font_color
          ),
          plot.background = ggplot2::element_rect(
            fill = input$facet_bg_color
          ),
          panel.background = ggplot2::element_rect(
            fill = input$facet_bg_color
          ),
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    })
    # The number of columns in the facet plot (for sizing of the output)
    rv$ncol <- eventReactive(input$facet, {input$facet_ncol})
    
    #### Render Outputs ----
    
    # Download output for the line chart
    output$reg_download <- downloadHandler(
      filename = function() {
        stringr::str_c(
          isolate(input$reg_filename),
          ".png"
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = rv$plt(), bg = isolate(input$reg_bg_color))
      }
    )
    # Download output for the line chart by position
    output$facet_download <- downloadHandler(
      filename = function() {
        stringr::str_c(
          isolate(input$facet_filename),
          ".png"
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = rv$facet(), bg = isolate(input$facet_bg_color))
      }
    )
    
}

## To be copied in the UI
# mod_btns_ui("btns_ui_1")

## To be copied in the server
# mod_btns_server("btns_ui_1")

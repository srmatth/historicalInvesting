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
        col_4(
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
                  inputId = ns("reg_title"),
                  label = "Plot Title",
                  value = "Value of Portfolio Over Time"
                ),
                checkboxInput(
                  inputId = ns("reg_sp500"),
                  label = "Include S&P 500 Overlay",
                  value = FALSE
                ),
                checkboxInput(
                  inputId = ns("reg_contributed"),
                  label = "Include Amount Contributed Overlay",
                  value = FALSE
                )
              )
            ),
            col_6(
              offset = 3,
              style = "padding-top:15px",
              actionButton(
                inputId = ns("reg_download"),
                label = "Download PNG",
                icon = icon("download")
              )
            )
          )
        ),
        col_4(
          col_12(
            class = "chart-opts",
            col_6(
              actionButton(
                inputId = ns("anim"),
                label = "View Animated Chart"
              )
            ),
            col_6(
              style = "text-align:right",
              actionButton(
                inputId = ns("anim_show"),
                label = HTML('Show Options   <i class="fas fa-plus"></i>'),
                class = "expand"
              ),
              shinyjs::hidden(
                actionButton(
                  inputId = ns("anim_hide"),
                  label = HTML('Hide Options   <i class="fas fa-minus"></i>'),
                  class = "expand"
                )
              )
            ),
            shinyjs::hidden(
              col_12(
                id = ns("anim_opts"),
                br(),
                textInput(
                  inputId = ns("anim_line_color"),
                  label = "Line Color",
                  value = "#370F06"
                ),
                textInput(
                  inputId = ns("anim_bg_color"),
                  label = "Background Color",
                  value = "transparent"
                ),
                textInput(
                  inputId = ns("anim_font_family"),
                  label = "Font Family",
                  value = "Arial"
                ),
                textInput(
                  inputId = ns("anim_title"),
                  label = "Plot Title",
                  value = "Value of Portfolio Over Time"
                ),
                checkboxInput(
                  inputId = ns("anim_sp500"),
                  label = "Include S&P 500 Overlay",
                  value = FALSE
                ),
                checkboxInput(
                  inputId = ns("anim_contributed"),
                  label = "Include Amount Contributed Overlay",
                  value = FALSE
                )
              )
            ),
            col_6(
              offset = 3,
              style = "padding-top:15px",
              actionButton(
                inputId = ns("anim_download"),
                label = "Download GIF"
              )
            )
          )
        ),
        col_4(
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
                  value = "#370F06"
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
                  inputId = ns("facet_title"),
                  label = "Plot Title",
                  value = "Value of Portfolio Over Time"
                ),
                checkboxInput(
                  inputId = ns("facet_sp500"),
                  label = "Include S&P 500 Overlay",
                  value = FALSE
                ),
                checkboxInput(
                  inputId = ns("facet_contributed"),
                  label = "Include Amount Contributed Overlay",
                  value = FALSE
                )
              )
            ),
            col_6(
              offset = 3,
              style = "padding-top:15px",
              actionButton(
                inputId = ns("facet_download"),
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
    
    observeEvent(input$reg_show, {
      shinyjs::hide(ns("reg_show"), asis = TRUE)
      shinyjs::show(ns("reg_hide"), asis = TRUE)
      shinyjs::show(ns("reg_opts"), asis = TRUE)
    })
    observeEvent(input$reg_hide, {
      shinyjs::hide(ns("reg_hide"), asis = TRUE)
      shinyjs::hide(ns("reg_opts"), asis = TRUE)
      shinyjs::show(ns("reg_show"), asis = TRUE)
    })
    
    observeEvent(input$anim_show, {
      shinyjs::hide(ns("anim_show"), asis = TRUE)
      shinyjs::show(ns("anim_hide"), asis = TRUE)
      shinyjs::show(ns("anim_opts"), asis = TRUE)
    })
    observeEvent(input$anim_hide, {
      shinyjs::hide(ns("anim_hide"), asis = TRUE)
      shinyjs::hide(ns("anim_opts"), asis = TRUE)
      shinyjs::show(ns("anim_show"), asis = TRUE)
    })
    
    observeEvent(input$facet_show, {
      shinyjs::hide(ns("facet_show"), asis = TRUE)
      shinyjs::show(ns("facet_hide"), asis = TRUE)
      shinyjs::show(ns("facet_opts"), asis = TRUE)
    })
    observeEvent(input$facet_hide, {
      shinyjs::hide(ns("facet_hide"), asis = TRUE)
      shinyjs::hide(ns("facet_opts"), asis = TRUE)
      shinyjs::show(ns("facet_show"), asis = TRUE)
    })
    
    rv$plt <- eventReactive(input$reg, {
      req(rv$data())
      rv$data() %>%
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
        ggplot2::geom_line(color = input$reg_line_color) +
        ggplot2::theme_classic() +
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
            family = input$reg_font_family
          ),
          plot.background = ggplot2::element_rect(
            fill = input$reg_bg_color
          ),
          panel.background = ggplot2::element_rect(
            fill = input$reg_bg_color
          )
        )
    })
    
    rv$anim <- eventReactive(input$anim, {
      req(rv$data())
      rv$data() %>%
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
        ggplot2::geom_line(color = input$reg_line_color) +
        ggplot2::geom_point(color = input$reg_line_color) +
        ggplot2::theme_classic() +
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
            family = input$reg_font_family
          ),
          plot.background = ggplot2::element_rect(
            fill = input$reg_bg_color
          ),
          panel.background = ggplot2::element_rect(
            fill = input$reg_bg_color
          )
        ) +
        gganimate::transition_reveal(date)
    })
    
    rv$facet <- eventReactive(input$facet, {
      req(rv$data())
      rv$data() %>%
        dplyr::mutate(date = lubridate::ymd(date)) %>%
        dplyr::mutate(
          Date = stringr::str_c(date, "\nValue: ", scales::dollar(curr_value, accuracy = .01))
        ) %>%
        ggplot2::ggplot() +
        ggplot2::aes(
          x = date, 
          y = curr_value,
          label = Date
        ) +
        ggplot2::geom_line(color = input$reg_line_color) +
        ggplot2::theme_classic() +
        ggplot2::labs(
          title = input$reg_title
        ) +
        ggplot2::facet_wrap(~ticker) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Value ($)") +
        ggplot2::scale_y_continuous(
          labels = scales::dollar_format(accuracy = 1)
        ) +
        ggplot2::theme(
          text = ggplot2::element_text(
            family = input$reg_font_family
          ),
          plot.background = ggplot2::element_rect(
            fill = input$reg_bg_color
          ),
          panel.background = ggplot2::element_rect(
            fill = input$reg_bg_color
          )
        )
    })
    
    observeEvent(input$facet, {
      shinyjs::hide("body_ui_1-plt", asis = TRUE)
      shinyjs::hide("body_ui_1-anim", asis = TRUE)
      shinyjs::show("body_ui_1-facet", asis = TRUE)
    })
    observeEvent(input$reg, {
      shinyjs::hide("body_ui_1-facet", asis = TRUE)
      shinyjs::hide("body_ui_1-anim", asis = TRUE)
      shinyjs::show("body_ui_1-plt", asis = TRUE)
    })
    observeEvent(input$anim, {
      shinyjs::hide("body_ui_1-plt", asis = TRUE)
      shinyjs::hide("body_ui_1-facet", asis = TRUE)
      shinyjs::show("body_ui_1-anim", asis = TRUE)
    })
    
}

## To be copied in the UI
# mod_btns_ui("btns_ui_1")

## To be copied in the server
# mod_btns_server("btns_ui_1")

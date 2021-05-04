#' Value Box
#'
#' This is a function that creates a value box but allows you to customize the
#' colors inside the value box.
#'
#' This function build on the valueBox functions from the `{shinydashboard}` package.
#' Having color arguments allows the valueboxes in the Shiny Apps to be more in line
#' with the cla color scheme.
#' The function was adapted from
#' \href{https://community.rstudio.com/t/shinydashboard-custom-box-colors-to-match-brand/14147/5}{RStudioCommunity}
#' and was posted by the user \emph{efhopkins} on 2018-09-18.
#'
#' @param value the value to be displayed in the value box, typically a numeric
#' @param subtitle the text that will be displayed underneath the value,
#'   typically a string
#' @param icon the icon that will appear on the right side of the value box,
#'   from `icon()`
#' @param color the color of the text in hex format, generally '#FFFFFF'
#' @param background the color of the background in hex format
#' @param width the width of the value box (bootstrap grid system), defaults to 4
#' @param href an optional argument for adding a link to the value box
#'
#' @return html tags that can be placed inside
#'   `shinydashboard::renderValueBox()`
#'
#' @export
value_box <- function (value, subtitle, icon = NULL,
                       color = "#ffffff", background = "#2180BC", width = 4, href = NULL) {
  
  style <- paste0("color: ", color, "; background-color: ", background, "; box-shadow: 4px 4px #370f06;")
  
  boxContent <- div(class = "small-box", style = style,
                    div(class = "inner", h3(value, style = "white-space:normal"), p(subtitle)), if (!is.null(icon))
                      div(class = "icon-large", icon))
  if (!is.null(href))
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width))
    paste0("col-sm-", width), boxContent)
}
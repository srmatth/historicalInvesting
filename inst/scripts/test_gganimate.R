l <- list(
  list(
    ticker = "T",
    start_date = lubridate::ymd("2016-05-05"),
    start_amt = 1000,
    freq = "Never",
    add_amt = 0,
    reinvest = TRUE
  ),
  list(
    ticker = "ADBE",
    start_date = lubridate::ymd("2016-05-05"),
    start_amt = 1000,
    freq = "Never",
    add_amt = 0,
    reinvest = TRUE
  ),
  list(
    ticker = "UNM",
    start_date = lubridate::ymd("2016-05-05"),
    start_amt = 1000,
    freq = "Never",
    add_amt = 0,
    reinvest = TRUE
  ),
  list(
    ticker = "NHI",
    start_date = lubridate::ymd("2016-05-05"),
    start_amt = 1000,
    freq = "Never",
    add_amt = 0,
    reinvest = TRUE
  ),
  list(
    ticker = "VZ",
    start_date = lubridate::ymd("2016-05-05"),
    start_amt = 1000,
    freq = "Never",
    add_amt = 0,
    reinvest = TRUE
  )
)

dat <- purrr::map_dfr(l, run_simulation)


p <- dat %>%
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
  ggplot2::geom_line(color = "#942911") +
  ggplot2::geom_point(color = "#942911") +
  ggplot2::theme_classic() +
  ggplot2::labs(
    title = "Value Over Time of Entire Portfolio"
  ) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Value ($)") +
  ggplot2::scale_y_continuous(
    labels = scales::dollar_format(accuracy = 1)
  ) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = "transparent"
    ),
    panel.background = ggplot2::element_rect(
      fill = "transparent"
    ),
    aspect.ratio = 0.5
  ) +
  gganimate::transition_reveal(date)
gganimate::animate(p, end_pause = 30)

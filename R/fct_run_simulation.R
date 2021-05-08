#' run_simulation 
#'
#' @description A fct function
#' 
#' @param l a list of inputs from the user that 
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
run_simulation <- function(l, sp500) {
  prices <- get_historical_prices(l$ticker) %>%
    dplyr::filter(date > l$start_date) %>%
    dplyr::select(ticker, date, close)
  if (nrow(prices) > 0) {
    sp500 <- sp500  %>%
      dplyr::filter(date > l$start_date) %>%
      dplyr::select(ticker, date, close)
    start_price <- prices %>%
      dplyr::pull(close) %>%
      `[`(1)
    start_price_sp500 <- sp500 %>%
      dplyr::pull(close) %>%
      `[`(1)
    pcts <- prices %>%
      dplyr::mutate(
        pct_growth = close / start_price,
        pct_growth_sp500 = sp500$close / start_price_sp500
      )
    if (l$freq != "Never") {
      deposits <- data.frame(
        date = seq.Date(lubridate::ymd(l$start_date), Sys.Date(), by = tolower(l$freq)),
        amount = l$add_amt
      ) 
      deposits[1, 2] <- l$start_amt
      deposits <- deposits %>%
        dplyr::mutate(tot_amt = cumsum(amount))
      principle <- pcts %>%
        dplyr::full_join(deposits, by = "date") %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(tot_amt = zoo::na.locf(tot_amt, na.rm = TRUE)) %>%
        dplyr::filter(!is.na(pct_growth)) %>%
        dplyr::select(-amount)
    } else {
      principle <- pcts %>%
        dplyr::mutate(tot_amt = l$start_amt)
    }
    
    if (l$reinvest) {
      dividends <- get_historical_dividends(l$ticker) 
      if (nrow(dividends) > 0) {
        dividends <- dividends %>%
          dplyr::arrange(date) %>%
          dplyr::left_join(principle %>% dplyr::select(-ticker), by = "date") %>%
          dplyr::filter(not_na(close)) %>%
          dplyr::mutate(
            curr_value = pct_growth * tot_amt,
            num_shares = curr_value / close,
            dividend = cumsum(num_shares * amount_per_share),
            new_curr_val = curr_value + dividend,
            new_principle = new_curr_val / pct_growth,
            add_dividend = new_principle - tot_amt
          ) %>%
          dplyr::select(date, add_dividend)
        
        principle <- principle %>%
          dplyr::left_join(dividends, by = "date") %>%
          dplyr::mutate(
            add_dividend = zoo::na.locf(add_dividend, na.rm = FALSE),
            base_amount = ifelse(is.na(add_dividend), tot_amt, tot_amt + add_dividend)
          )
      } else {
        principle <- principle %>%
          dplyr::mutate(add_dividend = NA, base_amount = tot_amt)
      }
    } else {
      principle <- principle %>%
        dplyr::mutate(add_dividend = NA, base_amount = tot_amt)
    }
    
    d <- principle %>%
      dplyr::mutate(
        curr_value = base_amount * pct_growth,
        curr_sp500 = base_amount * pct_growth_sp500
      )
    return(d)
  } else {
    return(NULL)
  }
}
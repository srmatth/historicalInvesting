library(ggplot2)
library(dplyr)

t <- get_historical_prices("T")


p <- t %>%
  mutate(
    ma = zoo::rollmean(
      close, 
      k = 1000, 
      na.pad = TRUE, 
      align = "right"
    )
  ) %>%
  ggplot() +
  aes(x = date, y = close) +
  geom_line() +
  theme_void() +
  theme(
    aspect.ratio = 0.1
  )

ggsave(
  "possible_bg.png",
  plot = p
)

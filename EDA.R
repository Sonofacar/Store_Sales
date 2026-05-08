source("clean_data.R")

library(ggplot2)

# Distribution of sales
ggplot(train) + geom_density(aes(x = sales), color = "blue", fill = "blue")

# Distribution of sales minus outliers and zeros
train |>
  (\(df) df[df$sales != 0, ])() |>
  (\(df) df[df$sales < quantile(df$sales, 0.90), ])() |>
  ggplot() + geom_density(aes(x = sales), color = "blue", fill = "blue")
# It feels like doing a logistic regression for zeros and log-transformed
# regression for everything else would be a decent way of doing things.

# Distribution of zeros
train |>
  (\(df) df$sales == 0)() |>
  table() |>
  as.data.frame() |>
  within({
    Proportion <- Freq / sum(Freq)
  })

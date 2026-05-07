raw_oil <- read.csv("oil.csv") |>
  within({
    date <- as.Date(date) |> as.numeric()
  }) |>
  (\(df) {
     data.frame(
       date = numeric(1704),
       dcoilwtico = NA
     ) |>
       within({
         date <- 15706:17409
         dcoilwtico[date %in% df$date] <- df$dcoilwtico
       })
  })() |>
  within({
    dcoilwtico[1] <- dcoilwtico[2]
    # Fill NAs with previous values
    while (is.na(dcoilwtico) |> any()) {
      dcoilwtico[is.na(dcoilwtico)] <- is.na(dcoilwtico) |>
        (\(.) c(.[-1], FALSE) )() |>
        (\(.) dcoilwtico[.])()
    }
  })
raw_holidays <- read.csv("holidays_events.csv") |>
  within({
    date <- as.Date(date) |> as.numeric()
    type <- as.factor(type)
    locale <- as.factor(locale)
    locale_name <- as.factor(locale_name)
    transferred <- transferred == "True"
  }) |>
  (\(df) df[!df$transferred, -6])() # Drop transferred holidays
raw_stores <- read.csv("stores.csv") |>
  within({
    store_nbr <- as.factor(store_nbr)
    city <- as.factor(city)
    state <- as.factor(state)
    type <- as.factor(type)
    cluster <- as.factor(cluster)
  })
# Transactions are difficult to use as they don't exist for the test data
raw_transactions <- read.csv("transactions.csv") |>
  within({
    date <- as.Date(date) |> as.numeric()
    store_nbr <- as.factor(store_nbr)
  })
raw_train <- read.csv("train.csv") |>
  within({
    date <- as.Date(date) |> as.numeric()
    family <- as.factor(family)
    store_nbr <- as.factor(store_nbr)
  })
raw_test <- read.csv("test.csv") |>
  within({
    date <- as.Date(date) |> as.numeric()
    family <- as.factor(family)
    store_nbr <- as.factor(store_nbr)
  })

clean_data <- function(df) {
  df |>
    within({
      days_after_pay <- as.Date(date) |>
        format("%d") |>
        as.numeric() |>
        (\(.) {
           ((. - 1) %% 15) |>
             (\(x) {
                x[. == 31] <- 15
                x
              })()
         })()
      month <- as.Date(date) |>
        format("%m") |>
        as.numeric() |>
        as.factor()
      quake_recovery_period <- length(id) |>
        numeric() |>
        (\(.) {
           .[date >= 16907 & date < 16963] <- rep(1:8, 7) |>
             sort(decreasing = TRUE)
           .
         })()
    }) |>
    merge(raw_oil, all.x = TRUE) |>
    merge(raw_stores, all.x = TRUE) |>
    within({
      store_type <- type
      rm(type)
      rm(state) # All info encoded in state must also be encoded in city
      rm(store_type, cluster)
    }) |>
    merge(
      raw_holidays[c(-3, -5)],
      all.x = TRUE,
      by.x = c("date", "city"),
      by.y = c("date", "locale_name")
    ) |>
    within({
      holiday_type <- type
      levels(holiday_type) <- levels(holiday_type) |>
        (\(.) c(., "None"))()
      holiday_type[is.na(holiday_type)] <- "None"
      rm(type)
    })
}

train <- raw_train |>
  clean_data() |>
  within(rm(id))
test <- raw_test |>
  clean_data()

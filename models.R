##############
# Clean data #
##############

source("clean_data.R")


##################################
# Logistic-log linear regression #
##################################

log_model <- train |>
  within(sales <- sales == 0) |>
  glm(sales ~ ., data = _, family = binomial())

lm(log(sales) ~ ., train[train$sales > 0, ]) |>
  predict(test) |>
  exp() |>
  (\(.) {
     data.frame(
       id = test$id,
       sales = .
     )
   })() |>
  (\(df) {
     df[predict(log_model, test) > (1/3), "sales"] <- 0
     df
   })() |>
  write.csv("log_regression.csv", quote = FALSE, row.names = FALSE)

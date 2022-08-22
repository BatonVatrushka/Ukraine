# predict initiator 
# install.packages("caret")
library(pacman)
p_load(tidyverse, caret, MLmetrics, acled.api)

# load in the data
# df <- acled.api(
#   email.address = Sys.getenv("ACLED_EMAIL")
#   , access.key = Sys.getenv("ACLED_API_KEY")
#   , country = "Ukraine"
#   , start.date = "2022-02-24"
#   , end.date = Sys.Date()
#   , all.variables = T
# )
#==================
# Data Cleaning
#==================
# read in the data
war <- read_csv(file = 'war.csv')
#war <- df

# actor1 will be the response variable
war |> group_by(actor1) |>
  tally() |>
  arrange(desc(n)) |> 
  print(n=35)

# create a new field where the actors are flattened
war <- war |> 
  mutate("initiator" = case_when(
    actor1 |> str_detect("Ukraine") ~ "U"
    , actor1 |> str_detect("Russia|NAF|Wagner") ~ "R"
    , T ~ "O"
  ))

# check a table of the new variable
war$initiator |> table()

# lets determine what fields can be removed
# take a glimpse of the data
war |> glimpse()

# select for the necessary fields
df <- war |> select(initiator, event_type, sub_event_type
                    , latitude, longitude, fatalities) |>
  filter(initiator != "O") |>
  # turn character fields to factors
  mutate_if(is.character, as.factor) 

# look at the new data set
df |> glimpse()

#==================
# Model Building
#==================
# define a cumstom trainControl
control <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# -------------------
# GLMNET
# -------------------
model <- train(
  initiator ~ . 
  , df
  , tuneGrid = expand.grid(
    alpha = 0:1,
    lambda = seq(0.0001, 1, length = 20)
  )
  , method = "glmnet"
  , trControl = control
)

# Print model to console
model

# Print maximum ROC statistic
max(model[["results"]][3])

# make predictions
pred <- predict(model, df |> select(-initiator))
# confusion matrix
glmnet_cm <- confusionMatrix(pred, df$initiator)
glmnet_cm[[3]][1]

# -------------------
# GLM (regular)
# -------------------
model_glm <- train(
  initiator ~ . 
  , df
  , method = "glm"
  , trControl = control
)

# make predictions
glm_predict <- predict(model_glm, df |> select(-initiator))
# confusion matrix
glm_cm <-confusionMatrix(glm_predict, df$initiator)
glm_cm[[3]][1]

# -------------------
# KNN
# -------------------
model_knn <- train(
  initiator ~ .
  , df
  , metric = "ROC"
  , method = "knn"
  , trControl = control
)

# Print model to console
model_knn

plot(model_knn)

# predict
knn_predict <- predict(model_knn, df |> select(-initiator))
# confusion matrix
knn_cm <- confusionMatrix(knn_predict, df$initiator)
knn_cm[[3]][1]

# KNN w/ cener and scale
model_knn_cs <- train(
  initiator ~ .
  , df
  , metric = "ROC"
  , method = "knn"
  , preProcess = c("center", "scale")
  , trControl = control
)

# predict
knn_predict_cs <- predict(model_knn_cs, df |> select(-initiator))
# confusion matrix
knn_scaled_cm <- confusionMatrix(knn_predict_cs, df$initiator)
knn_scaled_cm[[3]][1]

# -------------------
# RF
# -------------------
model_svm <- train(
  initiator ~ .
  , df
  , metric = "ROC"
  , method = "rf"
  , trControl = control
)

# predict
rf_predict <- predict(model_svm, df |> select(-initiator))
# confusion matrix
rf_cm <- confusionMatrix(rf_predict, df$initiator)
rf_cm[[3]][1]

# rda
model_rda <- train(
  initiator ~ .
  , df
  , metric = "ROC"
  , method = "rda"
  , trControl = control
)

# predict
rda_predict <- predict(model_svm, df |> select(-initiator))
# confusion matrix
rda_cm <- confusionMatrix(rda_predict, df$initiator)
rda_cm[[3]][1]

# Neural Network
model_nn <- train(
  initiator ~ .
  , df
  , metric = "ROC"
  , method = "nnet"
  , trControl = control
)

# predict
nn_predict <- predict(model_nn, df |> select(-initiator))
# confusion matrix
neu_cm <- confusionMatrix(nn_predict, df$initiator)
neu_cm[[3]][1]

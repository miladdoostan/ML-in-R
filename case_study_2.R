library(tidyverse)
library(xgboost)
library(Matrix)
library(ISLR)
source("train_test_split.R")

# importing the dataset and doing some cleansing
df <- Auto %>% as_tibble()
df  <- df %>% mutate(cylinders =  factor(cylinders, levels=c(3,4,5,6,8), ordered = T),
                     origin = as.factor(origin),
                     year = as.integer(year),
                     mpg01 = as.factor(ifelse(mpg >= median(mpg), 1, 0)))
df %>% head()

# creating training and testing data
df_train <- train_test_split(select(df, -name, -mpg), 0.8, seed=1)$training
df_test <-  train_test_split(select(df, -name, -mpg), 0.8, seed=1)$testing

# One-hot encoding for factor variables
trainm <- sparse.model.matrix(mpg01 ~ .-1, data=df_train)
testm <- sparse.model.matrix(mpg01 ~ .-1, data=df_test)

# defining the label
train_label <- df_train %>% pull(mpg01) %>% as.matrix()
test_label <- df_test %>% pull(mpg01) %>% as.matrix()

# creating the matrix forsm for xgb
matrix_train <- xgb.DMatrix(data = trainm, label = train_label)
matrix_test <- xgb.DMatrix(data = testm, label = test_label)

# defining the parameters of xgb model
xgb_params <- list("objective" = "multi:softprob", "eval_metric" = "mlogloss", "num_class"=2)
watchlist <- list(train = matrix_train, test = matrix_test)

# building xgb model
xgb_model <- xgb.train(params = xgb_params, 
                       data = matrix_train, 
                       nrounds = 25, 
                       watchlist = watchlist, 
                       eta=0.4, 
                       max.depth=4, 
                       gamma=0, 
                       subsample=1,
                       missing = NA,
                       seed = 1)

# evaluation of the training and testing error
xgb_model$evaluation_log %>% as_tibble() %>% 
  ggplot(aes(x=iter)) + 
  geom_line(aes(y=train_mlogloss), size=2, color="red") +
  geom_line(aes(y=test_mlogloss), size=2, color="navy")

# feature importance 
imp <- xgb.importance(colnames(matrix_train), model = xgb_model)
imp %>% as_tibble() %>% mutate(Feature = as.factor(Feature),
                               Feature = fct_reorder(Feature, Gain, max)) %>% 
  ggplot(aes(x=Feature, y=Gain)) + geom_col(fill = "gold", color="black") 

# prediction and confusion matrix
pred <- predict(xgb_model, matrix_test) %>% 
  matrix(nrow = 78, ncol = 2, byrow = T) %>% 
  as_tibble() %>% mutate(pred = as.factor(ifelse(V1 >= 0.5, 0, 1))) %>% pull(pred)

df_test %>% mutate(pred = pred) %>% select(mpg01, pred) %>% table()



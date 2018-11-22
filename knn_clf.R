# Fit a KNN classifier to a tibble using k-fold cross-validation and outputs the confusion matrices for all folds.
# predictors and response take char vectors.

k_nearest_neighbors <- function(df, predictors, response, k=5, folds=5, seed=NA){
    library(tidyverse)
    source("split_df.R")
    splited_df <- split_df(df, folds, seed)
    cm <- list()
    for (i in 1:folds){
        df_train <- splited_df[-i] %>% bind_rows() %>% select(predictors, response)
        df_test <- splited_df[[i]] %>% select(predictors, response)
        X_train <- df_train %>% select(predictors) %>% as.matrix()
        X_test <- df_test %>% select(predictors) %>% as.matrix()
        y_train <- df_train %>% pull(response) %>% as.factor()
        testing_cm <- df_test %>% mutate(pred = knn(X_train, X_test, y_train, k=k)) %>% 
        select(response, pred) %>% table()
        cm[[i]] <- testing_cm
    }
    return(cm)   
}

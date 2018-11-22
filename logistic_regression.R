# Fit a binary logistic regression to a tibble using k-fold cross validation and outputs the confusion matrices for all folds.
# The regularization is not included in this model.
# predictors and response take char vectors.

logistic_regression <- function(df, predictors, response, folds = 5, seed=NA){
    splited_df <- split_df(df, folds, seed)
    cm <- list()
    for (i in 1:folds){
        df_train <- splited_df[-i] %>% bind_rows() %>% select(predictors, response)
        df_test <- splited_df[[i]] %>% select(predictors, response)
        X_train <- select(df_train, predictors)
        y_train <- pull(df_train, response)
        model <- glm(y_train ~ ., data = X_train, family=binomial)
        testing_cm <- confusion_matrix(df_test, model, response)
        cm[[i]] <- testing_cm
    }
    return(cm)   
}

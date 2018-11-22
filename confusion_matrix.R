# A simple function that calculates the confusion matrix for a logistic regression glm model

confusion_matrix <- function(df, model, response){
    library(tidyverse)
    df %>% 
    mutate(probs = predict(model, df, type="response"),
           pred = as.factor(ifelse(probs >= 0.5, 1, 0))) %>% 
    select(response, pred) %>% 
    table()
}

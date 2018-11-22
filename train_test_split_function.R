# A simple function for splitting a tibble to training and testing sets based on a fraction

train_test_split <- function(df, training_frac){
    library(tidyverse)
    training <- df %>% mutate(id = row_number()) %>% sample_frac(training_frac)
    testing <- df %>% mutate(id = row_number()) %>% anti_join(training, by="id")
    return(list(training = training, testing = testing) %>% map(~select(.,-id)))
}

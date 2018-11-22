# A simple function for splitting a tibble into K tibbles
split_df <- function(df, folds, seed){
library(tidyverse)
    if (!is.na(seed)){set.seed(seed)}
    splited_df <- df %>% 
    sample_frac(1) %>% 
    mutate(cl = cut(row_number(), folds, labels = 1:folds)) %>% 
    split(.$cl)
    return(splited_df)
}

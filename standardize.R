# A function that standardize (a.k.a. normaize) the numeric variables of a tibble and outputs the scaled tibble.

standardize_df <- function(df, vars){
  library(tidyverse)
  df %>% mutate_each_(funs(scale),vars=vars)
}


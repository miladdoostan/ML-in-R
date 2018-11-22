library(tidyverse)
library(ISLR)
df <- Auto %>% as_tibble()
df  <- df %>% mutate(cylinders =  factor(cylinders, levels=c(3,4,5,6,8), ordered = T),
                     origin = as.factor(origin),
                     year = as.integer(year),
                     mpg01 = as.factor(ifelse(mpg >= median(mpg), 1, 0)))
head(df)

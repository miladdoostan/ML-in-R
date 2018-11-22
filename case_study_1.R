# In this cse study, I will use the Auto dataframe from ISLR. I will create a binary variable from mpg. I then apply two
# classifiers, namely logistic_regression and k_nearest_neighbors ( both available in repo) to the data. Then, I 
# calculate the average classification accuracy for both models. The predictios could change to see which combinations deliver
# the best result.


# importing necessary libraries and functions
library(tidyverse)
library(ISLR)
source("logistic_regression.R")
source("knn_clf.R")

# importing and tidying the Auto dataframe -  creating a binary variable as response ("mpg01")
df <- Auto %>% as_tibble()
df  <- df %>% mutate(cylinders =  factor(cylinders, levels=c(3,4,5,6,8), ordered = T),
                     origin = as.factor(origin),
                     year = as.integer(year),
                     mpg01 = as.factor(ifelse(mpg >= median(mpg), 1, 0)))
print(head(df))

# defining the predictors and response variables for binary classification
predictors <-c("horsepower", "origin", "weight", 'cylinders', 'year', 'displacement', "acceleration")
response <- c("mpg01")

# building LR nad KNN classifiers
LR_cm <- logistic_regression(df, predictors, response, folds = 5, seed=1)
knn_cm <- k_nearest_neighbors(df, predictors, response, k=5, folds=5, seed=1)

# calculating the average accuracy of cross-validation sets
LR_acc <- map_dbl(LR_cm, function(x){sum(diag(x))/sum(x)})
knn_acc <- map_dbl(knn_cm, function(x){sum(diag(x))/sum(x)})

# printing the results
print(paste("Logistic regression average accuracy: ", mean(LR_acc)))
print(paste("KNN average accuracy: ", mean(knn_acc)))


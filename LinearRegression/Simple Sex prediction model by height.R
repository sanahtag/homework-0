###############################################################################
# Simple Sex prediction model by height                                       #
###############################################################################

library(tidyverse)
library(caret)
library(dslabs)

data("heights")

# Create the train_set and test_set data.
y <- heights$sex
x <- heights$height
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
set.seed(2)
train_set <- heights[-test_index,]
test_set <- heights[test_index,]

#Create a first simple model by sample function
y_hat <- sample(c("Male","Female"), length(test_index), replace = T) %>% 
  factor(levels = levels(test_set$sex))

#Calculate the accuracy as below code
mean(y_hat == test_set$sex)
## [1] 0.5009524

#The accuracy of our model as 0.52(52%) is low. But we can do better. Let’s 
#look at our data again.
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))


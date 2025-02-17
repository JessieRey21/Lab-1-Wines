install.packages("tidyverse")
install.packages("class")
install.packages("gmodels")
library(tidyverse)
library(class)
library(gmodels)


### Part 0 - Background
wines <- read.csv("wine.csv")
wines <- wines %>%
  select(-X)

glimpse(wines)

wines %>% count(type)
wines$type <- factor(wines$type, levels = c("Red", "White"), labels = c("Red", "White"))
glimpse(wines$type)

## Part 1 - EDA
summary(wines)

hist(wines$fixed.acidity)
hist(wines$volatile.acidity)
hist(wines$citric.acid)
hist(wines$residual.sugar)
hist(wines$chlorides)
hist(wines$free.sulfur.dioxide)
hist(wines$total.sulfur.dioxide)
hist(wines$density)
hist(wines$pH)
hist(wines$sulphates)
hist(wines$alcohol)
hist(wines$quality)

boxplot(wines$fixed.acidity)
boxplot(wines$volatile.acidity)
boxplot(wines$citric.acid)
boxplot(wines$residual.sugar)
boxplot(wines$chlorides)
boxplot(wines$free.sulfur.dioxide)
boxplot(wines$total.sulfur.dioxide)
boxplot(wines$density)
boxplot(wines$pH)
boxplot(wines$sulphates)
boxplot(wines$alcohol)
boxplot(wines$quality)

summary(wines$type)
  ## Red - 1599
  ## White - 4898


### Part 2 - Data Preparation
## Create normalize function
normalize <- function(x) {
  return ((x-min(x)) / (max(x)-min(x)))}

wines_n <- as.data.frame(lapply(wines[1:12], normalize))

## Use z-scores
z <- function(x) {
  return ((x-mean(x)) / (sd(x)))}
  ## can also use scale()

wines_z <- as.data.frame(lapply(wines[1:12], z))

### Part 3 - Training and Test Datasets
set.seed(4836)

train.size <- floor(0.8 * 6497)
train.size
  ## 5197

train_sample <- sample(6497, train.size)

wines_n_train <- wines_n[train_sample,]
wines_n_test <- wines_n[-train_sample,]

wines_z_train <- wines_z[train_sample,]
wines_z_test <- wines_z[-train_sample,]

wines_train_labels <- wines[train_sample,13]
wines_test_labels <- wines[-train_sample,13]
glimpse(wines_train_labels)
glimpse(wines_test_labels)



### Part 4 - Using kNN Algorithm
sqrt(5197)
## Using wines_n
wines_n_pred <- knn(train = wines_n_train, test = wines_n_test, 
                    cl = wines_train_labels, k = 72)

CrossTable(x = wines_test_labels, y = wines_n_pred, prop.chisq = FALSE)


## Using wines_z
wines_z_pred <- knn(train = wines_z_train, test = wines_z_test, 
                    cl = wines_train_labels, k = 72)

CrossTable(x = wines_test_labels, y = wines_z_pred, prop.chisq = FALSE)



### Part 5 - Improve the model
## Using different k for wines_n
  ## k = 50
wines_n1_pred <- knn(train = wines_n_train, test = wines_n_test, 
                    cl = wines_train_labels, k = 50)

CrossTable(x = wines_test_labels, y = wines_n1_pred, prop.chisq = FALSE)


  ## k = 25
wines_n2_pred <- knn(train = wines_n_train, test = wines_n_test, 
                     cl = wines_train_labels, k = 25)

CrossTable(x = wines_test_labels, y = wines_n2_pred, prop.chisq = FALSE)

  ## k = 10
wines_n3_pred <- knn(train = wines_n_train, test = wines_n_test, 
                     cl = wines_train_labels, k = 10)

CrossTable(x = wines_test_labels, y = wines_n3_pred, prop.chisq = FALSE)



## Using different k for wines_z
  ## k = 50
wines_z1_pred <- knn(train = wines_z_train, test = wines_z_test, 
                     cl = wines_train_labels, k = 50)

CrossTable(x = wines_test_labels, y = wines_z1_pred, prop.chisq = FALSE)

  ## k = 25
wines_z2_pred <- knn(train = wines_z_train, test = wines_z_test, 
                     cl = wines_train_labels, k = 25)

CrossTable(x = wines_test_labels, y = wines_z2_pred, prop.chisq = FALSE)


## k = 10
wines_z3_pred <- knn(train = wines_z_train, test = wines_z_test, 
                     cl = wines_train_labels, k = 10)

CrossTable(x = wines_test_labels, y = wines_z3_pred, prop.chisq = FALSE)


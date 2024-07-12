install.packages("tidyverse")
install.packages("dplyr")
install.packages("caTools")
install.packages("ROCR")
install.packages("rpart.plot")
library(caTools)
library(ROCR)
library(dplyr)
library(tidyverse)
library(rpart)
library(rpart.plot)

setwd("C:/Users/micky/OneDrive/Desktop/DataScience/Miniproject2")
df_raw <- read.delim("adult.DATA", sep=",", header=FALSE)
colnames(df_raw) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_worked", "native_country", "income")
view(df_raw)
df <- na.omit(df_raw)
df_new <- df
df <- df %>% mutate(across(c('workclass'), substr, 2, nchar(workclass)))
df <- df %>% mutate(across(c('education'), substr, 2, nchar(education)))
df <- df %>% mutate(across(c('marital_status'), substr, 2, nchar(marital_status)))
df <- df %>% mutate(across(c('occupation'), substr, 2, nchar(occupation)))
df <- df %>% mutate(across(c('relationship'), substr, 2, nchar(relationship)))
df <- df %>% mutate(across(c('race'), substr, 2, nchar(race)))
df <- df %>% mutate(across(c('sex'), substr, 2, nchar(sex)))
df <- df %>% mutate(across(c('native_country'), substr, 2, nchar(native_country)))
df <- df %>% mutate(across(c('income'), substr, 2, nchar(income)))

`%notin%` <- Negate(`%in%`)
df <- df %>%
  as_tibble %>%
  filter(if_all(age:income, ~.x %notin% c("?")))

df$incomeGreater50K <- ifelse(df$income == ">50K", 1, 0)
df$incomeGreater50K <- as.integer(df$incomeGreater50K)
df <- df[,-15]

view(df)

# logistic regression
#example, >50K is 1
df_regression <- as.data.frame(df)
view(df_regression)

set.seed(100)

split <- sample.split(df_regression, SplitRatio = 0.8)
split
df_regression$incomeGreater50K <- as.numeric(df_regression$incomeGreater50K)

train_reg <- subset(df_regression, split == "TRUE")
test_reg <- subset(df_regression, split == "FALSE")

# Training model
logistic_model <- glm(incomeGreater50K ~ age + workclass + education + marital_status + occupation + relationship + race + sex + capital_gain + capital_loss + hours_worked + native_country, #formula e.g. flower type ~ sepal length + sepal width + petal length + petal width
                      data = train_reg,
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)

# Make predictions on the dataset
predictions <- predict(logistic_model, test_reg, type = "response")

predict_reg <- predict(logistic_model,
                       test_reg, type = "response")
predict_reg

predict_reg <- ifelse(predict_reg > 0.5, 1, 0)
table(test_reg$incomeGreater50K, predict_reg)
missing_classerr <- mean(predict_reg != test_reg$incomeGreater50K)
print(paste('Accuracy =', 1 - missing_classerr))

ROCPred <- prediction(predict_reg, test_reg$incomeGreater50K)
ROCPer <- performance(ROCPred, measure = "tpr",
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)





#neural nets shallow
df_neuralnet <- df
df_neuralnet <-as.data.frame(df_neuralnet)
df_neuralnet$workclass <- as.numeric(as.factor(df_neuralnet$workclass))
df_neuralnet$marital_status <- as.numeric(as.factor(df_neuralnet$marital_status))
df_neuralnet$occupation <- as.numeric(as.factor(df_neuralnet$occupation))
df_neuralnet$relationship <- as.numeric(as.factor(df_neuralnet$relationship))
df_neuralnet$sex <- as.numeric(as.factor(df_neuralnet$sex))
df_neuralnet$native_country <- as.numeric(as.factor(df_neuralnet$native_country))

df_neuralnet <- df_neuralnet[, unlist(lapply(df_neuralnet, is.numeric))]

df_neuralnet <- df_neuralnet[sample(nrow(df_neuralnet)), ]
view(df_neuralnet)

train_test_split_index <- 0.8 * nrow(df_neuralnet)
train <- df_neuralnet[1:train_test_split_index,]
train

test <- df_neuralnet[(train_test_split_index+1): (nrow(df_neuralnet)),]
test

X_train <- scale(train[, c(1:2)])
y_train <- train$incomeGreater50K
dim(y_train) <- c(length(y_train), 1) # add extra dimension to vector

X_test <- scale(test[, c(1:2)])

y_test <- test$incomeGreater50K
dim(y_test) <- c(length(y_test), 1)

X_train <- as.matrix(X_train, byrow=TRUE)
X_train <- t(X_train)
y_train <- as.matrix(y_train, byrow=TRUE)
y_train <- t(y_train)

X_test <- as.matrix(X_test, byrow=TRUE)
X_test <- t(X_test)
y_test <- as.matrix(y_test, byrow=TRUE)
y_test <- t(y_test)

getLayerSize <- function(X, y, hidden_neurons, train=TRUE) {
  n_x <- dim(X)[1]
  n_h <- hidden_neurons
  n_y <- dim(y)[1]   
  
  size <- list("n_x" = n_x,
               "n_h" = n_h,
               "n_y" = n_y)
  
  return(size)
}
#n_x size of input
#n_h size of hidden
#n_y size of output
layer_size <- getLayerSize(X_train, y_train, hidden_neurons = 4)
layer_size

initializeParameters <- function(X, list_layer_size){
  
  m <- dim(data.matrix(X))[2]
  
  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  
  W1 <- matrix(runif(n_h * n_x), nrow = n_h, ncol = n_x, byrow = TRUE) * 0.01
  b1 <- matrix(rep(0, n_h), nrow = n_h)
  W2 <- matrix(runif(n_y * n_h), nrow = n_y, ncol = n_h, byrow = TRUE) * 0.01
  b2 <- matrix(rep(0, n_y), nrow = n_y)
  
  params <- list("W1" = W1,
                 "b1" = b1, 
                 "W2" = W2,
                 "b2" = b2)
  
  return (params)
}
init_params <- initializeParameters(X_train, layer_size)
lapply(init_params, function(x) dim(x))
sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}
forwardPropagation <- function(X, params, list_layer_size){
  
  m <- dim(X)[2]
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  
  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2
  
  b1_new <- matrix(rep(b1, m), nrow = n_h)
  b2_new <- matrix(rep(b2, m), nrow = n_y)
  
  Z1 <- W1 %*% X + b1_new
  A1 <- sigmoid(Z1)
  Z2 <- W2 %*% A1 + b2_new
  A2 <- sigmoid(Z2)
  
  cache <- list("Z1" = Z1,
                "A1" = A1, 
                "Z2" = Z2,
                "A2" = A2)
  
  return (cache)
}
fwd_prop <- forwardPropagation(X_train, init_params, layer_size)
lapply(fwd_prop, function(x) dim(x))

computeCost <- function(X, y, cache) {
  
  m <- dim(X)[2]
  
  A2 <- cache$A2
  
  logprobs <- (log(A2) * y) + (log(1-A2) * (1-y))
  cost <- -sum(logprobs/m)
  
  return (cost)
}
cost <- computeCost(X_train, y_train, fwd_prop)
cost
backwardPropagation <- function(X, y, cache, params, list_layer_size){
  
  m <- dim(X)[2]
  
  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  
  A2 <- cache$A2
  A1 <- cache$A1
  W2 <- params$W2
  
  
  dZ2 <- A2 - y
  dW2 <- 1/m * (dZ2 %*% t(A1)) 
  db2 <- matrix(1/m * sum(dZ2), nrow = n_y)
  db2_new <- matrix(rep(db2, m), nrow = n_y)
  
  dZ1 <- (t(W2) %*% dZ2) * (1 - A1^2)
  dW1 <- 1/m * (dZ1 %*% t(X))
  db1 <- matrix(1/m * sum(dZ1), nrow = n_h)
  db1_new <- matrix(rep(db1, m), nrow = n_h)
  
  grads <- list("dW1" = dW1, 
                "db1" = db1,
                "dW2" = dW2,
                "db2" = db2)
  
  return(grads)
}
back_prop <- backwardPropagation(X_train, y_train, fwd_prop, init_params, layer_size)
lapply(back_prop, function(x) dim(x))

updateParameters <- function(grads, params, learning_rate){
  
  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2
  
  dW1 <- grads$dW1
  db1 <- grads$db1
  dW2 <- grads$dW2
  db2 <- grads$db2
  
  
  W1 <- W1 - learning_rate * dW1
  b1 <- b1 - learning_rate * db1
  W2 <- W2 - learning_rate * dW2
  b2 <- b2 - learning_rate * db2
  
  updated_params <- list("W1" = W1,
                         "b1" = b1,
                         "W2" = W2,
                         "b2" = b2)
  
  return (updated_params)
}
update_params <- updateParameters(back_prop, init_params, learning_rate = 0.01)
lapply(update_params, function(x) dim(x))

trainModel <- function(X, y, num_iteration, hidden_neurons, lr){
  
  layer_size <- getLayerSize(X, y, hidden_neurons)
  init_params <- initializeParameters(X, layer_size)
  
  cost_history <- c()
  
  for (i in 1:num_iteration) {
    fwd_prop <- forwardPropagation(X, init_params, layer_size)
    cost <- computeCost(X, y, fwd_prop)
    back_prop <- backwardPropagation(X, y, fwd_prop, init_params, layer_size)
    update_params <- updateParameters(back_prop, init_params, learning_rate = lr)
    init_params <- update_params
    
    cost_history <- c(cost_history, cost)
    
    if (i %% 10000 == 0) cat("Iteration", i, " | Cost: ", cost, "\n")
  }
  
  model_out <- list("updated_params" = update_params,
                    "cost_hist" = cost_history)
  
  return (model_out)
}

EPOCHS = 60000
HIDDEN_NEURONS = 40
LEARNING_RATE = 0.9

train_model <- trainModel(X_train, y_train, hidden_neurons = HIDDEN_NEURONS, num_iteration = EPOCHS, lr = LEARNING_RATE)





# decision trees
set.seed(51)
df_tree <- df
df_tree <-as.data.frame(df_tree)

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample < - 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
rows <- sample(nrow(df_tree))
df_tree <- df_tree[rows, ]
train_test_split_index <- 0.8 * nrow(df_tree)
data_train <- df_tree[1:train_test_split_index,]

data_test <- df_tree[(train_test_split_index+1): nrow(df_neuralnet),]

fit <- rpart(incomeGreater50K~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, data_test, type = 'class')

table_mat <- table(data_test$incomeGreater50K, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

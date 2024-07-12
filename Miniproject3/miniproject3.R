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

setwd("C:/Users/User/Downlaods/Miniproject3")
df_raw <- read.delim("adult.DATA", sep=",", header=FALSE)
colnames(df_raw) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_worked", "native_country", "income")
view(df_raw)
df <- na.omit(df_raw)
df <- df %>% mutate(across(c('workclass'), substr, 2, nchar(workclass)))
df <- df %>% mutate(across(c('education'), substr, 2, nchar(education)))
df <- df %>% mutate(across(c('marital_status'), substr, 2, nchar(marital_status)))
df <- df %>% mutate(across(c('occupation'), substr, 2, nchar(occupation)))
df <- df %>% mutate(across(c('relationship'), substr, 2, nchar(relationship)))
df <- df %>% mutate(across(c('race'), substr, 2, nchar(race)))
df <- df %>% mutate(across(c('sex'), substr, 2, nchar(sex)))
df <- df %>% mutate(across(c('native_country'), substr, 2, nchar(native_country)))
df <- df %>% mutate(across(c('income'), substr, 2, nchar(income)))

df$workclass <- as.numeric(as.factor(df$workclass))

df$marital_status <- as.numeric(as.factor(df$marital_status))

df$occupation <- as.numeric(as.factor(df$occupation))

df$relationship <- as.numeric(as.factor(df$relationship))

df$race <- as.numeric(as.factor(df$race))

#df$sex <- as.numeric(as.factor(df$sex))

df$native_country <- as.numeric(as.factor(df$native_country))

df$incomeGreater50K <- ifelse(df$income == ">50K", 1, 0)
df$incomeGreater50K <- as.integer(df$incomeGreater50K)

df$sex <- ifelse(df$sex == "Male", 1, 0)
df$sex <- as.integer(df$sex)

#df <- as.data.frame(lapply(df, rep, df$fnlwgt))

#this is the line that was supposed to fix the dataset's bias

df <- subset(df, select = -c(income, education, fnlwgt) )
view(df)

#normalizing the data
#Encode categorical data to be numeric

#Subtract the mean and divide by standard deviation
#subtract mean
#divide by sd
#df <- df %>% mutate(across(c(age, workclass, education_num, marital_status, occupation, relationship, race, sex, capital_gain, capital_loss, hours_worked, native_country, incomeGreater50K), ~ (.x - mean(.x)) / sd(.x)))
#view(df) 

#set null to zero
#str(df)
#df[is.na(df)] <- 0

#The code above, making it deal with the mean and standard deviation, makes my neural net break

#make decision tree
# decision trees

df_tree <- df
df_tree <-as.data.frame(df_tree)

create_train_test <- function(data, size = .8, train = TRUE) {
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
data_train_f <- df_tree[1:train_test_split_index,] %>% filter(sex == 0)
data_train_m <- df_tree[1:train_test_split_index,] %>% filter(sex == 1)

data_test <- df_tree[(train_test_split_index+1): nrow(df_neuralnet),]
data_test_m <- df_tree[(train_test_split_index+1): nrow(df_neuralnet),]
data_test_f <- df_tree[(train_test_split_index+1): nrow(df_neuralnet),]

fit <- rpart(incomeGreater50K~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, data_test, type = 'class')
predict_unseen_f <-predict(fit, data_test_f, type ='class')
predict_unseen_m <-predict(fit, data_test_m, type ='class')

table_mat <- table(data_test$incomeGreater50K, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

table_matm <- table(data_test_m$incomeGreater50K, predict_unseen_m)
table_matm
accuracy_mTest <- sum(diag(table_matm)) / sum(table_matm)

table_matf <- table(data_test_f$incomeGreater50K, predict_unseen_f)
table_matf
accuracy_fTest <- sum(diag(table_matf)) / sum(table_matf)

print(paste('Accuracy for test', accuracy_Test))
print(paste('Accuracy for test (male)', accuracy_mTest))
print(paste('Accuracy for test (female)', accuracy_fTest))

#make neural net


df_neuralnet <- df
df_neuralnet <-as.data.frame(df_neuralnet)

#df_neuralnet$sex <- ifelse(df$sex > 0, 1, 0)
#df_neuralnet$sex <- as.integer(df$sex)

#this code is only needed when we divide by st dev and subtract mean

df_neuralnet <- df_neuralnet[, unlist(lapply(df_neuralnet, is.numeric))]

df_neuralnet <- df_neuralnet[sample(nrow(df_neuralnet)), ]
view(df_neuralnet)

train_test_split_index <- 0.8 * nrow(df_neuralnet)
train <- df_neuralnet[1:train_test_split_index,]
train

test <- df_neuralnet[(train_test_split_index+1): (nrow(df_neuralnet)),]
testm <- test %>% filter(sex == 1)
testf <- test %>% filter(sex == 0)

X_train <- scale(train[, c(1:2)])
y_train <- train$incomeGreater50K
dim(y_train) <- c(length(y_train), 1) # add extra dimension to vector

X_test <- scale(testm[, c(1:2)])
y_test <- testm$incomeGreater50K
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
    
    if (i %% 10 == 0) cat("Iteration", i, " | Cost: ", cost, "\n")
  }
  
  model_out <- list("updated_params" = update_params,
                    "cost_hist" = cost_history)
  
  return (model_out)
}

EPOCHS = 1000
HIDDEN_NEURONS = 40
LEARNING_RATE = 0.9

##male
train_modelm <- trainModel(X_train, y_train, hidden_neurons = HIDDEN_NEURONS, num_iteration = EPOCHS, lr = LEARNING_RATE)



X_test <- scale(testf[, c(1:2)])
y_test <- testf$incomeGreater50K
dim(y_test) <- c(length(y_test), 1)

X_train <- as.matrix(X_train, byrow=TRUE)
X_train <- t(X_train)
y_train <- as.matrix(y_train, byrow=TRUE)
y_train <- t(y_train)

X_test <- as.matrix(X_test, byrow=TRUE)
X_test <- t(X_test)
y_test <- as.matrix(y_test, byrow=TRUE)
y_test <- t(y_test)

layer_size <- getLayerSize(X_train, y_train, hidden_neurons = 4)
layer_size

init_params <- initializeParameters(X_train, layer_size)
lapply(init_params, function(x) dim(x))
fwd_prop <- forwardPropagation(X_train, init_params, layer_size)
lapply(fwd_prop, function(x) dim(x))
costf <- computeCost(X_train, y_train, fwd_prop)
costf
back_prop <- backwardPropagation(X_train, y_train, fwd_prop, init_params, layer_size)
lapply(back_prop, function(x) dim(x))
update_params <- updateParameters(back_prop, init_params, learning_rate = 0.01)
lapply(update_params, function(x) dim(x))

#female
train_modelf <- trainModel(X_train, y_train, hidden_neurons = HIDDEN_NEURONS, num_iteration = EPOCHS, lr = LEARNING_RATE)



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

layer_size <- getLayerSize(X_train, y_train, hidden_neurons = 4)
layer_size

init_params <- initializeParameters(X_train, layer_size)
lapply(init_params, function(x) dim(x))
fwd_prop <- forwardPropagation(X_train, init_params, layer_size)
lapply(fwd_prop, function(x) dim(x))
costf <- computeCost(X_train, y_train, fwd_prop)
costf
back_prop <- backwardPropagation(X_train, y_train, fwd_prop, init_params, layer_size)
lapply(back_prop, function(x) dim(x))
update_params <- updateParameters(back_prop, init_params, learning_rate = 0.01)
lapply(update_params, function(x) dim(x))

#all data
train_model <- trainModel(X_train, y_train, hidden_neurons = HIDDEN_NEURONS, num_iteration = EPOCHS, lr = LEARNING_RATE)

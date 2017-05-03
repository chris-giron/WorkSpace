### Code for CSE-6242 (Data and Visual Analytics) Homework 3 ###
# dgiron3
# =============================== #
# Problem 0 #
library(aod)
library(ggplot2)
library(reshape2)
library(stats)

problem0 <- function(){
  train <- read.csv('mnist_train.csv', header = FALSE)
  test <- read.csv('mnist_test.csv', header = FALSE)
  train = as.data.frame(t(train))
  test = as.data.frame(t(test))
  train_0_1_ind <- (train[,dim(train)[2]] < 2)
  train_3_5_ind <- (train[,dim(train)[2]] > 2)
  test_0_1_ind <- (test[,dim(test)[2]] < 2)
  test_3_5_ind <- (test[,dim(test)[2]] > 2)
  train_0_1 = train[train_0_1_ind,]
  train_3_5 = train[train_3_5_ind,]
  test_0_1 = test[test_0_1_ind,]
  test_3_5 = test[test_3_5_ind,]
  true_label_train_0_1 = train_0_1[,dim(train_0_1)[2]]
  true_label_train_3_5 = train_3_5[,dim(train_3_5)[2]]
  true_label_test_0_1 = test_0_1[,dim(test_0_1)[2]]
  true_label_test_3_5 = test_3_5[,dim(test_3_5)[2]]
  train_0_1 = train_0_1[,-dim(train_0_1)[2]]
  train_3_5 = train_3_5[,-dim(train_3_5)[2]]
  test_0_1 = test_0_1[,-dim(test_0_1)[2]]
  test_3_5 = test_3_5[,-dim(test_3_5)[2]]
  return(list(train_0_1=train_0_1,train_3_5=train_3_5,test_0_1=test_0_1,test_3_5=test_3_5,
         true_label_train_0_1=true_label_train_0_1,true_label_train_3_5=true_label_train_3_5,
         true_label_test_0_1=true_label_test_0_1,true_label_test_3_5=true_label_test_3_5))
}
# list2env(problem0(),globalenv())

# Create 4 Images (One for each class)
problem0Images <- function(){
  zero_ind = which(true_label_train_0_1==0)[1]
  one_ind = which(true_label_train_0_1==1)[1]
  three_ind = which(true_label_train_3_5==3)[1]
  five_ind = which(true_label_train_3_5==5)[1]
  zero = as.numeric(train_0_1[zero_ind,])
  one = as.numeric(train_0_1[one_ind,])
  three = as.numeric(train_3_5[three_ind,])
  five = as.numeric(train_3_5[five_ind,])
  dim(zero) = c(28,28)
  dim(one) = c(28,28)
  dim(three) = c(28,28)
  dim(five) = c(28,28)
  return(list(zero=zero,one=one,three=three,five=five))
}
# list2env(problem0Images(),globalenv())

# image(zero,col = gray.colors(256), main = paste("Class label =",true_label_train_0_1[which(true_label_train_0_1==0)[1]]))
# image(one,col = gray.colors(256), main = paste("Class label =",true_label_train_0_1[which(true_label_train_0_1==1)[1]]))
# image(three,col = gray.colors(256), main = paste("Class label =",true_label_train_3_5[which(true_label_train_3_5==3)[1]]))
# image(five,col = gray.colors(256), main = paste("Class label =",true_label_train_3_5[which(true_label_train_3_5==5)[1]]))

# =============================== #
# Problem 1 #

# =============================== #
# Problem 2 #
sigmoid <- function(x, theta){
  sigmoid = 1 / (1 + exp(-x %*% theta))
  return(sigmoid)
}

sigmoid.t <- function(x, theta){
  sigmoid = 1 / (1 + exp(-x %*% t(theta)))
  return(sigmoid)
}

gradient.descent <- function(x, y, theta, alpha=0.1){
  grad = (1 / nrow(y)) * (t(x) %*% (sigmoid.t(x, theta) - y))
  tgrad = t(grad)
  theta = theta - alpha * tgrad
  return(theta)
}

logisticRegression <- function(x, y, alpha=0.1, num.Iterations=100, convergence=1e-4){
  # Add column of ones to begining of x
  x = cbind(rep(1,nrow(x)), x)
  x = apply(x, 2, as.numeric)
  y = matrix(y)
  
  # Initialize theta
  rand.num = sample(randu[,sample(3)[1]])[1]
  theta = matrix(rep(0, ncol(x)), nrow = 1)
  theta.hist = theta
  start.time = Sys.time()
  
  # Perfrom logistic regression with gradient descent
  for(iter in 1:num.Iterations){
    theta = gradient.descent(x, y, theta, alpha)
    theta.hist = rbind(theta.hist, theta)
    if(iter > 2){
      if(all(abs(theta - theta.hist[iter - 1,]) < convergence)) break
    }
  }
  print(paste(iter, as.numeric(Sys.time() - start.time)))
  return(theta.hist[nrow(theta.hist),])
}

# =============================== #
# Problem 3 #

predict_prob = function(theta, test){
  x = test[,]
  # Add column of ones to beginning of x
  x = cbind(rep(1, nrow(x)), x)
  x.matrix = as.matrix(x)
  prob = sigmoid(x.matrix, theta)
  return(prob)
}

predict_val = function(predict_prob){
  return(as.numeric(predict_prob > 0.5))
}

accuracy = function(predict, labels){
  pred.v.actual = cbind(predict, as.matrix(labels))
  accuracy = (pred.v.actual[,1] == pred.v.actual[,2])
  return(mean(accuracy))
}

problem3 = function(){
  binary35_label_train = as.numeric(true_label_train_3_5 > 4)
  binary35_label_test = as.numeric(true_label_test_3_5 > 4)
  acc.01.train_hist = rep(0,times=0)
  acc.01.test_hist = rep(0,times=0)
  acc.35.train_hist = rep(0,times=0)
  acc.35.test_hist = rep(0,times=0)
  
  for(i in seq(1:10)){
    theta01 = logisticRegression(train_0_1, true_label_train_0_1)
    theta35 = logisticRegression(train_3_5, binary35_label_train)
    pred01_prob_train = predict_prob(theta01, train_0_1)
    pred01_prob_test = predict_prob(theta01, test_0_1)
    pred35_prob_train = predict_prob(theta35, train_3_5)
    pred35_prob_test = predict_prob(theta35, test_3_5)
    predict01_train = predict_val(pred01_prob_train)
    predict01_test = predict_val(pred01_prob_test)
    predict35_train = predict_val(pred35_prob_train)
    predict35_test = predict_val(pred35_prob_test)
    acc.01.train = accuracy(predict01_train, true_label_train_0_1)
    acc.01.test = accuracy(predict01_test, true_label_test_0_1)
    acc.35.train = accuracy(predict35_train, binary35_label_train)
    acc.35.test = accuracy(predict35_test, binary35_label_test)
    acc.01.train_hist[i] = acc.01.train
    acc.01.test_hist[i] = acc.01.test
    acc.35.train_hist[i] = acc.35.train
    acc.35.test_hist[i] = acc.35.test
    print(paste(i,"--- acc.01.train =",acc.01.train,"--- acc.01.test =",acc.01.test,
                "--- acc.35.train =",acc.35.train,"--- acc.35.test =",acc.35.test))
  }
  print(paste("AVG.01.TRAIN =",mean(acc.01.train_hist),"--- AVG.01.TEST =",mean(acc.01.test_hist),
              "--- AVG.35.TRAIN =",mean(acc.35.train_hist),"--- AVG.35.TEST =",mean(acc.35.test_hist)))
}

# problem3()

# =============================== #
# Problem 4 #

problem4 = function(alpha = 0.1, num.Iterations = 100, convergence = 1e-4){
  binary35_label_train = as.numeric(true_label_train_3_5 > 4)
  binary35_label_test = as.numeric(true_label_test_3_5 > 4)
  acc.35.train_hist = rep(0,times=0)
  acc.35.test_hist = rep(0,times=0)
  
  for(i in seq(1:10)){
    theta35 = logisticRegression(train_3_5, binary35_label_train, alpha, num.Iterations, convergence)
    pred35_prob_train = predict_prob(theta35, train_3_5)
    pred35_prob_test = predict_prob(theta35, test_3_5)
    predict35_train = predict_val(pred35_prob_train)
    predict35_test = predict_val(pred35_prob_test)
    acc.35.train = accuracy(predict35_train, binary35_label_train)
    acc.35.test = accuracy(predict35_test, binary35_label_test)
    acc.35.train_hist[i] = acc.35.train
    acc.35.test_hist[i] = acc.35.test
    # print(paste(i,"--- acc.35.train =",acc.35.train,"--- acc.35.test =",acc.35.test))
  }
  print(paste("AVG.35.TRAIN =",mean(acc.35.train_hist),"--- AVG.35.TEST =",mean(acc.35.test_hist)))
}

# problem4()
# problem4(alpha = 0.01)
# problem4(alpha = 0.3)
# problem4(num.Iterations = 500)
# problem4(num.Iterations = 1000)
# problem4(num.Iterations = 500, convergence = 1e-3)
# problem4(num.Iterations = 500, convergence = 1e-2)

# =============================== #
# Problem 5 #

problem5 = function(perc){
  binary35_label_train = as.numeric(true_label_train_3_5 > 4)
  binary35_label_test = as.numeric(true_label_test_3_5 > 4)
  acc.01.train_hist = rep(0,times=0)
  acc.01.test_hist = rep(0,times=0)
  acc.35.train_hist = rep(0,times=0)
  acc.35.test_hist = rep(0,times=0)
  nrows01 = dim(train_0_1)[1]
  nrows35 = dim(train_3_5)[1]
  train01 = train_0_1[1:round(perc*nrows01,0),]
  train35 = train_3_5[1:round(perc*nrows35,0),]
  labelstrain01 = true_label_train_0_1[1:round(perc*nrows01,0)]
  labelstrain35 = binary35_label_train[1:round(perc*nrows35,0)]
  
  for(i in seq(1:1)){
    theta01 = logisticRegression(train01, labelstrain01)
    theta35 = logisticRegression(train35, labelstrain35)
    pred01_prob_train = predict_prob(theta01, train01)
    pred01_prob_test = predict_prob(theta01, test_0_1)
    pred35_prob_train = predict_prob(theta35, train35)
    pred35_prob_test = predict_prob(theta35, test_3_5)
    predict01_train = predict_val(pred01_prob_train)
    predict01_test = predict_val(pred01_prob_test)
    predict35_train = predict_val(pred35_prob_train)
    predict35_test = predict_val(pred35_prob_test)
    acc.01.train = accuracy(predict01_train, labelstrain01)
    acc.01.test = accuracy(predict01_test, true_label_test_0_1)
    acc.35.train = accuracy(predict35_train, labelstrain35)
    acc.35.test = accuracy(predict35_test, binary35_label_test)
    acc.01.train_hist[i] = acc.01.train
    acc.01.test_hist[i] = acc.01.test
    acc.35.train_hist[i] = acc.35.train
    acc.35.test_hist[i] = acc.35.test
    print(paste(i,"--- acc.01.train =",acc.01.train,"--- acc.01.test =",acc.01.test,
                "--- acc.35.train =",acc.35.train,"--- acc.35.test =",acc.35.test))
  }
  print(paste("AVG.01.TRAIN =",mean(acc.01.train_hist),"--- AVG.01.TEST =",mean(acc.01.test_hist),
              "--- AVG.35.TRAIN =",mean(acc.35.train_hist),"--- AVG.35.TEST =",mean(acc.35.test_hist)))
}

problem5(0.05)
problem5(0.1)



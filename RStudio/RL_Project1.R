library(ggplot2)
#install.packages("hydroGOF")
require(hydroGOF)

# ===================================== #
# Reinforcement Learning - Project 1
# dgiron3

# ===================================== #
# Experiment 1
lastRow <- function(X){
  return(dim(X)[1])
}

lastRowSum <- function(X){
  rowNum = lastRow(X)
  return(sum(X[rowNum,]))
}

generateSequence <- function(){
  xB = c(1,0,0,0,0)
  xC = c(0,1,0,0,0)
  xD = c(0,0,1,0,0)
  xE = c(0,0,0,1,0)
  xF = c(0,0,0,0,1)
  X = xD
  randNum = runif(1)
  if(randNum < 0.5) {X = rbind(X,xC)} else {X = rbind(X,xE)}
  sum = lastRowSum(X)
  while(sum ==1){
    randNum = runif(1)
    if(all(X[lastRow(X),] == xD) == TRUE){
      if(randNum < 0.5) {X = rbind(X,xC)} else {X = rbind(X,xE)}
    }
    else if(all(X[lastRow(X),] == xC) == TRUE){
      if(randNum < 0.5) {X = rbind(X,xB)} else {X = rbind(X,xD)}
    }
    else if(all(X[lastRow(X),] == xB) == TRUE){
      if(randNum < 0.5) {X = rbind(X,rep(0, times = 5))} else {X = rbind(X,xC)}
    }
    else if(all(X[lastRow(X),] == xE) == TRUE){
      if(randNum < 0.5) {X = rbind(X,xD)} else {X = rbind(X,xF)}
    }
    else {
      if(randNum < 0.5) {X = rbind(X,xE)} else {X = rbind(X,rep(1, times = 5))}
    }
    sum = lastRowSum(X)
  }
  return(X)
}

exp1seqs <- function(seq, weights, alpha, L){
  dw = rep(0, times = 5)
  e = seq[1,]
  for(t in seq(1,lastRow(seq)-1)){
    Pt = weights %*% seq[t,]
    Pt_1 = weights %*% seq[t+1,]
    if(sum(seq[t+1,]) > 1) {Pt_1 = 1}
    dw = dw + alpha * (Pt_1 - Pt) * e
    e = seq[t+1,] + L * e
  }
  return(dw)
}

exp1train <- function(alpha,tolerance){
  lambda = c(0.0,0.1,0.3,0.5,0.7,0.9,1.0)
  idealProj = c(1/6,1/3,1/2,2/3,5/6)
  trainSet = seq(1,100)
  RMS = array(0, dim = c(length(lambda),length(trainSet)))
  for(trnSet in seq(1,length(trainSet))){
    seq1 = generateSequence()
    seq2 = generateSequence()
    seq3 = generateSequence()
    seq4 = generateSequence()
    seq5 = generateSequence()
    seq6 = generateSequence()
    seq7 = generateSequence()
    seq8 = generateSequence()
    seq9 = generateSequence()
    seq10 = generateSequence()
    for(L in seq(1,length(lambda))){
      weights = rep(0.5, times = 5)
      norm_score = 1
      tol = tolerance
      dW = rep(0, times = 5)
      while(norm_score > tol){
        dw1 = exp1seqs(seq1, weights, alpha, lambda[L])
        dw2 = exp1seqs(seq2, weights, alpha, lambda[L])
        dw3 = exp1seqs(seq3, weights, alpha, lambda[L])
        dw4 = exp1seqs(seq4, weights, alpha, lambda[L])
        dw5 = exp1seqs(seq5, weights, alpha, lambda[L])
        dw6 = exp1seqs(seq6, weights, alpha, lambda[L])
        dw7 = exp1seqs(seq7, weights, alpha, lambda[L])
        dw8 = exp1seqs(seq8, weights, alpha, lambda[L])
        dw9 = exp1seqs(seq9, weights, alpha, lambda[L])
        dw10 = exp1seqs(seq10, weights, alpha, lambda[L])
        dW = (dw1+dw2+dw3+dw4+dw5+dw6+dw7+dw8+dw9+dw10)
        norm_score = norm(dW, type = "2")
        weights = weights + dW
      }
      RMS[L,trnSet] = rmse(weights,idealProj)
    }
    print(paste("train set =",trainSet[trnSet]))
  }
  RMS_sum = array(0, dim = c(length(lambda)))
  for(i in seq(1,length(trainSet))){
    RMS_sum = RMS_sum + RMS[,i]
  }
  RMS_mean = RMS_sum/length(trainSet)
  df = data.frame(lambda,RMS_mean)
  return(df)
}

exp1graph <- function(df){
  ggplot(df, aes(x = lambda, y = RMS_mean)) + geom_line() + labs(x = "lambda", y = "ERROR", title = "RMS Error vs lambda")
}

# ===================================== #
# Experiment 2 - Figure 4

exp2train <- function(){
  lambda = c(0.0,0.3,0.8,1.0)
  alpha = seq(0.0,0.5, by = 0.05)
  idealProj = c(1/6,1/3,1/2,2/3,5/6)
  trainSet = seq(1,100)
  RMS = array(0, dim = c(length(alpha),length(lambda),length(trainSet)))
  for(trnSet in seq(1,length(trainSet))){
    seq1 = generateSequence()
    seq2 = generateSequence()
    seq3 = generateSequence()
    seq4 = generateSequence()
    seq5 = generateSequence()
    seq6 = generateSequence()
    seq7 = generateSequence()
    seq8 = generateSequence()
    seq9 = generateSequence()
    seq10 = generateSequence()
    for(L in seq(1,length(lambda))){
      for(A in seq(1,length(alpha))){
        weights = rep(0.5, times = 5)
        weights = weights + exp1seqs(seq1, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq2, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq3, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq4, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq5, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq6, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq7, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq8, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq9, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq10, weights, alpha[A], lambda[L])
        RMS[A,L,trnSet] = rmse(weights,idealProj)
      }
    }
  }
  RMS_sum = array(0, dim = c(length(alpha),length(lambda)))
  for(i in seq(1,length(trainSet))){
    RMS_sum = RMS_sum + RMS[,,i]
  }
  RMS_mean = RMS_sum/length(trainSet)
  L0.0 = RMS_mean[,1]
  L0.3 = RMS_mean[,2]
  L0.8 = RMS_mean[,3]
  L1.0 = RMS_mean[,4]
  df = data.frame(alpha,L0.0,L0.3,L0.8,L1.0)
  return(df)
}

exp2graph <- function(df2){
  ggplot(df2, aes(x = alpha, y = ERROR, color = variable)) +
    geom_line(aes(y = L0.0, col = "L0.0")) +
    geom_line(aes(y = L0.3, col = "L0.3")) +
    geom_line(aes(y = L0.8, col = "L0.8")) +
    geom_line(aes(y = L1.0, col = "L1.0")) +
    labs(title = "RMS Error vs. alpha")
}


# ===================================== #
# Experiment 2 - Figure 5

exp3train <- function(){
  lambda = seq(0,1, by = 0.1)
  alpha = seq(0.0,0.5, by = 0.05)
  idealProj = c(1/6,1/3,1/2,2/3,5/6)
  trainSet = seq(1,100)
  RMS = array(0, dim = c(length(alpha),length(lambda),length(trainSet)))
  for(trnSet in seq(1,length(trainSet))){
    seq1 = generateSequence()
    seq2 = generateSequence()
    seq3 = generateSequence()
    seq4 = generateSequence()
    seq5 = generateSequence()
    seq6 = generateSequence()
    seq7 = generateSequence()
    seq8 = generateSequence()
    seq9 = generateSequence()
    seq10 = generateSequence()
    for(L in seq(1,length(lambda))){
      for(A in seq(1,length(alpha))){
        weights = rep(0.5, times = 5)
        weights = weights + exp1seqs(seq1, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq2, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq3, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq4, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq5, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq6, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq7, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq8, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq9, weights, alpha[A], lambda[L])
        weights = weights + exp1seqs(seq10, weights, alpha[A], lambda[L])
        RMS[A,L,trnSet] = rmse(weights,idealProj)
      }
    }
  }
  RMS_sum = array(0, dim = c(length(alpha),length(lambda)))
  for(i in seq(1,length(trainSet))){
    RMS_sum = RMS_sum + RMS[,,i]
  }
  RMS_mean = RMS_sum/length(trainSet)
  L0.0 = RMS_mean[,1]
  L0.1 = RMS_mean[,2]
  L0.2 = RMS_mean[,3]
  L0.3 = RMS_mean[,4]
  L0.4 = RMS_mean[,5]
  L0.5 = RMS_mean[,6]
  L0.6 = RMS_mean[,7]
  L0.7 = RMS_mean[,8]
  L0.8 = RMS_mean[,9]
  L0.9 = RMS_mean[,10]
  L1.0 = RMS_mean[,11]
  df_0 = data.frame(alpha,L0.0,L0.1,L0.2,L0.3,L0.4,L0.5,L0.6,L0.7,L0.8,L0.9,L1.0)
  RMS_best = array(dim = c(length(lambda)))
  for(i in seq(1,length(lambda))){
    RMS_best[i] = min(df_0[,i+1])
  }
  df3 = data.frame(lambda,RMS_best)
  return(df3)
}

exp3graph <- function(df3){
  ggplot(df3, aes(x = lambda, y = RMS_best)) + geom_line() + labs(x = "lambda", y = "ERROR USING BEST ALPHA", title = "RMS Error vs lambda")
}

# ================================== #
# Implementation of all three parts

alpha = 0.001
tol = 1e-3

# df = exp1train(alpha, tol)
# print(df)
# exp1graph(df)

df2 = exp2train()
print(df2)
exp2graph(df2)

df3 = exp3train()
print(df3)
exp3graph(df3)


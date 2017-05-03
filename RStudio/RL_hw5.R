# Homework 5 #
# dgiron3
library(ggplot2)
library(reshape2)

pt1Band <- function(k = 100){
  band = rep(0, times = 0)
  for(arm in seq(1,k)){
    band[arm] = rnorm(1, mean = 5, sd = 1)
  }
  return(band)
}

pt2Band <- function(k = 10){
  band = rep(0, times = 0)
  for(arm in seq(1,k)){
    band[arm] = rnorm(1, mean = 5, sd = 1)
  }
  return(band)
}

e_greedy <- function(Q, eps){
  xi = runif(1)
  if(xi < eps){
    a = sample(1:length(Q),1)
  } else {
    a = which.max(Q)
  }
  return(a)
}

pt1Experiment <- function(numTests = 500, steps = 10000, eps = 0.1, softmax = FALSE, initialQ = 5){
  Q_matrix = rep(0, times = 0)
  maxQ = rep(0, times = 0)
  R_matrix = rep(0, times = 0)
  optActnPerc_matrix = rep(0, times = 0)
  for(test in seq(1,numTests)){
    print(test)
    band = pt1Band()
    optActn = which.max(band)
    optActnCnt = 0
    optActnPerc = rep(0, times = steps)
    Q = rep(initialQ, times = length(band))
    Q_cnt = rep(0, times = length(band))
    reward = rep(0, times = steps)
    H = rep(0, times = length(band))
    for(step in seq(1,steps)){
      if(softmax){
        xi = runif(1)
        if(xi < eps){
          a = sample(1:length(Q),1)
        } else {
          weights = rep(0, times = length(Q))
          sum = 0
          for(b in seq(1,length(Q))){
            sum = sum + exp(H[b])
          }
          for(act in seq(1,length(Q))){
            weights[act] = exp(H[act]) / sum
          }
          a = sample(1:length(Q),1, prob = weights)
        }
      } else {
        a = e_greedy(Q, eps)
      }
      if(a == optActn){optActnCnt = optActnCnt + 1}
      optActnPerc[step] = optActnCnt / steps
      Q_cnt[a] = Q_cnt[a] + 1
      alpha = 1 / Q_cnt[a]
      reward[step] = rnorm(1, mean = band[a], sd = 1)
      if(softmax){
        avgR = mean(reward[1:step])
        for(act in seq(1,length(Q))){
          if(act != a){
            H[act] = H[act] - 0.1 * (reward[step] - avgR) * weights[act]
          } else if(act == a){
            H[act] = H[act] + 0.1 * (reward[step] - avgR) * (1 - weights[act])
          }
        }
      }
      Q[a] = Q[a] + alpha * (reward[step] - Q[a])
    }
    Q_matrix = rbind(Q_matrix,Q)
    maxQ[test] = max(Q)
    R_matrix = rbind(R_matrix,reward)
    optActnPerc_matrix = rbind(optActnPerc_matrix,optActnPerc)
  }
  R_means = colMeans(R_matrix)
  optActPerc_means = colMeans(optActnPerc_matrix)
  return(list(maxQavg=mean(maxQ),R_means=R_means,optActPerc_means=optActPerc_means))
}

pt2Experiment <- function(numTests = 500, steps = 10000, eps = 0.1, softmax = FALSE, initialQ = 5){
  Q_matrix = rep(0, times = 0)
  maxQ = rep(0, times = 0)
  R_matrix = rep(0, times = 0)
  optActnPerc_matrix = rep(0, times = 0)
  for(test in seq(1,numTests)){
    print(test)
    band = pt2Band()
    optActnCnt = 0
    optActnPerc = rep(0, times = steps)
    Q = rep(initialQ, times = length(band))
    Q_cnt = rep(0, times = length(band))
    reward = rep(0, times = steps)
    H = rep(0, times = length(band))
    for(step in seq(1,steps)){
      if(softmax){
        xi = runif(1)
        if(xi < eps){
          a = sample(1:length(Q),1)
        } else {
          weights = rep(0, times = length(Q))
          sum = 0
          for(b in seq(1,length(Q))){
            sum = sum + exp(H[b])
          }
          for(act in seq(1,length(Q))){
            weights[act] = exp(H[act]) / sum
          }
          a = sample(1:length(Q),1, prob = weights)
        }
      } else {
        a = e_greedy(Q, eps)
      }
      band[a] = sample(0:10,1)
      optActn = which.max(band)
      if(a == optActn){optActnCnt = optActnCnt + 1}
      optActnPerc[step] = optActnCnt / steps
      Q_cnt[a] = Q_cnt[a] + 1
      alpha = 1 / Q_cnt[a]
      reward[step] = rnorm(1, mean = band[a], sd = 1)
      if(softmax){
        avgR = mean(reward[1:step])
        for(act in seq(1,length(Q))){
          if(act != a){
            H[act] = H[act] - 0.1 * (reward[step] - avgR) * weights[act]
          } else if(act == a){
            H[act] = H[act] + 0.1 * (reward[step] - avgR) * (1 - weights[act])
          }
        }
      }
      Q[a] = Q[a] + alpha * (reward[step] - Q[a])
    }
    Q_matrix = rbind(Q_matrix,Q)
    maxQ[test] = max(Q)
    R_matrix = rbind(R_matrix,reward)
    optActnPerc_matrix = rbind(optActnPerc_matrix,optActnPerc)
  }
  R_means = colMeans(R_matrix)
  optActPerc_means = colMeans(optActnPerc_matrix)
  return(list(maxQavg=mean(maxQ),R_means=R_means,optActPerc_means=optActPerc_means))
}

smallEpsListP1 = pt1Experiment(eps = 0.01)
bigEpsListP1 = pt1Experiment()
softmaxListp1 = pt1Experiment(softmax = TRUE)
initListp1 = pt1Experiment(eps = 0, initialQ = 10)
smallEpsListP2 = pt2Experiment(eps = 0.01)
bigEpsListP2 = pt2Experiment()
softmaxListp2 = pt2Experiment(softmax = TRUE)
initListp2 = pt2Experiment(eps = 0, initialQ = 10)
steps = 10000
avgRewardP1 = data.frame(Steps=seq(1,steps),
                       smallEpsP1=smallEpsListP1$R_means,
                       bigEpsP1=bigEpsListP1$R_means,
                       softmaxP1=softmaxListp1$R_means,
                       initP1=initListp1$R_means)

optActnPercP1 = data.frame(Steps=seq(1,steps),
                         smallEpsP1=smallEpsListP1$optActPerc_means,
                         bigEpsP1=bigEpsListP1$optActPerc_means,
                         softmaxP1=softmaxListp1$optActPerc_means,
                         initP1=initListp1$optActPerc_means)

avgRewardP2 = data.frame(Steps=seq(1,steps),
                         smallEpsP2=smallEpsListP2$R_means,
                         bigEpsP2=bigEpsListP2$R_means,
                         softmaxP2=softmaxListp2$R_means,
                         initP2=initListp2$R_means)

optActnPercP2 = data.frame(Steps=seq(1,steps),
                           smallEpsP2=smallEpsListP2$optActPerc_means,
                           bigEpsP2=bigEpsListP2$optActPerc_means,
                           softmaxP2=softmaxListp2$optActPerc_means,
                           initP2=initListp2$optActPerc_means)

avgRewardP1 = melt(avgRewardP1, id.vars = 'Steps', variable.name = 'Exploration')
avgRewardP2 = melt(avgRewardP2, id.vars = 'Steps', variable.name = 'Exploration')
optActnPercP1 = melt(optActnPercP1, id.vars = 'Steps', variable.name = 'Exploration')
optActnPercP2 = melt(optActnPercP2, id.vars = 'Steps', variable.name = 'Exploration')

ggplot(avgRewardP1, aes(x = Steps, y = value)) +
  geom_line(aes(colour = Exploration)) +
  labs(y = 'Average Reward P1')

ggplot(avgRewardP2, aes(x = Steps, y = value)) +
  geom_line(aes(colour = Exploration)) +
  labs(y = 'Average Reward P2')

ggplot(optActnPercP1, aes(x = Steps, y = value)) +
  geom_line(aes(colour = Exploration)) +
  labs(y = '% Optimal Action P1')

ggplot(optActnPercP2, aes(x = Steps, y = value)) +
  geom_line(aes(colour = Exploration)) +
  labs(y = '% Optimal Action P2')

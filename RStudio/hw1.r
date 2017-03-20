sink("hw1_output.txt", type = c("output", "message"))
### Code for CSE-6242 (Data and Visual Analytics) Homework 1 ###
# dgiron3
# =============================== #
# Problem 1 #

w1 = seq(0,1, by = 0.1)
w2 = seq(0,1, length.out = 11)
stopifnot(w1 == w2)
print(w1)

# =============================== #
# Problem 2 #

log_gamma_loop = function(n){
  if (n == 1) return(0)
  n = n - 1
  sum = 0
  for (i in n:1){
    sum = sum + log(i)
  }
  return(sum)
}

# =============================== #
# Problem 3 #

log_gamma_recursive = function(n){
  if (n == 1) return(0)
  n = n - 1
  return(log(n) + log_gamma_recursive(n))
}

# =============================== #
# Problem 4 #

sum_log_gamma_loop = function(n){
  sum = 0
  for (i in 1:n){
    sum = sum + log_gamma_loop(i)
  }
  return(sum)
}

sum_log_gamma_recursive = function(n){
  sum = 0
  for (i in 1:n){
    sum = sum + log_gamma_recursive(i)
  }
  return(sum)
}

## Code Implementation of 2-4
for (i in 1:5){
  stopifnot(all.equal(log_gamma_loop(i), lgamma(i)))
  stopifnot(all.equal(log_gamma_recursive(i), lgamma(i)))
  stopifnot(all.equal(sum_log_gamma_loop(i), sum_lgamma(i)))
  stopifnot(all.equal(sum_log_gamma_recursive(i), sum_lgamma(i)))
}

print("Tests passed!")

# =============================== #
# Problem 5 #

require(reshape2)
require(ggplot2)
sum_lgamma <- function(n){
  if (n < 1){
    stop("value out of range")
  }
  sum <- 0
  for (i in seq(1,n,1)){
    sum <- sum + lgamma(i)
  }
  return(sum)
}

# Uncomment Below to produce all 3 functions time comparison (runtime ~ 75 seconds)
expressions = 500000
time_sum_lgamma = system.time(sum_lgamma(1))[1]
time_sum_log_gamma_loop = system.time(sum_log_gamma_loop(1))[1]
time_sum_log_gamma_recursive = system.time(sum_log_gamma_recursive(1))[1]
i = 0
while (i < 3100){
  i = i + 100
  time_sum_lgamma = rbind(time_sum_lgamma, system.time(sum_lgamma(i))[1])
  time_sum_log_gamma_loop = rbind(time_sum_log_gamma_loop, system.time(sum_log_gamma_loop(i))[1])
  time_sum_log_gamma_recursive = rbind(time_sum_log_gamma_recursive, system.time(sum_log_gamma_recursive(i))[1])
}
df = data.frame(n = seq(0,3100,by=100),
                lgamma = time_sum_lgamma,
                gamma_loop = time_sum_log_gamma_loop,
                gamma_recursive = time_sum_log_gamma_recursive)

colnames(df)[2] <- "lgamma"
colnames(df)[3] <- "gamma_loop"
colnames(df)[4] <- "gamma_recursive"

ggplot(df, aes(x = n, y = t, color = variable)) + geom_point(aes(y = lgamma, col = "lgamma")) +
  + geom_point(aes(y = gamma_loop, col = "gamma_loop")) +
  + geom_point(aes(y = gamma_recursive, col = "gamma_recursive"))

# Uncomment Below to get R lgamma function comparison with loop lgamma function (runtime ~ 24 minutes)
# time_sum_lgamma = system.time(sum_lgamma(1))[1]
# time_sum_log_gamma_loop = system.time(sum_log_gamma_loop(1))[1]
# i = 1
# n = rep(0, times = 6)
# n[1] = 1
# n[2] = n[1]*10
# n[3] = n[2]*10
# n[4] = n[3]*10
# n[5] = n[4]*10
# n[6] = n[5]*10
# while (i < 100000){
#   i = i*10
#   time_sum_lgamma = rbind(time_sum_lgamma, system.time(sum_lgamma(i))[1])
#   time_sum_log_gamma_loop = rbind(time_sum_log_gamma_loop, system.time(sum_log_gamma_loop(i))[1])
# }
# df2 = data.frame(n = n,
#                 lgamma = time_sum_lgamma,
#                 gamma_loop = time_sum_log_gamma_loop)
# 
# colnames(df2)[2] <- "lgamma"
# colnames(df2)[3] <- "gamma_loop"
# 
# ggplot(df2, aes(x = n, y = t, color = variable)) + geom_point(aes(y = lgamma, col = "lgamma")) +
#  geom_point(aes(y = gamma_loop, col = "gamma_loop"))

# Uncomment Below to get R lgamma function time analysis (runtime ~ 6 minutes)
# time_sum_lgamma = system.time(sum_lgamma(1))[1]
# i = 1
# n = rep(0, times = 6)
# n[1] = 1
# n[2] = n[1]*10
# n[3] = n[2]*10
# n[4] = n[3]*10
# n[5] = n[4]*10
# n[6] = n[5]*10
# n[7] = n[6]*10
# n[8] = n[7]*10
# n[9] = n[8]*10
# n[10] = n[9]*10
# while (i < 1000000000){
#   i = i*10
#   time_sum_lgamma = rbind(time_sum_lgamma, system.time(sum_lgamma(i))[1])
# }
# df3 = data.frame(n = n,
#                 lgamma = time_sum_lgamma)
# 
# colnames(df3)[2] <- "lgamma"
# 
# ggplot(df3, aes(x = n, y = t, color = variable)) + geom_point(aes(y = lgamma, col = "lgamma"))

# =============================== #
sink()
closeAllConnections()

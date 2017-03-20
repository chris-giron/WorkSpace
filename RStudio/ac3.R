### Code for CSE-6242 (Data and Visual Analytics) ac3 ###
# dgiron3
# =============================== #
require(ggplot2)
log_factorial <- function (n) {
  # Return the log of factorial(n) for any integer n > 0
  if (n <= 1)
    return (0)
  return (log(n) + log_factorial(n - 1))
}

sum_log_factorial <- function (n) {
  # Return the sum of log_factorial(i) for i in 1..n
  sum <- 0
  for(i in seq(1, n, 1)) {
    sum <- sum + log_factorial(i)
  }
  return (sum)
}

fibonacci <- function(n) {
  # Return nth Fibonacci number
  if (n <= 1)
    return (n)
  return (fibonacci(n - 1) + fibonacci(n - 2))
}

# Make N vector, initialize time vectors

make_log_fact_df <- function(){
  N_logfact = c(1,seq(10,100, by = 10),
        seq(100,3150,by=50))
  
  time_logfact = array(0, dim = c(length(N_logfact)))
  time_sumlogfact = array(0, dim = c(length(N_logfact)))
  options(expressions=500000)
  for(n in seq(1,length(N_logfact))){
     time_logfact[n] = system.time(log_factorial(N_logfact[n]))
     time_sumlogfact[n] = system.time(sum_log_factorial(N_logfact[n]))
     print(paste('n =',N_logfact[n],' ---',round(N_logfact[n]/3150*100,1),'%'))
  }
  df_logfact = data.frame(n = N_logfact, time_logfact = time_logfact, time_sumlogfact = time_sumlogfact)
  return(df_logfact)
}

make_fibonacci_df <- function(){
  N_fib = seq(1,40)
  time_fib = array(0, dim = c(length(N_fib)))
  
  for(n in seq(1,length(N_fib))){
    time_fib[n] = system.time(fibonacci(N_fib[n]))
    print(paste('n =',N_fib[n],' ---',round(N_fib[n]/40*100,1),'%'))
  }
  df_fib = data.frame(n = N_fib, time_fib = time_fib)
  return(df_fib)
}

#df_logfact = make_log_fact_df()
#df_fib = make_fibonacci_df()

plot_logfact <- function(df_logfact){
  ggplot(data = df_logfact, aes(x = n, y = time_logfact)) + 
    geom_line(colour = 'blue') +
    labs(title = 'Log Factorial Runtimes vs. n', y = 'Log Factorial Runtime(s)')
}

plot_logfactsum <- function(df_logfact){
  ggplot(data = df_logfact, aes(x = n, y = time_sumlogfact)) + 
    geom_line(colour = 'green') +
    labs(title = 'Sum Log Factorial Runtimes vs. n', y = 'Sum Log Factorial Runtime(s)')
}

plot_logfactboth <- function(df_logfact){
  ggplot(data = df_logfact, aes(x = n, y = Runtime(s), color = variable)) + 
    geom_line(aes(y = time_logfact, col = 'Log Factorial Runtime')) +
    geom_line(aes(y = time_sumlogfact, col = 'Sum Log Factorial Runtime')) +
    labs(title = 'Factorial Function Runtimes vs. n')
}

plot_fib <- function(df_fib){
  ggplot(data = df_fib, aes(x = n, y = time_fib)) + 
    geom_line(colour = 'red') +
    labs(title = 'Fibonacci Runtimes vs. n', y = 'Time(s)')
}

plot_logfact(df_logfact)
plot_logfactsum(df_logfact)
plot_logfactboth(df_logfact)
plot_fib(df_fib)











source('hw1.r')

for (i in 1:5){
  stopifnot(all.equal(log_gamma_loop(i), lgamma(i)))
  stopifnot(all.equal(log_gamma_recursive(i), lgamma(i)))
  stopifnot(all.equal(sum_log_gamma_loop(i), sum_lgamma(i)))
  stopifnot(all.equal(sum_log_gamma_recursive(i), sum_lgamma(i)))
}

print("Tests passed!")
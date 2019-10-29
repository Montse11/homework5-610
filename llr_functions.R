llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*%  Wz %*% y
  return(f_hat)
}

make_weight_matrix = function(z, x, omega){
  W = abs(x-z)/omega
  return(diag(W))
}

make_predictor_matrix = function(x){
  ones = rep(1, times = length(x)) 
  return(matrix(c(ones,x), nrow = length(x), ncol = 2, byrow = FALSE))
}

library(reshape2)
data(french_fries)
french_fries = french_fries[complete.cases(french_fries),]
z = seq(0, 15, length.out = 300)
fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 2)
plot(z, fits)


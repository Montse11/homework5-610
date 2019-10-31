setwd("/Users/azakeres/Downloads/homework5-610")

llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

make_weight_matrix = function(z, x, omega) {
  r = abs(x - z) / omega  # this is a vector of the same length as x
  w = sapply(r, W)  # this is a vector of the same length as x and r
  Wz = diag(w)  # this is a diagonal matrix with elements from w
  return(Wz)
}

W = function(r) {
  if (abs(r) < 1) {
    return((1 - abs(r) ** 3) ** 3)
  } else {
    return(0)
  }
}
make_predictor_matrix = function(x){
  ones = rep(1, times = length(x)) 
  return(matrix(c(ones,x), nrow = length(x), ncol = 2, byrow = FALSE))
}

library(reshape2)
data(french_fries)

french_fries = french_fries[complete.cases(french_fries),]
z = seq(0, 15, length.out = 100)
fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 200)
plot(french_fries$potato, french_fries$buttery)
lines(z, fits, col = 'red')


# STAT-S 610
# LAB 4
# 2019-10-03
# https://jfukuyama.github.io/teaching/stat610/assignments/lab4.pdf

# --- functions --- #

#' @param x (numeric) vector of same length as y
#' @param y (numeric) vector of same length as y
#' @param z (numeric) vector, can be of a different length
#' @param omega (numeric) must be a scalar
#' @return (numeric) vector of the same length as z

llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

#' @param z (numeric) must be a scalar
#' @param x (numeric) vector of the same length as y
#' @param y (numeric) vector of the same length as x
#' @param omega (numeric) must be a scalar
#' @return (numeric) scalar

compute_f_hat = function(z, x, y, omega) {
  Wz = diag(make_weight_matrix(z, x, omega))
  X = make_predictor_matrix(x)
  n = nrow(X)
  A = t(sapply(1:n, function(i){
    Wz[i]*X[i,]
  }))
  f_hat = c(1,z) %*% solve(t(X)%*% A) %*% t(X) %*% (Wz*y)
  return(f_hat)
}


#' @param z (numeric) must be a scalar
#' @param x (numeric) vector of arbitrary length
#' @param omega (numeric) must be a scalar
#' @return (numeric) a diagonal matrix

make_weight_matrix = function(z, x, omega) {
  r = abs(x - z) / omega  # this is a vector of the same length as x
  w = sapply(r, W)  # this is a vector of the same length as x and r
  Wz = diag(w)  # this is a diagonal matrix with elements from w
  return(Wz)
}

#' @param r (numeric) must be a scalar
#' @return (numeric) scalar
W = function(r) {
  if (abs(r) < 1) {
    return((1 - abs(r) ** 3) ** 3)
  } else {
    return(0)
  }
}

#' @param x (numeric) vector of arbitrary length
#' @return (numeric) matrix with 2 columns and rows equal to length of x
make_predictor_matrix = function(x) {
  n = length(x)
  return(cbind(rep(1, n), x))
}

#Krisy's test for commit#
# --- example 1 --- #

TEST=function(x){
  print("bacon")
}
# get the data
data(french_fries, package = 'reshape2')
french_fries = na.omit(french_fries)

# input data
x = french_fries$potato
y = french_fries$buttery

# space along which to smooth
z = seq(0, 15, length.out = 100)

# run smoothing
fits = llr(z = z, x = x, y = y, omega = 2)

# plot the data and the smoother
plot(x, y)
lines(z, fits, col = 'red')


# --- example 2 --- #

# noisy sine wave
x = runif(1000, -2 * pi, 2 * pi)
y = sin(x) + rnorm(length(x))

# space along which to smooth
z = seq(-2 * pi, 2 * pi, length.out = 100)

# run smoothing
fits = llr(z = z, x = x, y = y, omega = pi / 3)


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

# plot the data and the smoother
plot(x, y)
lines(z, fits, col = 'red')

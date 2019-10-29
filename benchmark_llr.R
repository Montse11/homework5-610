library(microbenchmark)
source("llr_functions.R")

x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
omega = 2

microbenchmark(
  llr(x=x,z=z,y=y,omega = omega)
)

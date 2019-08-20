require(pracma)
#R2_F1
F1 <- function(x) c(x[1]^2 + x[2]^2 - 1, sin(pi*x[1]/2) + x[2]^3)
broyden(F1, x0 = c(1, 1))
#R2_TS
F2 <- function(x) c(x[2]-x[1]^3,x[1]^2+x[2]^2-1)
x0 <- (c(1, 1))
broyden(F2, x0)
#Complejo
F3 <- function(x) {
  z  <- x[1] + x[2]*1i
  fz <- sin(z)^2 + sqrt(z) - log(z)
  c(Re(fz), Im(fz))
}
broyden(F3, c(1, 1))
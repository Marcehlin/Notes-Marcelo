#set.seed(1)
u1 <- runif(1)
if (u1 < 0.3) {
  x1 <- 1
} else if(u1 < 0.4) {
  x1 <-  3
}else {
  x1 <- 4
}

x1
u1

n <- 10000
gerador_de_x <- function(n) {
  u <- runif(n)
  i <- 1
  x <- rep(NA, n)
  for (i in 1:n){
    if (u[i] < 0.3) {
      x[i] <- 1
    } else if(u[i] < 0.4) {
      x[i] <- 3
    } else {
      x[i] <- 4
    }
  }
  return (x)
}
X <- gerador_de_x(n)
hist(X)

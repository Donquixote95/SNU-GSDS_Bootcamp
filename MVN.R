library(expm) # for matrix square root
N<-1000
Mu = c(170, 72)
Sigma = matrix(c(25, 12, 12, 16), ncol = 2)
Z = matrix(rnorm(N*2),nrow=2)
X = Mu + sqrtm(Sigma) %*% Z
plot(X[1,], X[2,],
     xlab = "height", ylab = "weight")
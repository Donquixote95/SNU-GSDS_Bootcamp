# Law of large number

mu = 5
nism <- 100 #simulation
N.all <- c(10, 100, 1000, 10000)
OUT.all <- matrix(0, nsim, 4)

for(i in 1:nsim){
  for(j in 1:4){
    OUT.all[i,j] <- mean(rnorm(N.all[j], mean = mu))
  }
}

par(mfrow = c(2,2))
for(j in 1:4)
  hist(OUT.all[,j], xlim = c(3.8,6.2), main=paste("N=", N.all[j]))


n.all<-c(1, 10, 100, 10000)
x<-matrix(rbinom(10000*1000, 1, 0.1), nrow = 1000)

par(mfrow = c(2,2))
for(i in 1:4){
  n<-n.all[i]
  x_bar <- rowMeans(cbind(x[, 1:n]))
  z = (x_bar - 0.1)*sqrt(n)/sqrt(0.1*0.9)
  hist(z, main=paste("n=", n), xlab="z", ylab="frequency")
}
#Bootstrap for The Median

# given data X = (X(1), ... , X(n))

# T <- median(X)
# Tboot <- vector of length B
# for(i in 1:B){
#   Xstar <- sample of size n from X (with replacement)
#   Tboot[i] <- median(Xstar)
# }
# se <- sqrt(variance(Tboot))

x<-c(1,3,2,5,53,5,5,47,4,7,7,56,56,56,2,3,4,5,6,8,9,0,1,2,3,4,6,3,3,2,2,1,2)
B<-1000
Tboot <- rep(0,B)
for(i in 1:B){
  Xstar<-sample(X, replace=TRUE)
  Tboot[i]<-median(Xstar)
}
var(Tboot)
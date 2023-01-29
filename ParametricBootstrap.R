set.seed(100)
# data generation
n<-40
X<-rbinom(n, 1, 0.4)

# MLE estimation
Phat <- mean(X)
Psihat = log(Phat/(1-Phat))

# Parametric Bootstrap
nSIM<-1000; out<-rep(NA, nSIM)
for(i in 1:nSIM){
  X1<-rbinom(n, 1, Phat)
  Phat1<-mean(X1)
  Psihat1 = log(Phat1/(1-Phat1))
  out[i]<-Psihat1
}

# From PB(Prametric Bootstrap)
sd(out)

# From derivation
1/sqrt(n*Phat*(1-Phat)) # Delta
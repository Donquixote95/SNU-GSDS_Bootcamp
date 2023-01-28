nSIM = 1000 # B
n = 20 # sample size
median_all <- rep(0, nSIM)

for (i in 1:nSIM){
  data_gen<-rgamma(n,1,2)
  median_all[i]<-median(data_gen)
}

var(median_all)

#Hierarchical model
nSIM=1000
n=20
mean_all<-rep(0,nSIM)

for(i in 1:nSIM){
  Z1<- rbinom(n, 1, 0.3)
  Z2<- rbinom(n, 1, 0.5)
  X<-rnorm(n, Z1+Z2) # Z1 + Z2는 mean
  mean_all[i]<-mean(X)
}

var(mean_all)
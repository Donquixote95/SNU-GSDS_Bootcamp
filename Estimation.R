nSIM<-1000
n<-100
p<-0.3
p_est<-rep(0,nSIM)
for(i in 1:nSIM){
  X<-rbinom(n,1,p)
  p_est[i]<-mean(X)
}

mean(p_est)
sd(p_est) #Monte-Carlo Method

#Theoretical SE
sqrt(p*(1-p)/n)

hist(p_est)


nSIM<-100
n<-100
p<-0.3
p_set<-rep(0,nSIM)
se_est<-rep(0,nSIM)
CI<-matrix(rep(0,nSIM*2), ncol=2)
for(i in 1:nSIM){
  X<-rbinom(n,1,p)
  p_est[i]<-mean(X)
  se_est[i]<-sqrt(p_est[i]*(1-p_est[i])/n)
  CI[i,]<-c(p_est[i]-1.96*se_est[i],p_est[i]+1.96*se_est[i])
}

plot(CI[c(1:10),1], ylim=c(0.1,0.5),col="blue")
points(CI[c(1:10),2], col="red")
abline(v=1:10, col="lightgray", lty=3)
abline(h=p)
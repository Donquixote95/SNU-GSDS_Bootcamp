df = read.table("coris.txt", sep=",", header=TRUE)

N <- n0+n1
nPerm<-10000

perm.out<-rep(0,nPerm)
for(i in 1:nPerm){
  
  chd.perm<- sample(df$chd)
  
  Mu0.perm = mean(df$sbp[chd.perm==0])
  Mu1.perm = mean(df$sbp[chd.perm==1])
  perm.out[i] <- abs(Mu1.perm - Mu0.perm)
}

T = (Mu1 - Mu0)
Pvalue.perm <- mean(T <= perm.out)

T
Pvalue.perm
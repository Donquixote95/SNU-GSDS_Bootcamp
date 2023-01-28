x ~ N(0,1)

# E(X^3) ?
# V(X^3) ?

nsim <- 10000 # 10,000 개 random number를 generation
x <- rnorm(nsim)
x3 <- x^3
mean(x3)
sd(x3)

help(mean)

Get_Trajectory <- function(){
  n_current_case <- 10
  n_case_trajectory <- rep(0,11)
  n_case_trajectory[1] <- 10
  for(i in 1:10){
    n_contact<-rpois(n_current_case,10)
    n_new_case<-rbinom(n_current_case,n_contact, 0.2)
    n_current_case<-sum(n_new_case)
    n_case_trajectory[i+1]<-n_current_case
  }
  return(n_case_trajectory)
}
nsim<-100 # 이 실험을 100번 하는 것것
out.all <- NULL
for(i in 1:nsim){
  out.all<-rbind(out.all, Get_Trajectory()) 
}
boxplot(out.all)

mean(out.all[,11])
sd(out.all[,11])
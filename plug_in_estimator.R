# getwd() ; R에서 디렉토리 확인

df = read.table("./DATA/coris.txt", sep=",", header = TRUE)
df[1:2,]

# Change chd as a factor
df$chd = factor(df$chd)
table(df$chd)

par(mfrow=c(1,2))
hist(df$ldl, main = "Histogram of LDL")
boxplot(df$ldl ~ df$chd, main = "LDL by CHD")

par(mfrow=c(1,1))

# Get a little bit fancier plots using ggplot
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")
library(ggplot2)
library(gridExtra)
library(dplyr)

# Violin plot
p1 <- ggplot(df, aes(x=ldl, color=chd)) + geom_histogram(fill="white")
p2 <- ggplot(df, aes(x=chd, y=ldl, color=chd)) + geom_violin() + geom_boxplot(width=0.1)

grid.arrange(p1, p2, nrow = 1)

eCDF_0 <- ecdf(df$ldl[df$chd==0])
eCDF_1 <- ecdf(df$ldl[df$chd==1])

x_range <- seq(min(df$ldl), max(df$ldl), length.out=1000)
plot(x_range, eCDF_0(x_range))
points(x_range, eCDF_1(x_range), col="red")
legend("topleft", legend=c("CHD=0", "CHD=1"), col=c("black", "red"), pch=1)

n0<-length(which(df$chd == 0))
n1<-length(which(df$chd == 1))

mu_0 = mean(df$ldl[df$chd == 0])
mu_1 = mean(df$ldl[df$chd == 1])
se_0 = sd(df$ldl[df$chd==0]) / sqrt(n0)
se_1 = sd(df$ldl[df$chd==1]) / sqrt(n1)

mu_0
mu_1
se_0
se_1

# Plug in estimator

T = mu_1 - mu_0
T_se = sqrt(se_0^2 + se_1^2)
T
T_se

# 95% CI
c(T-1.96*T_se, T+1.96*T_se)

# Bootstrap
LDL_1 = df$ldl[df$chd==1]
T_Median = median(LDL_1)
B<- 1000
B_Median<-rep(0,B)
for(i in 1:B){
  B_sample<-sample(LDL_1, replace=TRUE)
  B_Median[i]<-median(B_sample)
}
hist(B_Median)

# Now we get various Bootstrap confidence intervals
Get_CI <- function(T, B_sample){
  # Percentile Intervals
  CI_Percent = quantile(B_sample, probs=c(0.025, 0.975))
  # Normal Intervals
  se_b = sd(B_sample)
  CI_Normal = c(T-1.96*se_b, T+1.96*se_b)
  # Pivot Intervals
  CI_Pivot = c(2*T - CI_Percent[2], 2*T - CI_Percent[1])
  
  names(CI_Percent) <- c("2.5%", "97.5%")
  names(CI_Normal) <- c("2.5%", "97.5%")
  names(CI_Pivot) <- c("2.5%", "97.5%")
  
  re<-list(CT_Percent=CI_Percent, CI_Normal=CI_Normal, CI_Pivot=CI_Pivot)
  return(re)
}
Get_CI(T_Median, B_Median) 

# Difference가 estimator라면 
# Now we are interested in checking whether LDL levels are different by CHD groups.

n<-nrow(df)
T_MeanDiff = mean(df$ldl[df$chd==1])-mean(df$ldl[df$chd==0])
# mean(df$ldl[df$chd==1]) ; mu1
# mean(df$ldl[df$chd==0]) ; mu0
T_MeanDiff

B<-1000
B_MeanDiff<-rep(0,B)
for(i in 1:B){
  B_sample_idx<-sample(1:n, replace=TRUE)
  df1<-df[B_sample_idx,]
  mean1<-mean(df1$ldl[df1$chd==1])
  mean0<-mean(df1$ldl[df1$chd==0])
  
  B_MeanDiff[i] <-mean1-mean0
}
hist(B_MeanDiff)
Get_CI(T_MeanDiff, B_MeanDiff)

plot(df$ldl, df$sbp)
T_Corr<-cor(df$ldl, df$sbp)
T_Corr

n<-nrow(df)
B<-1000
B_Corr<-rep(0,B)
for(i in 1:B){
  B_sample_idx<-sample(1:n, replace=TRUE)
  df1<-df[B_sample_idx,]
  B_Corr[i]<-cor(df1$ldl, df1$sbp)
}
hist(B_Corr)

Get_CI(T_Corr, B_Corr)

install.packages("boot")
library(boot)
CorrFunc <- function(d, indice, formula, resid, fit){
  df1 = d[indice,]
  B_Corr<-cor(df1$ldl, df1$sbp)
  return(B_Corr)
}
boot.sample<-boot(df, CorrFunc, R=1000)
boot.sample
# d가 sample size?, indice가 bootstrap의 index

boot.ci(boot.sample)

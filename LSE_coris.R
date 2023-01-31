# Least Square Estimation(Coris)
df = read.table("coris.txt", sep = ",", header=TRUE)

Y<-df$ldl
X<-df$obesity

beta_1 = sum((X-mean(X))*(Y-mean(Y)))/sum((X-mean(X))^2)
beta_0 = mean(Y)-beta_1*mean(X)

plot(Y~X, main="LDL~Obesity")
abline(beta_0, beta_1)

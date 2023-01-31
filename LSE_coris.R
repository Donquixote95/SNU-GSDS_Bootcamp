# Least Square Estimation(Coris)
df = read.table("coris.txt", sep = ",", header=TRUE)

Y<-df$ldl
X<-df$obesity

beta_1 = sum((X-mean(X))*(Y-mean(Y)))/sum((X-mean(X))^2)
beta_0 = mean(Y)-beta_1*mean(X)

plot(Y~X, main="LDL~Obesity")
abline(beta_0, beta_1)


n<-length(X)
fitted_value<-beta_0 + beta_1*X
resid<-Y-fitted_value

par(mfrow=c(1,2))
hist(resid, main="histogram of residuals")
plot(resid~fitted_value, main="fitted vlaue vs resid")

SST = sum((Y-mean(Y))^2)
SSE = sum((resid)^2)
SSR = sum((fitted_value-mean(Y))^2)

R2_t1 = SSR/SST
R2_t2 = 1-(SSE/SST)

SST
SSR
SSE
SSE+SSR == SST
R2_t2
R2_t1


# CI and P-value calculation(Coris)
var_hat = sum(resid^2)/(n-2)
Sx_sq= mean((X-mean(X))^2)
SE_beta_0 = sqrt(var_hat/(n*Sx_sq)*mean(X^2))
SE_beta_1 = sqrt(var_hat/(n*Sx_sq))

W0 = beta_0/SE_beta_0
W1 = beta_1/SE_beta_1

# 95% CI
beta_0_CI<-c(beta_0-1.96*SE_beta_0, beta_0+1.96*SE_beta_0)
beta_1_CI<-c(beta_1-1.96*SE_beta_1, beta_1+1.96*SE_beta_1)

# p-value, 카이제곱분포로 구하는 것
pval_beta_0 = pchisq(W0^2, df=1, lower.tail = FALSE)
pval_beta_1 = pchisq(W1^2, df=1, lower.tail = FALSE)

# lm fucntion in R
lm.out<-lm(Y~X)
summary(lm.out)

beta_0

par(mfrow=c(2,2))
plot(lm.out)
par(mfrow=c(1,1))


head(df)
# Y = ldl
# Y = beta_0 + beta_1*age + ... + beta_5*obesity + error
# Do predictors explain the ldl level?
# How much the ldl level would increase if the level of adiposity increases one unit?
# Which predictor is most significantly associated with ldl?
# Is obesity significantly associated with ldl?

lm.out<-lm(ldl ~ age + tobacco + alcohol + adiposity + obesity, data=df)
summary(lm.out)
 # R-squared를 보면 model이 얼마나 잘 설명하는지 알 수 있다.

# If adiposity를 제거하면? 
lm.out<-lm(ldl ~ age + tobacco + alcohol + obesity, data=df)
summary(lm.out)
# 이전과 다른 이유는 adiposity와 obesity가 dependent하기 때문이다. 
# 둘 중 하나를 제거하는 게 더 낫다.

par(mfrow=c(2,2))
plot(lm.out)


#Logistic regression
glm.out <- glm(chd~age+sbp+tobacco+ldl+adiposity+famhist+obesity+alcohol,
               family = binomial, data=df)
summary(glm.out)

# Predicted logti and prob.
logit.predict<-predict(glm.out)
prob.predict<-exp(logit.predict)/(1+exp(logit.predict))

col<-c("blue","red")
par(mfrow=c(1,3))
boxplot(logit.predict~df$chd, xlab="CHD")
boxplot(prob.predict~df$chd, xlab="CHD")
plot(logit.predict, prob.predict, ylim=c(-0.1, 1.1), col=col[df$chd+1])

df = read.table("coris.txt", sep=",", header=TRUE)

# Wald test

sbp0 = df$sbp[df$chd==0]
sbp1 = df$sbp[df$chd==1]

n0 = length(sbp0)
n1 = length(sbp1)
Mu0 = mean(sbp0)
Mu1 = mean(sbp1)
Var0 = sum((sbp0-Mu0)^2)/n0
Var1 = sum((sbp1-Mu1)^2)/n1

Theta = Mu1 - Mu0
SE = sqrt(Var0/n0 + Var1/n1)
W = Theta/SE
Pvalue = pchisq(W^2, df=1, lower.tail=FALSE)
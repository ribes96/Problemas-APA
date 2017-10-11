X1 = c(1,3,3,5,5,6,8,9)
X2 = c(2,3,5,4,6,5,4,8)

X = cbind(X1,X2)
X
S = cov(X)
S
eigen(S)

plot(X1,X2,type = 'lp')

f = matrix(1:5, ncol = 1)
l = matrix(c(7.97, 10.2, 14.2, 16.0, 21.2), ncol = 1)

#df = data.frame(f, l)

#model = lm(formula = l ~  f,  data = df)

#phi.0 = c(rep(model$coefficients[1],5))
#phi.1 = model$coefficients[2] * f

phi.0 = function(x) return(1)
phi.1 = function(x) return(x)

PHI = cbind(apply(f, MARGIN = 1, FUN = phi.0), apply(f, MARGIN = 1, FUN = phi.1))

(design = t(PHI) %*% PHI)


library(MASS)


# w = ginv(t(PHI) %*% PHI) %*% t(PHI) %*% l

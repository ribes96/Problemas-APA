library(MASS)
library(nnet)

f = function(x) {
  return(4*sin(pi*x[1]) + 2*cos(pi*x[2]) + rnorm(1,0,0.5**2))
}

runf = function(x) {
  return(apply(x,1,f))
}

gendata = function() {
  data1 = cbind(runif(100,-1,1),runif(100,-1,1))
  data2 = cbind(runif(200,-1,1),runif(200,-1,1))
  data3 = cbind(runif(500,-1,1),runif(500,-1,1))
  data4 = cbind(runif(1000,-1,1),runif(1000,-1,1))
  
  result1 = runf(data1)
  result2 = runf(data2)
  result3 = runf(data3)
  result4 = runf(data4)
  
  nm = c("x1","x2","target")
  
  df1 <<- as.data.frame(cbind(data1, result1))
  df2 <<- as.data.frame(cbind(data2, result2))
  df3 <<- as.data.frame(cbind(data3, result3))
  df4 <<- as.data.frame(cbind(data4, result4))
  
  names(df1) <<- nm
  names(df2) <<- nm
  names(df3) <<- nm
  names(df4) <<- nm
}

gendata()

N = nrow(df1)

learn <- sample(1:N, round(2*N/3))
nlearn = length(learn)

model.nnet = nnet(target ~ . , data = df1, linout = TRUE, subset = learn, size = 5, maxit = 500)
p1 = predict(model.nnet, df1, type = "raw")
t1 = table(p1, df1$target)

error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

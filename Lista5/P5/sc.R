library(MASS)
library(nnet)
library(caret)
library(stargazer)

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
  
  test_data = cbind(runif(1024,-1,1),runif(1024,-1,1))
  
  result1 = runf(data1)
  result2 = runf(data2)
  result3 = runf(data3)
  result4 = runf(data4)
  
  test_result = runf(test_data)
  
  nm = c("x1","x2","target")
  
  df1 <<- as.data.frame(cbind(data1, result1))
  df2 <<- as.data.frame(cbind(data2, result2))
  df3 <<- as.data.frame(cbind(data3, result3))
  df4 <<- as.data.frame(cbind(data4, result4))
  
  test <<- as.data.frame(cbind(test_data, test_result))
  
  names(df1) <<- nm
  names(df2) <<- nm
  names(df3) <<- nm
  names(df4) <<- nm
  names(test) <<- nm
}

gendata()

trc <- trainControl (method="repeatedcv", number=10, repeats=5)

model1.nnet <- train (
  target ~.,
  data = df1,
  linout = TRUE,
  method='nnet',
  metric = "RMSE",
  trControl=trc)

model2.nnet <- train (
  target ~.,
  data = df2,
  linout = TRUE,
  method='nnet',
  metric = "RMSE",
  trControl=trc)

model3.nnet <- train (
  target ~.,
  data = df3,
  linout = TRUE,
  method='nnet',
  metric = "RMSE",
  trControl=trc)

model4.nnet <- train (
  target ~.,
  data = df4,
  linout = TRUE,
  method='nnet',
  metric = "RMSE",
  trControl=trc)

model1.linear = train (
  target ~.,
  data = df1,
  method='lm',
  metric = "RMSE",
  trControl=trc)

model2.linear = train (
  target ~.,
  data = df2,
  method='lm',
  metric = "RMSE",
  trControl=trc)

model3.linear = train (
  target ~.,
  data = df3,
  method='lm',
  metric = "RMSE",
  trControl=trc)

model4.linear = train (
  target ~.,
  data = df4,
  method='lm',
  metric = "RMSE",
  trControl=trc)





p1.nnet = predict(model1.nnet, test)
p2.nnet = predict(model2.nnet, test)
p3.nnet = predict(model3.nnet, test)
p4.nnet = predict(model4.nnet, test)

p1.linear = predict(model1.linear, test)
p2.linear = predict(model2.linear, test)
p3.linear = predict(model3.linear, test)
p4.linear = predict(model4.linear, test)



r1.nnet = postResample(p1.nnet, test$target)
r2.nnet = postResample(p2.nnet, test$target)
r3.nnet = postResample(p3.nnet, test$target)
r4.nnet = postResample(p4.nnet, test$target)

r1.linear = postResample(p1.linear, test$target)
r2.linear = postResample(p2.linear, test$target)
r3.linear = postResample(p3.linear, test$target)
r4.linear = postResample(p4.linear, test$target)



r = rbind(
  r1.nnet,
  r2.nnet,
  r3.nnet,
  r4.nnet,
  r1.linear,
  r2.linear,
  r3.linear,
  r4.linear
  )

stargazer(r)

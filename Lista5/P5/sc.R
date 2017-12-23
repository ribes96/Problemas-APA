library(MASS)
library(nnet)
library(caret)

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


#N = nrow(df1)

#learn <- sample(1:N, round(2*N/3))

trc <- trainControl (method="repeatedcv", number=10, repeats=5)
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
model1 <- train (
  target ~.,
  data = df1,
  linout = TRUE,
  method='nnet',
  metric = "RMSE",
  tuneGrid = nnetGrid,
  trControl=trc)

model2 <- train (
  target ~.,
  data = df2,
  linout = TRUE,
  method='nnet',
  metric = "RMSE",
  tuneGrid = nnetGrid,
  trControl=trc)
model3 <- train (
  target ~.,
  data = df3,
  linout = TRUE,
  method='nnet',
  metric = "RMSE",
  tuneGrid = nnetGrid,
  trControl=trc)

model4 <- train (
  target ~.,
  data = df4,
  linout = TRUE,
  method='nnet',
  metric = "RMSE",
  tuneGrid = nnetGrid,
  trControl=trc)

p1 = predict(model1, test)
p2 = predict(model2, test)
p3 = predict(model3, test)
p4 = predict(model4, test)

r1 = postResample(p1, test$target)
r2 = postResample(p2, test$target)
r3 = postResample(p3, test$target)
r4 = postResample(p4, test$target)

r1 = c(r1, model1$bestTune)
r2 = c(r2, model2$bestTune)
r3 = c(r3, model3$bestTune)
r4 = c(r4, model4$bestTune)

r = rbind(r1,r2,r3,r4)

#model.nnet = nnet(target ~ . , data = df1, linout = TRUE, subset = learn, size = 5, maxit = 500)
#p1 = predict(model.nnet, df1, type = "raw")
t1 = table(p1, df1$target)

error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

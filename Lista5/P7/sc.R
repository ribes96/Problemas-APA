library(nnet)
library(caret)

mydata = read.table("letters.txt")

corrupt = function(bit_vector, nchanges) {
  changes = sample(length(bit_vector), nchanges)
  g = rep(0, length(bit_vector))
  for (n in changes) {
    g[n] = 1
  }
  new = as.numeric(xor(bit_vector, g))
  return(new)
}

generate_corrupt = function(df, n) {
  nm = c(1:35, "letter")
  g = sample(1:nrow(df), n, replace = TRUE)
  ch = rpois(n, 1.01)
  new_data = data.frame()
  for (i in 1:length(g)) {
    elem = g[i]
    new_vector = corrupt(df[elem, -36], ch[i])
    newrow = c(new_vector, df[elem, 36])
    new_data = rbind(new_data, newrow)
    new_data[i,36] = as.character(df[elem, 36])
  }
  colnames(new_data) = nm
  new_data$letter = as.factor(new_data$letter)
  return(new_data)
}

train_data = generate_corrupt(mydata, 1000)
test_data = generate_corrupt(mydata, 300)

trc <- trainControl (method="repeatedcv", number=10, repeats=5)
## WARNING: this takes maaaaaany minutes
caret.nnet.model <- train (
  letter ~.,
  data = train_data,
  method='nnet',
  metric = "Accuracy",
  trControl=trc)

test_results = predict(caret.nnet.model, test_data)

confusionMatrix(test_data$letter, test_results)



#Not useful

model.nnew = nnet(letter ~ ., data = let_data, size = 10, maxit = 500)

results = predict(model.nnew, newdata = let_data, type = "class")

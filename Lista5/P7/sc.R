library(nnet)


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
  return(new_data)
}

let_data = generate_corrupt(mydata, 50)


# Hay que hacer algo con la variable de respuesta: debería ser factor, o algo parecido
model.nnew = nnet(letter ~ ., data = let_data, size = 10, maxit = 500, linout = FALSE, softmax = TRUE)

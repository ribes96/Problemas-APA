energy = c(
2.899,
3.484 ,
3.984 ,
4.444 ,
4.831 ,
5.376 ,
6.211 ,
7.576 ,
11.905 ,
16.667 
)

st = c(
   367 ,
   311 ,
   295 ,
   268 ,
   253 ,
   239 ,
   220 ,
   213 ,
   193 ,
   192
)

sd = c(
   17,
   9,
   9,
   7,
   7,
   6,
   6,
   6,
   5,
   5
)

energy.inv = 1/energy
d = matrix(c(st, energy.inv), ncol = 2)
d.df = as.data.frame(d)
var = sd^2
w = 1 / var
l.heter = lm(formula = V2 ~ V1, weights = w, data = d.df)
l.homo = lm(formula = V2 ~ V1, data = d.df)
# V2 is energy, V1 is st

plot(st,energy.inv)
abline(l.heter$coefficients[1], l.heter$coefficients[2], col = "red")
abline(l.homo$coefficients[1], l.homo$coefficients[2], col = "blue")

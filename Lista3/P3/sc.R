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
var = sd^2
w = 1 / var

df = data.frame(energy.inv, st)

l.heter = lm(formula = st ~ energy.inv, weights = w, data = df)
l.homo = lm(formula = st ~ energy.inv, data = df)



plot(energy.inv,st, ylab = "Secció transversal", xlab = "Inversa de la energia")
legend('topleft', c("Asumiendo heterocedasticidad","Asumiendo homocedasticidad") , lty=1, col=c('red', 'blue'), bty='L', cex=.75)
abline(l.heter$coefficients[1], l.heter$coefficients[2], col = "red")
abline(l.homo$coefficients[1], l.homo$coefficients[2], col = "blue")


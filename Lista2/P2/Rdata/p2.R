N <- 8
X <- c(1, 3, 3, 5, 5, 6, 8, 9)
Y <- c(2, 3, 5, 4, 6, 5, 7, 8)

X <- (X - mean(X)) / sd(X)
Y <- (Y - mean(Y)) / sd(Y)
M <- cbind(X, Y)

Sigma <- cov(M)

ev <- eigen(Sigma)
eigenvalues <- ev$values
eigenvectors <- ev$vectors

PCA <- prcomp(M)

PCA.PC1 <- PCA$x[,1]
PCA.slope1 <- PCA$rotation[2,1] / PCA$rotation[1,1]

PCA.PC2 <- PCA$x[,2]
PCA.slope2 <- PCA$rotation[2,2] / PCA$rotation[1,2]

plot(M, col = 'red', ann = FALSE)
abline(0, PCA.slope1, col = 'green')
abline(0, PCA.slope2, col = 'blue')
grid()


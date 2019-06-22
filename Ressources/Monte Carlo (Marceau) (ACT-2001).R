# Cours : Act-2001
# Semestre : H2019
# Date : 2019-02-15
#
# Methode MC
# 
# Objectif : Examine la somme de S = X1+X2
# Somme de 2 v.a. indépendantes X1 et X2
#
# Loi de X1: Gamma
aa <- 2
bb <- 1 / 5
EX1 <- aa / bb
# Loi de X2: LogNormale
mu <- log(10) - 0.32
sig <- 0.8
EX2 <- exp(mu + (sig ^ 2) / 2)
#
#
ES <- EX1 + EX2
#
#
# Calculs no1
nsim <- 5
set.seed(2019)
#
matU <- matrix(runif(nsim * 2), nsim, 2, byrow = T)
X1 <- qgamma(matU[, 1], aa, bb)
X2 <- qlnorm(matU[, 2], mu, sig)
S <- X1 + X2
round(cbind(matU, X1, X2, S), 4)
#
#
# Calculs no2
#
nsim <- 10000
set.seed(2019)
#
matU <- matrix(runif(nsim * 2), nsim, 2, byrow = T)
X1 <- qgamma(matU[, 1], aa, bb)
X2 <- qlnorm(matU[, 2], mu, sig)
S <- X1 + X2
c(mean(X1), EX1)
c(mean(X2), EX2)
c(mean(S), ES)
c(max(S), max(X1), max(X2))
#
#
#
kap <- 0.3
VaRX1 <- quantile(X1, kap, type = 1)
VaRX2 <- quantile(X2, kap, type = 1)
VaRS <- quantile(S, kap, type = 1)
#
TVaRX1 <- sum(X1 * (X1 > VaRX1)) / (nsim * (1 - kap))
TVaRX2 <- sum(X2 * (X2 > VaRX2)) / (nsim * (1 - kap))
TVaRS <- sum(S * (S > VaRS)) / (nsim * (1 - kap))
#
round(c(kap, VaRX1, VaRX2, VaRX1 + VaRX2, VaRS), 3)
round(c(kap, TVaRX1, TVaRX2, TVaRX1 + TVaRX2, TVaRS), 3)
#
#
kap <- 0.99
VaRX1 <- quantile(X1, kap, type = 1)
VaRX2 <- quantile(X2, kap, type = 1)
VaRS <- quantile(S, kap, type = 1)
#
TVaRX1 <- sum(X1 * (X1 > VaRX1)) / (nsim * (1 - kap))
TVaRX2 <- sum(X2 * (X2 > VaRX2)) / (nsim * (1 - kap))
TVaRS <- sum(S * (S > VaRS)) / (nsim * (1 - kap))
#
round(c(kap, VaRX1, VaRX2, VaRX1 + VaRX2, VaRS), 3)
round(c(kap, TVaRX1, TVaRX2, TVaRX1 + TVaRX2, TVaRS), 3)
#
#
plot(
    c(0, sort(S)),
    (0:nsim) / nsim,
    type = "s",
    xlab = "x",
    ylab = "approx de FS(x)",
    main = "Methode MC : Approximation de FS(x) avec S = X1 + X2"
)
#
#




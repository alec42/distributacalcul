#####Fonction de répartition, Quantile, et TVaRX d'une F-généralisée
### Marceau

kappa <- 0.995
lambda <- (44000 / 43)
alpha <- (87 / 43)
tho <- 2

pFgen <- function(x, lambda, alpha, tho)
{
    pbeta(q = (x / (lambda + x)), 
          shape1 = tho, 
          shape2 = alpha)
}

qFgen <- function(x) {
    abs(pFgen(x, lambda, alpha, tho) - kappa)
}

qFgenTest <- function(x, kappa)
{
    if(kappa <= 0)
        0
    else
        optimize(qFgen, c(0, 30000))$minimum
}
qFgen(10)
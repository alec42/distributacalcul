# Processus Homogène Composée Poiss-Gamma
F_s <- function(x, t){
    dpois(0, lambda * t) + sum(sapply(1:k0, function(k) dpois(k, lambda * t) * pgamma(x, alpha * k, beta)))
}
VaR_s <- function(kappa , t){ 
    if(kappa <= dpois(0, lambda * t))
       return (0)
    uniroot(function(x) F_s(x, t) - kappa, c(0,10000))$root
}
TvaR_S <- function(kappa, t){
    sum(sapply(1:k0, function(k) dpois(k, 1.8 * t) * alpha * k / beta * (1 - pgamma(VaR_s(kappa, t), (alpha*k)+1, beta)))) / (1 - kappa )
}
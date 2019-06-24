### Code pour composées

##### BNComp ####

### Gamma

Fx <- function(x, r, q, k0, alpha, beta) 
{
    dnbinom(x = 0, size = r, prob = q) + sum(sapply(1:k0, function(i) dnbinom(x = i, size = r, prob = q) * pgamma(q = x, shape = alpha * i, rate = beta)))
}

VaR <- function(k, upper = 1e3)
{
    if(k <= Fx(0))
        0
    else
        optimize(function(i) abs(Fx(i) - k), c(0, upper))$minimum
}

TVaR <- function(x, vark){
    
    if (vark == 0)
    {
        (alpha / beta * r * (1 - q) / q) / (1 - x)
    }
    else
    {
        (sum(sapply(1:k0, function(i) dnbinom(x = i, size = r, prob = q) * (alpha / beta) * pgamma(q = vark, shape = alpha * i + 1, rate = beta, lower.tail = F))) / (1 - x))
    }
}

## Sn

FS <- function(x, n){
    dnbinom(x = 0, size = r * n, prob = q) + sum(sapply(1:ko, function(i) dnbinom(x = i, size = r * n, prob = q) * pgamma(q = x, shape = alpha * i, rate = beta)))
}

VaR <- function(k, upper = 1e4, n = 1) optimise(function(i) abs(FS(i, n) - k), c(0, upper))$minimum

tvarS <- function(x, k) (sum(sapply(1:ko, function(i) dnbinom(x = i, size = r * n, prob = q) * (alpha * i)/beta * pgamma(q = x, shape = alpha * i + 1, rate = beta, lower.tail = F)))/(1 - k))

## Wn

Fw <- function(x, n) 
{
    dnbinom(x = 0, size = r * n, prob = q) + sum(sapply(1:k0, function(i) dnbinom(x = i, size = r * n, prob = q) * pgamma(q = x * n, shape = alpha * i, rate = beta)))
}



#### PComp ####

### Gamma

Fx <- function(x, lambda, alpha, beta, ko = 1e3)
{
    dpois(x = 0, lambda = lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda = lambda) * pgamma(q = x, shape = alpha * k, rate = beta)))
}

EXtronq <- function(x, lam, alp, bet, k0 = 1000)
{
    f0 <- dpois(0, lam)
    vk <- 1:1000
    fk <- dpois(vk, lambda)
    EXt <- sum(fk * (vk * alp) / bet * (1 - pgamma(x, vk * alp + 1, bet)))
    return(EXt)
}

vark <- function(k, upper = 1e3) 
{
    if(k <= Fx(0))
        0
    else
        optimise(function(x) abs(Fx(x) - k), c(0, upper))$minimum
}

tvark <- function(x, var)
{
    sum(sapply(1:ko, function(k) dpois(x = k, lambda = lambda) * (alpha * k )/beta *pgamma(q = var, shape = alpha * k + 1, rate = beta, lower.tail = F)))/(1 - x)
}


#### BComp ####

### Gamma

## Sn
# 4.2.4 
Fs <- function(x)
{
    dbinom(x = 0, size = 6, prob = .5) + sum(sapply(1:ko, function(i) dbinom(x = i, size = 6, prob = 0.5) * pgamma(q = x, shape = alpha * i, rate = beta)))
}

Vark <- function(k, upper = 1e2) 
{
    if(k <= Fs(0))
        0
    else
        optimise(function(i) abs(Fs(i) - k), c(0, upper))$minimum
}

Tvark <- function(k, vark)
{
    sum(sapply(1:ko, function(i) dbinom(x = i, size = 6, prob = 0.5) * (alpha * i)/beta * pgamma(q = vark, shape = alpha * i + 1, rate = beta, lower.tail = F)))/(1 - k)
}

#### Dépannages ####
# Loi poisson compose avec sinistre loi gamma ----------------------------------------------------------------------

a <- 1.5
lam <- 2
b <- 1.5/100
n <- c(1,10,100,1000)
k0 <- 1000


(esperance_wn <- 2 * 1.5 / (1.5/100))
(var_wn <- (lambda*shape/(shape/100)^2 + lambda*(shape/(shape/100))^2)/n)



Fsn <- function(x, n){
    dpois(0, lam*n) + sum(sapply(1:1000, function(k) dpois(k, lam*n) * pgamma(x, a*k, b)))
}

Fwn <- function(x, n) Fsn(x*n, n)

sapply(n, function(n) Fsn(10, n))
sapply(n, function(n) Fwn(10, n))



kappa <- c(0.001, 0.01, 0.05, 0.5, 0.95, 0.99, 0.999)
VaR <- function(k, n){
    if(k <= dpois(0, lam*n))
        var <- 0
    else
        var <- optimize(function(i) abs(Fsn(i, n) - k), c(0, 10000))$minimum
    
    return(var)
}
sapply(kappa, function(k) sapply(n, function(n) VaR(k, n))) / n # calcul VaR_Wn (plusieurs kappa et n)


#### Loi poisson compose avec sinistre loi lognormale ####

u <- log(100) - 0.32
sig <- 0.8
lam <- 2
valeurs <- seq(from=0, to=2000, by=50)
kappa <- c(0.001, 0.01, 0.05, 0.5, 0.95, 0.99, 0.999)

# p.38 et 39 diapo #

esp_b <- exp(u + (sig^2)/2)
esp_m <- lam
(esp_x <- esp_b * esp_m)

var_b <- exp(2*u + sig^2) * (exp(sig^2) - 1)
var_m <- lam
(var_x <- esp_m * var_b + var_m * esp_b^2)

sqrt(var_x)

# p.40 diapo #

set.seed(2019)
m <- 1000000
X <- numeric(m)
M <- numeric(m)
U <- runif(m*qpois(0.99, lam))
j <- 1

for (i in 1:m){
    M[i] <- qpois(U[j], lam)
    
    if (M[i] > 0)
        X[i] <- sum(sapply(1:M[i], function(k) qlnorm(U[j + k], u, sig)))
    
    j <- j + 1 + M[i]
}

# p.41, 42, 43 et 44 diapo #

X[c(1,2,3,4,5,m)]
sort(X)[c(1, m/2, m)]

mean(X) ; var(X) ; sqrt(var(X))

# p.45, 46, 47, 48 et 49 diapo #

Fx <- function(x){
    mean(X<=x)
}
sapply(valeurs, function(x) Fx(x))

esp_tronq <- function(x){
    mean(X * (X > x))
}
sapply(valeurs, function(x) esp_tronq(x))

stop_loss <- function(x){
    mean(pmax(X-x,0))
}
sapply(valeurs, function(x) stop_loss(x))

var <- function(k){
    sort(X)[k*m]
}
sapply(kappa, function(x) var(x))

TVaR <- function(k){
    mean(X[X > var(k)])
}
sapply(kappa, function(x) TVaR(x))

# BNeg avec gamma ----------------------------------------------------------------------


r <- 1.5
q <- 3/7
a <- 1.5
b <- 1.5/100
nn <- c(1,10,100,1000)
#Esperance

E_M <- 1.5 * (1-q)/q
E_B <- a/b

(E_X <- E_M * E_B)

(E_w <- E_X)
#Variance
VAR_M <- 1.5 * (1-q)/q^2
VAR_B <- a/b^2

VAR_X <- E_M * VAR_B + VAR_M * E_B^2

(VAR_W <- VAR_X/nn)
#FS
f_M <-function(x,par){
    dnbinom(x,par[1],par[2])
}
F_B <- Vectorize(function(x,par1,par2){
    pgamma(x,par1,par2)
})


F_S<-function(x,f_M,F_B,n)
{
    f0<-f_M(0,c(n*r,q))
    k<-1:(600*n)
    ff_M<-f_M(k,c(n*r,q))
    
    f0+sum(ff_M*F_B(x,k*a,b))
}


#Esp tronqu?
E_tr_S<-function(d,f_M,n)
{
    k0 <- 600*n
    k<-1:k0
    ff_M<-f_M(k,c(n*r,q))
    sum(ff_M*k* a/b * (1-pgamma(d,k*a+1,b)))
}


#VaR
VaR_S <- Vectorize(function(u,n){
    if(u <= f_M(0,c(n*r,q))){
        0
    }else{
        optimise(function(x) abs(F_S(x,f_M,F_B,n)-u),c(0,2000*n), tol = .Machine$double.eps)$minimum 
    }
})
VaR_S_grandN_100 <- Vectorize(function(u,n){
    if(u <= f_M(0,c(n*r,q))){
        0
    }else{
        optimise(function(x) abs(F_S(x,f_M,F_B,n)-u),c(0,50000), tol = .Machine$double.eps)$minimum 
    }
})
VaR_S_grandN_1000 <- Vectorize(function(u,n){
    if(u <= f_M(0,c(n*r,q))){
        0
    }else{
        optimise(function(x) abs(F_S(x,f_M,F_B,n)-u),c(0,300000), tol = .Machine$double.eps)$minimum 
    }
})
kappa <- c(0.9,0.95,0.99,0.999)

(V_1_10 <- sapply(nn[1:2],function(n) VaR_S(kappa,n)/n))
(V_100 <- sapply(nn[3],function(n) VaR_S_grandN_100(kappa,n)/n))
(V_1000 <- sapply(nn[4],function(n) VaR_S_grandN_1000(kappa,n)/n)) 
#TVaR
TVaR_S <- Vectorize(function(kap,n){
    E_tr_S(VaR_S(kap,n),f_M,n)/ (1-kap)
})
TVaR_S_100 <- Vectorize(function(kap,n){
    E_tr_S(VaR_S_grandN_100(kap,n),f_M,n)/ (1-kap)
})
TVaR_S_1000 <- Vectorize(function(kap,n){
    E_tr_S(VaR_S_grandN_1000(kap,n),f_M,n)/ (1-kap)
})
(TV_1_10 <- sapply(nn[1:2],function(n) TVaR_S(kappa,n)/n))
(TV_100 <- sapply(nn[3],function(n) TVaR_S_100(kappa,n)/n))
(TV_1000 <- sapply(nn[4],function(n) TVaR_S_1000(kappa,n)/n))
# BNeg avec Log normal ----------------------------------------------------------------------


r <- 1.5
q <- 3/7
mu <- log(100)-0.32
sig <- 0.8

#Esperance

E_M <- 1.5 * (1-q)/q
E_B <- exp(mu + sig^2 /2)

E_X <- E_M * E_B
(E_W <- E_X)
#Variance
VAR_M <- 1.5 * (1-q)/q^2
VAR_B <- exp(2*mu + sig^2 )*(exp(sig^2)-1)

VAR_X <- E_M * VAR_B + VAR_M * E_B^2
(VAR_W <- VAR_X/nn)
#Simulation
y <- seq(0,2000,by=50)
m <- 1000000
sim_loi_comp_S <- function(m=1000000,qF_M,qF_B,n){
    set.seed(2019)
    X <- rep(0,m)
    for (i in 1:m){
        M <- qF_M(runif(1),r*n,q)
        if(M >0){
            X[i] <- sum(qF_B(runif(M),mu,sig))
        }else{
            X[i] <- M
        }
    }
    return(X)
}


S <- sim_loi_comp_S(m,qnbinom,qlnorm,1)
S_10 <- sim_loi_comp_S(m,qnbinom,qlnorm,10)
S_100 <- sim_loi_comp_S(m,qnbinom,qlnorm,100)
S_1000 <- sim_loi_comp_S(m,qnbinom,qlnorm,1000)
mean(S);mean(S_10)/10;mean(S_100)/100;mean(S_1000)/1000
var(S);var(S_10/10);var(S_100/100);var(S_1000/1000)

VaR_sim <- function(k,SS){
    sort(SS)[k*m]
}
V_sim <- sapply(kappa,VaR_sim,SS=S)
V_sim_10 <- sapply(kappa,VaR_sim,SS=S_10)
V_sim_100 <- sapply(kappa,VaR_sim,SS=S_100)
V_sim_1000 <- sapply(kappa,VaR_sim,SS=S_1000)
cbind(kappa,V_sim,V_sim_10/10,V_sim_100/100,V_sim_1000/1000)


TVaR_sim <- function(k,SS){
    mean(SS[SS > VaR_sim(k,SS)])
}
TV_sim <- sapply(kappa,TVaR_sim,SS=S)
TV_sim_10 <- sapply(kappa,TVaR_sim,SS=S_10)
TV_sim_100 <- sapply(kappa,TVaR_sim,SS=S_100)
TV_sim_1000 <- sapply(kappa,TVaR_sim,SS=S_1000)
cbind(kappa,TV_sim,TV_sim_10/10,TV_sim_100/100,TV_sim_1000/1000)


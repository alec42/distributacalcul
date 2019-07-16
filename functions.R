#### Burr ####

Mexcess_burr <- function(lam, alpha, tau, d) {
    (((lam + d ^ tau) ^ alpha) *
         gamma(1 + 1 / tau) *
         gamma(alpha - 1 / tau)) /
        ((lam ^ (alpha - 1 / tau)) *
             gamma(alpha)) *
        pbeta(
            q = (d ^ tau) / (lam + (d ^ tau)),
            shape1 = 1 + 1 / tau,
            shape2 = alpha - 1 / tau,
            lower.tail = F) - d
}

Elim_burr <- function(d, alpha, lam, tau) {
    1/gamma(alpha) * 
        lam^(1/tau) * 
        gamma(1 + 1/tau) * 
        gamma(alpha - 1/tau) * 
        pbeta(q = d^tau / (lam + d^tau), 
              shape1 = 1 + 1/tau, 
              shape2 = alpha - 1/tau) + 
        d * 
        (lam / (lam + d^tau)) ^ alpha
}

SL_burr <- function(d, alpha, lam, tau) {
    1/(gamma(alpha)) * 
        (lam^(1/tau)) * 
        gamma(1 + 1/tau) * 
        gamma(alpha - 1/tau) * 
        pbeta(q = (d^tau / (lam + (d^tau))), 
              shape1 = 1 + 1/tau, 
              shape2 = alpha - 1/tau, 
              lower.tail = F) - 
        d * 
        (lam / (lam + d^tau)) ^ alpha
}

Etronq_burr <- function(d, alpha, lam, tau) {
    1/(gamma(alpha)) * 
        (lam^(1/tau)) * 
        gamma(1 + 1/tau) * 
        gamma(alpha - 1/tau) * 
        pbeta(q = (d^tau / (lam + (d^tau))), 
              shape1 = 1 + 1/tau, 
              shape2 = alpha - 1/tau)
}

#-tau < k < alpha * tau
kthmoment_burr <- function(k, alpha, lam, tau)
{
    (1/gamma(alpha)) * 
        lam^(k/tau) * 
        gamma(1 + k/tau) * 
        gamma(alpha - k/tau)
}

TVaR_burr <- function(k, var, alpha, lam, tau) {
    1/((1 - k) * gamma(alpha)) * 
        (
            (lam^(1 / tau)) *
                gamma(1 + 1 / tau) *
                gamma(alpha - 1 / tau) *
                pbeta(q = (var^tau) / (lam + var^tau),
                      shape1 = 1 + 1 / tau,
                      shape2 = alpha - 1 / tau,
                      lower.tail = F)
        )
    
}

VaR_burr <- function(k, alpha, lam, tau) {
    (lam * ((1 - k)^(-1/alpha) - 1))^(1/tau)
}


#### Weibull ####

Mexcess_weibull <- function(d, tau, beta)
{
    exp((beta * d)^tau) / beta * 
        gamma(1 + 1/tau) * 
        pgamma(q = d^tau,
               shape = 1 + 1/tau,
               scale = beta^tau,
               lower.tail = F) - 
        d
}

Elim_weibull <- function(d, tau, beta)
{
    1 / beta * 
        gamma(1 + 1/tau) * 
        pgamma(q = d^tau,
               shape = 1 + 1/tau,
               scale = beta^tau) +
        d * exp(-(beta * d)^tau)
}

SL_weibull <- function(d, tau, beta)
{
    1 / beta * 
        gamma(1 + 1/tau) * 
        pgamma(q = d^tau,
               shape = 1 + 1/tau,
               scale = beta^tau,
               lower.tail = F) -
        d * exp(-(beta * d)^tau)
}

Etronq_weibull <- function(d, tau, beta)
{
    1 / beta * 
        gamma(1 + 1/tau) * 
        pgamma(q = d^tau,
               shape = 1 + 1/tau,
               scale = beta^tau)
}

TVaR_weibull <- function(k, tau, beta)
{
    1 / (beta * (1 - k)) * 
        gamma(1 + 1/tau) * 
        pgamma(q = -log(1 - k),
               shape = 1 + 1/tau,
               scale = 1,
               lower.tail = F)
}

VaR_weibull <- function(k, tau, beta)
{
    1 / beta * (-log(1 - k))^(1/tau)
}

E_weibull <- function(tau, beta, k = 1)
{
    1/(beta^k) * gamma(1 + k/tau)
}

V_weibull <- function(tau, beta)
{
    E_weibull(tau = tau, beta = beta, k = 2) - (E_weibull(tau = tau, beta = beta, k = 1))^2
}

#### Lognormale ####

kthmoment_lnorm <- function(k, mu, sig)
{
    exp(mu * k + k ^ 2 * (sig ^ 2) / 2)
}

#### Beta ####

kthmoment_beta <- function(k, a, b)
{
    (gamma(a + k) * gamma(a + b))/(gamma(a) * gamma(a + b + k))
}

#### Erlang ####

E_erlang <- function(n, b)
{
    n/b
}

V_erlang <- function(n, b)
{
    n/b^2
}

kthmoment_erlang <- function(k, n, b)
{
    prod(sapply(0:(k - 1), function(i) (n + i)))/(b^k)
}

Etronq_erlang <- function(d, n, b)
{
    n/b * (1 - exp(-b * d) * sum(sapply(0:n, function(j) ((b * d)^j)/factorial(j) )))
}

TVaR_erlang <- function(vark, k, n, b)
{
    (n / ((1 - k) * b)) * (exp(-b * vark) * sum(sapply(0:n, function(j) ((b * vark)^j)/factorial(j) )))
}

SL_erlang <- function(d, n, b)
{
    (n/b) * pgamma(q = d, shape = n + 1, rate = b, lower.tail = F) - d * pgamma(q = d, shape = n, rate = b, lower.tail = F)
}

Elim_erlang <- function(d, n, b)
{
    (n/b) * pgamma(q = d, shape = n + 1, rate = b) + d * pgamma(q = d, shape = n, rate = b, lower.tail = F)
}

Mexcess_erlang <- function(d, n, b)
{
    (n / b) * pgamma(q = d, shape = n + 1, rate = b, lower.tail = F)/pgamma(q = d, shape = n, rate = b, lower.tail = F) - d
}

perlang <- function(x, n, b, lower.tail = T)
{
    if(lower.tail == T)
        (1 - exp(-b * x) * sum(sapply(0:(n - 1), function(j) ((b * x)^j)/factorial(j))))
    else
        exp(-b * x) * sum(sapply(0:(n - 1), function(j) ((b * x)^j)/factorial(j)))
}

derlang <- function(x, n, b)
{
    ((b ^ n) / gamma(n)) * (x^(n - 1)) * exp(-b * x)
}

#### Log-logistique ####

kthmoment_llogis <- function(k = 1, lam, tau)
{
    lam^k * gamma(1 + k/tau) * gamma(1 - k/tau)
}

V_llogis <- function(lam, tau)
{
    kthmoment_llogis(k = 2,
                     lam = lam,
                     tau = tau) - 
        kthmoment_llogis(k = 1,
                         lam = lam,
                         tau = tau)^2
}

VaR_llogis <- function(k, lam, tau)
{
    lam * (k^(-1) - 1)^(-1/tau)
}

TVaR_llogis <- function(k, lam, tau)
{
    lam / (1 - k) * 
        gamma(1 + 1/tau) * 
        gamma(1 - 1/tau) * 
        pbeta(q = k,
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau,
              lower.tail = F)
}

SL_llogis <- function(d, lam, tau)
{
    lam * 
        gamma(1 + 1/tau) * 
        gamma(1 - 1/tau) * 
        pbeta(q = (d^tau)/(lam^tau + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau,
              lower.tail = F) -
        (d * (lam^tau)) / 
        (lam^tau + d^tau)
}

Mexcess_llogis <- function(d, lam, tau)
{
    (d^tau + lam^tau) / (lam^(tau - 1)) * 
        gamma(1 + 1/tau) * 
        gamma(1 - 1/tau) * 
        pbeta(q = (d^tau)/(lam^tau + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau,
              lower.tail = F) -
        d
    
}

Elim_llogis <- function(d, lam, tau)
{
    lam * 
        gamma(1 + 1/tau) * 
        gamma(1 - 1/tau) * 
        pbeta(q = (d^tau)/(lam^tau + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau) +
        (d * (lam^tau)) / 
        (lam^tau + d^tau)
}

Etronq_llogis <- function(d, lam, tau)
{
    lam * 
        gamma(1 + 1/tau) * 
        gamma(1 - 1/tau) * 
        pbeta(q = (d^tau)/(lam^tau + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau)
}







#### Hypergéometrique ####

E_hyper <- function(N, m, n)
{
    n * (m / N)
}

V_hyper <- function(N, m, n)
{
    (n * (m / N)) * ((((n - 1) * (m - 1)) / (N - 1)) + 1 - (n * (m / N)))
}

#### Pareto ####

# alpha > k
kthmoment_pareto1 <- function(alpha, lam, k)
{
    (lam^k * factorial(k))/prod(alpha - seq(from = 1, to = k, by = 1))
}

# -1 < k < alpha
kthmoment_pareto2 <- function(alpha, lam, k)
{
    (lam^k * gamma(k + 1) * gamma(alpha - k))/gamma(alpha)
}

#### Logarithmique ####

E_logarithmique <- function(gam)
{
    (-gam) / (log(1 - gam) * (1 - gam))
}

V_logarithmique <- function(gam)
{
    (gam + log(1 - gam)) / ((1 - gam)^2 * (log(1 - gam))^2)
}

#### Erlang Généralisée ####

# derlang_gen <- function(x, n = length(b), b)
# {
#     sum(sapply(1:n, function(i)
#         b[i] *
#             exp(-b[i] * x) *
#             prod(sapply(c(1:(i - 1),
#                           (i + 1):n),
#                         function(j)
#                             (b[j] / (b[j] - b[i]))))))
# }
# 
# derlang_gen(x = 1, b = c(3, 4))
#### Inverse Gaussienne ####

E_IG <- function(mu)
{
    mu
}

V_IG <- function(mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    mu * beta
}

d_IG <- function(x, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    dinvgauss(x = x, mean = mu, dispersion = dispersion)
}    

p_IG <- function(q, mu, beta = dispersion * mu^2, dispersion = beta / mu^2, lower.tail = T)
{
    pinvgauss(q = q, mean = mu, dispersion = dispersion, lower.tail = lower.tail)
}

MGF_IG <- function(t, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    exp((mu/beta) * (1 - sqrt(1 - 2 * beta * t)))
}

Etronq_IG <- function(d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    d - 
        (2 * d - mu) *
        pnorm(q = (d - mu) * 
                  sqrt(1 / (beta * d))) -
        (2 * d + mu) * 
        exp(2 * mu / beta) * 
        pnorm(q = - (d + mu) * 
                  sqrt(1 / (beta * d)))
}

VaR_IG <- function(p, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    qinvgauss(p = p, mean = mu, dispersion = dispersion)
}

TVaR_IG <- function(k, vark, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    (1/(1 - k)) * 
        (mu - vark + 
             (2 * vark + mu) * 
             exp(2 * mu / beta)
         ) + 
        (1/(1 - k)) * 
        (
            (2 * vark - mu) * 
             pnorm(q = ((vark - mu) * 
                       sqrt(1 / (beta * vark)))
                   )
        )
}

SL_IG <- function(d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    (mu - d) *
        pnorm(q = (d - mu) * 
                  sqrt(1 / (beta * d)), 
              lower.tail = F) +
        (d + mu) * 
        exp(2 * mu / beta) * 
        pnorm(q = - (d + mu) * 
                  sqrt(1 / (beta * d)))
}


# Doesn't work right now
# Mexcess_IG <- function(x, d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
# {
#     (
#         (mu - d) * (
#             (1 - qnorm(p = 
#                            (
#                                (d - mu) * sqrt(1 / (beta * d))
#                            )
#                       )
#             )
#         )
#     ) / 
#         (1 - (qnorm(p = 
#                         (
#                             (sqrt(1 / (beta * x)) * (d - mu))
#                         )
#                     ) +
#               exp(2 * mu / beta) *
#               qnorm(p = 
#                         (
#                             (- sqrt(1 / (beta * x)) * (d + mu))
#                         )
#                     )
#               )
#         ) +
#     (
#         (mu + d) * 
#             exp(2 * mu / beta) *
#             qnorm(p = 
#                       (
#                           (-sqrt(1 / (beta * x)) * (d + mu))
#                       )
#                   )
#     ) /
#         (1 - (qnorm(p = 
#                         (
#                             (sqrt(1 / (beta * x)) * (d - mu))
#                         )
#                    ) +
#              exp(2 * mu / beta) *
#              qnorm(p = 
#                       (
#                           (- sqrt(1 / (beta * x)) * (d + mu))
#                       )
#                  )
#             )
#         )
# }

Elim_IG <- function(d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    levinvgauss(limit = d, mean = mu, dispersion = dispersion, order = 1)
}
#### Poisson Composée ####

E_PCOMP <- function(rate, shape, lambda, distr_severity = "Gamma"){
    if(distr_severity == "Gamma"){
        shape / rate * lambda
    }
    else{
        exp(shape + rate / 2) * lambda
    }
}

V_PCOMP <- function(rate, shape, lambda, distr_severity = "Gamma"){
    if(distr_severity == "Gamma"){
        lambda * shape/(rate) * (shape + 1)
    }
    else{
        lambda * kthmoment_lnorm(k = 2, mu = shape, sig = sqrt(rate))
    }
}

p_Pcomp <- function(x, lambda, shape, rate, ko = 300, distr_severity = "Gamma"){
    if(distr_severity == "Gamma"){
        (dpois(x = 0, lambda = lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda = lambda) * pgamma(q = x, shape = shape * k, rate = rate))))
    } 
    else{
        dpois(x = 0, lambda = lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda = lambda) * plnorm(q = x, meanlog = shape * k, sdlog = sqrt(rate))))
    }
}

VaR_PComp <- function(k, ko = 300, lambda, shape, rate, distr_severity = "Gamma")
{
    if(k <= p_Pcomp(x = 0, lambda = lambda, shape = shape, rate = rate, ko = ko))
        0
    else
        optimize(function(i) abs(p_Pcomp(x = i, lambda = lambda, shape = shape, rate = rate, ko = ko) - k), c(0, ko))$minimum
}


TVaR_PComp <- function(x, lamb, ko = 300, shape, rate, vark, distr_severity = "Gamma")
{
    if (vark == 0)
    {
        E_PCOMP(rate = rate, shape = shape, lambda = lamb, distr_severity = distr_severity) / (1 - x)
    }
    else if (distr_severity == "Gamma")
    {
        sum(sapply(1:ko, function(k) dpois(x = k, lambda = lamb) * (shape * k )/rate * pgamma(q = vark, shape = shape * k + 1, rate = rate, lower.tail = F)))/(1 - x)
    }
    else
    {
        sum(sapply(1:ko, function(k) dpois(x = k, lambda = lamb) * (shape * k )/rate * plnorm(q = vark, meanlog = shape * k + 1, sdlog = sqrt(rate), lower.tail = F)))/(1 - x)
    }
    
}

#### BN Composée ####

E_BNComp <- function(r, q, shape, rate, distr_severity = "Gamma") 
{
    if(distr_severity == "Gamma")
    {
        shape / rate * (r * (1 - q)/q)
    }
    else
        (r * (1 - q)/q) * E_lnorm(mu = shape, sig = sqrt(rate))
}

V_BNComp <- function(r, q, shape, rate, distr_severity = "Gamma") 
{
    if(distr_severity == "Gamma")
    {
        (shape / rate)^2 * (r * (1 - q)/(q^2)) + (shape / rate^2) * (r * (1 - q)/q)
    }
    else
        (r * (1 - q)/q)*(E_lnorm(mu = shape, sig = sqrt(rate))/q + V_lnorm(mu = shape, sig = sqrt(rate)))
}

# pas certain si la LNORM est bien faite
p_BNComp <- function(x, r, q, ko, shape, rate, distr_severity = "Gamma") 
{
    if(distr_severity == "Gamma")
    {
        dnbinom(x = 0, size = r, prob = q) + sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * pgamma(q = x, shape = shape * i, rate = rate)))
    }
    else
    {
        dnbinom(x = 0, size = r, prob = q) + sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * plnorm(q = x, meanlog = shape * i, sdlog = sqrt(rate))))
    }
}

VaR_BNComp <- function(k, ko = 300, r, q, shape, rate, distr_severity = "Gamma")
{
    if(k <= p_BNComp(0, r = r, q = q, ko = ko, shape = shape, rate = rate))
        0
    else
        optimize(function(i) abs(p_BNComp(i, r = r, q = q, ko = ko, shape = shape, rate = rate) - k), c(0, ko))$minimum
}

TVaR_BNComp <- function(x, shape, rate, r, q, vark, ko, distr_severity = "Gamma"){
    
    if (vark == 0)
    {
        E_BNComp(r = r, q = q, shape = shape, rate = rate, distr_severity = distr_severity) / (1 - x)
    }
    else if (distr_severity == "Gamma")
    {
        (sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * (shape / rate) * pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = F))) / (1 - x))
    }
    else
    {
        (sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * (shape / rate) * plnorm(q = vark, meanlog = shape * i + 1, sdlog = sqrt(rate), lower.tail = F))) / (1 - x))
    }
}
#### Bin Composée ####


E_BINComp <- function(n, q, shape, rate, distr_severity = "Gamma") 
{
    if(distr_severity == "Gamma")
    {
        shape / rate * n * q
    }
    else
        n * q * E_lnorm(mu = shape, sig = sqrt(rate))
}

V_BINComp <- function(n, q, shape, rate, distr_severity = "Gamma") 
{
    if(distr_severity == "Gamma")
    {
        (shape / rate)^2 * n * q * (1 - q) + n * q * V_gamma(a = shape, b = rate)
    }
    else
        E_lnorm(mu = shape, sig = sqrt(rate))^2 * n * q * (1 - q) + n * q * V_lnorm(mu = shape, sig = rate)
}

p_BINComp <- function(x, n, q, ko, shape, rate, distr_severity = "Gamma") 
{
    if(distr_severity == "Gamma")
    {
        dbinom(x = 0, size = n, prob = q) + sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * pgamma(q = x, shape = shape * i, rate = rate)))
    }
    else
    {
        dbinom(x = 0, size = n, prob = q) + sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * plnorm(q = x, meanlog = shape * i, sdlog = sqrt(rate))))
    }
}


VaR_BINComp <- function(k, ko = 300, n, q, shape, rate, distr_severity = "Gamma")
{
    if(k <= p_BINComp(0, n = n, q = q, ko = ko, shape = shape, rate = rate))
        0
    else
        optimize(function(i) abs(p_BINComp(i, n = n, q = q, ko = ko, shape = shape, rate = rate) - k), c(0, ko))$minimum
}


TVaR_BINComp <- function(x, shape, rate, n, q, vark, ko, distr_severity = "Gamma"){
    
    if (vark == 0)
    {
        E_BINComp(n = n, q = q, shape = shape, rate = rate, distr_severity = distr_severity) / (1 - x)
    }
    else if (distr_severity == "Gamma")
    {
        (sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * (shape * i / rate) * pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = F))) / (1 - x))
    }
    else
    {
        (sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * (shape * i / rate) * plnorm(q = vark, meanlog = shape * i + 1, sdlog = sqrt(rate), lower.tail = F))) / (1 - x))
    }
}


#### Uniforme Discrète ####
E_unifD <- function(a, b){
    (a + b)/2
}

V_unifD <- function(a, b){
    ((b - a + 1)^2 - 1)/12
}

d_unifD <- function(x, a, b){
    1 / (b - a + 1)
}

p_unifD <- function(q, a, b){
    if(q < a){
        return(0)
    }
    else if(a <= q & q < b){
        (round(q) - a + 1)/(b - a + 1)
    }
    else{
        return(1)
    }
}
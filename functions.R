radioButtons_withHTML <- function (inputId, label, choices, selected = NULL, inline = FALSE, 
                                   width = NULL) 
{
    choices <- shiny:::choicesWithNames(choices)
    selected <- if (is.null(selected)) 
        choices[[1]]
    else {
        shiny:::validateSelected(selected, choices, inputId)
    }
    if (length(selected) > 1) 
        stop("The 'selected' argument must be of length 1")
    options <- generateOptions_withHTML(inputId, choices, selected, inline, 
                                        type = "radio")
    divClass <- "form-group shiny-input-radiogroup shiny-input-container"
    if (inline) 
        divClass <- paste(divClass, "shiny-input-container-inline")
    tags$div(id = inputId, style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), class = divClass, 
        shiny:::controlLabel(inputId, label), options)
}
generateOptions_withHTML <- function (inputId, choices, selected, inline, type = "checkbox") 
{
    options <- mapply(choices, names(choices), FUN = function(value, 
                                                              name) {
        inputTag <- tags$input(type = type, name = inputId, value = value)
        if (value %in% selected) 
            inputTag$attribs$checked <- "checked"
        if (inline) {
            tags$label(class = paste0(type, "-inline"), inputTag, 
                       tags$span(HTML(name)))
        }
        else {
            tags$div(class = type, tags$label(inputTag, tags$span(HTML(name))))
        }
    }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    div(class = "shiny-options-group", options)
}

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
            lower.tail = F
        ) - d
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

kthmoment_lnorm <- function(k, mu, sig)
{
    exp(mu * k + k ^ 2 * (sig ^ 2) / 2)
}
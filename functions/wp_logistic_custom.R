wp_logistic_custom <- function (n = NULL,
          p0 = NULL,
          p1 = NULL,
          alpha = 0.05,
          power = NULL,
          alternative = c("two.sided", "less", "greater"),
          family = c("Bernoulli", "exponential", "lognormal", "normal", "Poisson", "uniform"),
          parameter = NULL)
{
    sig.level <- alpha

    # Check if
    # if (sum(sapply(list(n, power), is.null)) != 1)
    if (sum(sapply(list(n, power, alpha), is.null)) != 1)
        stop("exactly one of n, power, and alpha must be NULL")

    if (is.null(p0) | (!is.null(p0) & !is.numeric(p0)) | any(0 > p0, p0 > 1))
        stop(sQuote("p0"), " must be numeric in (0,1)")

    if (is.null(p1) || !is.null(p1) && !is.numeric(p1) || any(0 > p1 | p1 > 1))
        stop(sQuote("p1"), " must be numeric in (0,1)")

    # if (!is.null(n) && min(n) < 5)
    #     stop("number of observations must be at least 5")

    if (is.null(alpha) || !is.null(alpha) & !is.numeric(alpha) ||
        any(alpha < 0 | alpha > 1))
        stop(sQuote("alpha"), " must be numeric in [0, 1]")
    if (!is.null(power) & !is.numeric(power) || any(0 > power |
                                                    power > 1))
        stop(sQuote("power"), " must be numeric in [0, 1]")
    alternative <- match.arg(alternative)
    tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)
    p.body <- quote({
        s * pnorm(-qnorm(1 - alpha) - sqrt(n)/sqrt(g * v0 + (1 -
                                                                 g) * v1) * beta1) + t * pnorm(-qnorm(1 - alpha) +
                                                                                                   sqrt(n)/sqrt(g * v0 + (1 - g) * v1) * beta1)
    })
    if (family == "Bernoulli") {
        if (is.null(parameter)) {
            B <- 0.5
        }
        else {
            B <- parameter
        }
        g <- 0
        odds <- p1/(1 - p1)/(p0/(1 - p0))
        beta1 <- log(odds)
        beta0 <- log(p0/(1 - p0))
        d <- B * p1 * (1 - p1) + (1 - B) * p0 * (1 - p0)
        e <- B * p1 * (1 - p1)
        f <- B * p1 * (1 - p1)
        v1 <- d/(d * f - e^2)
        mu1 <- B * p1 + (1 - B) * p0
        i00 <- log(mu1/(1 - mu1))
        pn <- 1/(1 + exp(-i00))
        a <- pn * (1 - pn)
        b <- B * pn * (1 - pn)
        c <- B * pn * (1 - pn)
        v0 <- b/(a * b - c^2)
        if (tside == 1) {
            s <- 1
            t <- 0
            alpha <- alpha
        }
        if (tside == 2) {
            s <- 1
            t <- 1
            alpha <- alpha/2
        }
        if (tside == 3) {
            s <- 0
            t <- 1
            alpha <- alpha
        }
        if (is.null(power))
            power <- eval(p.body)
        if (is.null(n))
            n <- uniroot(function(n) eval(p.body) - power, c(2 +
                                                                 1e-10, 1e+07))$root
    }
    if (family == "exponential") {
        if (is.null(parameter)) {
            lambda <- 1
        }
        else {
            lambda <- parameter
        }
        g <- 0
        beta0 <- log(p0/(1 - p0))
        odds <- p1/(1 - p1)/(p0/(1 - p0))
        beta1 <- log(odds)
        d <- integrate(function(x) (1 - (1 + exp(-beta0 - beta1 *
                                                     x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dexp(x, lambda), 0, Inf, subdivisions = 100L)$value
        e <- integrate(function(x) x * (1 - (1 + exp(-beta0 -
                                                         beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dexp(x, lambda), 0, Inf, subdivisions = 100L)$value
        f <- integrate(function(x) x^2 * (1 - (1 + exp(-beta0 -
                                                           beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dexp(x, lambda), 0, Inf, subdivisions = 100L)$value
        v1 <- d/(d * f - e^2)
        mu1 <- integrate(function(x) 1/(1 + exp(-beta0 - beta1 *
                                                    x)) * dexp(x, lambda), 0, Inf, subdivisions = 100L)$value
        i00 <- log(mu1/(1 - mu1))
        pn <- 1/(1 + exp(-i00))
        a <- pn * (1 - pn)
        b <- 2 * lambda^(-2) * pn * (1 - pn)
        c <- lambda^(-1) * pn * (1 - pn)
        v0 <- a/(a * b - c^2)
        if (tside == 1) {
            s <- 1
            t <- 0
            alpha <- alpha
        }
        if (tside == 2) {
            s <- 1
            t <- 1
            alpha <- alpha/2
        }
        if (tside == 3) {
            s <- 0
            t <- 1
            alpha <- alpha
        }
        if (is.null(power))
            power <- eval(p.body)
        if (is.null(n))
            n <- uniroot(function(n) eval(p.body) - power, c(2 +
                                                                 1e-10, 1e+07))$root
    }
    if (family == "lognormal") {
        g <- 0
        if (is.null(parameter)) {
            mu <- 0
            sigma <- 1
        }
        else {
            if (length(parameter) != 2)
                stop("Both mean and standard deviation of the log-normal distribution have to be provided as a vector.")
            mu <- parameter[1]
            sigma <- parameter[2]
        }
        beta0 <- log(p0/(1 - p0))
        odds <- p1/(1 - p1)/(p0/(1 - p0))
        beta1 <- log(odds)
        d <- integrate(function(x) (1 - (1 + exp(-beta0 - beta1 *
                                                     x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dlnorm(x, mu, sigma), 0, Inf, subdivisions = 100L)$value
        e <- integrate(function(x) x * (1 - (1 + exp(-beta0 -
                                                         beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dlnorm(x, mu, sigma), 0, Inf, subdivisions = 100L)$value
        f <- integrate(function(x) x^2 * (1 - (1 + exp(-beta0 -
                                                           beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dlnorm(x, mu, sigma), 0, Inf, subdivisions = 100L)$value
        v1 <- d/(d * f - e^2)
        mu1 <- integrate(function(x) 1/(1 + exp(-beta0 - beta1 *
                                                    x)) * dlnorm(x, mu, sigma), 0, Inf, subdivisions = 100L)$value
        i00 <- log(mu1/(1 - mu1))
        pn <- 1/(1 + exp(-i00))
        a <- pn * (1 - pn)
        b <- (exp(sigma * sigma) - 1) * exp(2 * mu + sigma^2) *
            pn * (1 - pn)
        c <- exp(mu + 0.5 * sigma^2) * pn * (1 - pn)
        v0 <- a/(a * b - c^2)
        if (tside == 1) {
            s <- 1
            t <- 0
            alpha <- alpha
        }
        if (tside == 2) {
            s <- 1
            t <- 1
            alpha <- alpha/2
        }
        if (tside == 3) {
            s <- 0
            t <- 1
            alpha <- alpha
        }
        if (is.null(power))
            power <- eval(p.body)
        if (is.null(n))
            n <- uniroot(function(n) eval(p.body) - power, c(2 +
                                                                 1e-10, 1e+07))$root
    }
    if (family == "normal") {
        g <- 0
        if (is.null(parameter)) {
            mu <- 0
            sigma <- 1
        }
        else {
            if (length(parameter) != 2)
                stop("Both mean and standard deviation of the normal distribution have to be provided as a vector.")
            mu <- parameter[1]
            sigma <- parameter[2]
        }
        beta0 <- log(p0/(1 - p0))
        odds <- p1/(1 - p1)/(p0/(1 - p0))
        beta1 <- log(odds)
        d <- integrate(function(x) (1 - (1 + exp(-beta0 - beta1 *
                                                     x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dnorm(x, mu, sigma), -Inf, Inf, subdivisions = 100L)$value
        e <- integrate(function(x) x * (1 - (1 + exp(-beta0 -
                                                         beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dnorm(x, mu, sigma), -Inf, Inf, subdivisions = 100L)$value
        f <- integrate(function(x) x^2 * (1 - (1 + exp(-beta0 -
                                                           beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                           dnorm(x, mu, sigma), -Inf, Inf, subdivisions = 100L)$value
        v1 <- d/(d * f - e^2)
        mu1 <- integrate(function(x) 1/(1 + exp(-beta0 - beta1 *
                                                    x)) * dnorm(x, mu, sigma), -Inf, Inf, subdivisions = 100L)$value
        i00 <- log(mu1/(1 - mu1))
        pn <- 1/(1 + exp(-i00))
        a <- pn * (1 - pn)
        b <- sigma^2 * pn * (1 - pn)
        c <- mu * pn * (1 - pn)
        v0 <- a/(a * b - c^2)
        if (tside == 1) {
            s <- 1
            t <- 0
            alpha <- alpha
        }
        if (tside == 2) {
            s <- 1
            t <- 1
            alpha <- alpha/2
        }
        if (tside == 3) {
            s <- 0
            t <- 1
            alpha <- alpha
        }
        if (is.null(power))
            power <- eval(p.body)
        if (is.null(n))
            n <- uniroot(function(n) eval(p.body) - power, c(2 +
                                                                 1e-10, 1e+07))$root
    }
    if (family == "Poisson") {
        g <- 0
        if (is.null(parameter)) {
            lambda <- 1
        }
        else {
            lambda <- parameter
        }
        beta0 <- log(p0/(1 - p0))
        odds <- p1/(1 - p1)/(p0/(1 - p0))
        beta1 <- log(odds)
        d <- sum(sapply(0:1e+05, function(x) (1 - (1 + exp(-beta0 -
                                                               beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                            dpois(x, lambda)))
        e <- sum(sapply(0:1e+05, function(x) x * (1 - (1 + exp(-beta0 -
                                                                   beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1) *
                            dpois(x, lambda)))
        f <- sum(sapply(0:1e+05, function(x) x^2 * (1 - (1 +
                                                             exp(-beta0 - beta1 * x))^(-1)) * (1 + exp(-beta0 -
                                                                                                           beta1 * x))^(-1) * dpois(x, lambda)))
        v1 <- d/(d * f - e^2)
        mu1 <- sum(sapply(0:1e+05, function(x) 1/(1 + exp(-beta0 -
                                                              beta1 * x)) * dpois(x, lambda)))
        i00 <- log(mu1/(1 - mu1))
        pn <- 1/(1 + exp(-i00))
        a <- pn * (1 - pn)
        b <- lambda * pn * (1 - pn)
        c <- lambda * pn * (1 - pn)
        v0 <- a/(a * b - c^2)
        if (tside == 1) {
            s <- 1
            t <- 0
            alpha <- alpha
        }
        if (tside == 2) {
            s <- 1
            t <- 1
            alpha <- alpha/2
        }
        if (tside == 3) {
            s <- 0
            t <- 1
            alpha <- alpha
        }
        if (is.null(power))
            power <- eval(p.body)
        if (is.null(n))
            n <- uniroot(function(n) eval(p.body) - power, c(2 +
                                                                 1e-10, 1e+07))$root
    }
    if (family == "uniform") {
        g <- 0
        if (is.null(parameter)) {
            L <- 0
            R <- 1
        }
        else {
            if (length(parameter) != 2)
                stop("The lower and upper bounds have to be provided as a vector")
            L <- parameter[1]
            R <- parameter[2]
        }
        beta0 <- log(p0/(1 - p0))
        odds <- p1/(1 - p1)/(p0/(1 - p0))
        beta1 <- log(odds)
        d <- integrate(function(x) (1 - (1 + exp(-beta0 - beta1 *
                                                     x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1)/(R -
                                                                                                         L), L, R, subdivisions = 100L)$value
        e <- integrate(function(x) x * (1 - (1 + exp(-beta0 -
                                                         beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1)/(R -
                                                                                                                     L), L, R, subdivisions = 100L)$value
        f <- integrate(function(x) x^2 * (1 - (1 + exp(-beta0 -
                                                           beta1 * x))^(-1)) * (1 + exp(-beta0 - beta1 * x))^(-1)/(R -
                                                                                                                       L), L, R, subdivisions = 100L)$value
        v1 <- d/(d * f - e^2)
        mu1 <- integrate(function(x) 1/(1 + exp(-beta0 - beta1 *
                                                    x))/(R - L), L, R, subdivisions = 100L)$value
        i00 <- log(mu1/(1 - mu1))
        pn <- 1/(1 + exp(-i00))
        a <- pn * (1 - pn)
        b <- (R - L)^2/12 * pn * (1 - pn)
        c <- (R + L)/2 * pn * (1 - pn)
        v0 <- a/(a * b - c^2)
        if (tside == 1) {
            s <- 1
            t <- 0
            alpha <- alpha
        }
        if (tside == 2) {
            s <- 1
            t <- 1
            alpha <- alpha/2
        }
        if (tside == 3) {
            s <- 0
            t <- 1
            alpha <- alpha
        }
        if (is.null(power))
            power <- eval(p.body)
        if (is.null(n))
            n <- uniroot(function(n) eval(p.body) - power, c(2 +
                                                                 1e-10, 1e+07))$root
    }
    METHOD <- "Power for logistic regression"
    URL <- "http://psychstat.org/logistic"
    structure(list(p0 = p0, p1 = p1, beta0 = beta0, beta1 = beta1,
                   n = n, alpha = sig.level, power = power, alternative = alternative,
                   family = family, parameter = parameter, method = METHOD,
                   url = URL), class = "webpower")
}

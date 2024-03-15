#' Utility to approximate one distribution with another
#'
#' @param posterior posterior draws you wish to approximate
#' @param dist Which distribution to approximate with
#' @param n how many quantiles to use
#'
#' @export
#'
qq_approx <- function(posterior, dist='norm', n=1000, distance='sq') {
    target_qs <- quantile(posterior, p=1:n/(n+1))

    pars <- calc_distpar(mean(posterior), sd(posterior))

    if (distance=='sq') {
        distfun <- sqdist
    } else if (distance=='abs') {
        distfun <- absdist
    } else if (distance=='pct') {
        distfun <- pctdist
    } else {
        stop('I don\'t have that distance')
    }

    if (dist=='norm') {
        return(optim(c(pars$norm$mean, pars$norm$sd), fn=qq_norm_approx, target_qs=target_qs, n=n, distfun=distfun))
    } else if (dist=='hypernorm') {
        return(optim(c(pars$norm$mean, pars$norm$sd), fn=qq_hypernorm_approx, target_qs=target_qs, n=n, distfun=distfun))
    } else if (dist=='lognorm') {
        return(optim(c(pars$lognorm$mean, pars$lognorm$sd), fn=qq_lognorm_approx, target_qs=target_qs, n=n, distfun=distfun))
    } else if (dist=='beta') {
        return(optim(c(pars$beta$alpha, pars$beta$beta), fn=qq_beta_approx, target_qs=target_qs, n=n, distfun=distfun))
    } else if (dist=='gamma') {
        return(optim(c(pars$gamma$shape, pars$gamma$rate), fn=qq_gamma_approx, target_qs=target_qs, n=n, distfun=distfun))
    } else {
        stop('I don\'t have that distribution')
    }
}

qq_norm_approx <- function(args, target_qs, n, distfun=sqdist) {
    if (args[2] <= 0) {
        return(Inf)
    } else {
        test_qs <- qnorm(p=1:n/(n+1), mean=args[1], sd = args[2])
        return(distfun(target_qs, test_qs))
    }
}

qq_hypernorm_approx <- function(args, target_qs, n, distfun=sqdist) {
    if (args[2] <= 0) {
        return(Inf)
    } else {
        hyper <- truncnorm::qtruncnorm(p=1:n/(n+1), mean=args[1], sd = args[2], a=0)
        draws <- rnorm(n*10, mean=0, sd=hyper)
        test_qs <- quantile(draws, 1:n/(n+1), type=4)
        return(distfun(target_qs, test_qs))
    }
}

qq_lognorm_approx <- function(args, target_qs, n, distfun=sqdist) {
    if (args[2] <= 0) {
        return(Inf)
    } else {
        test_qs <- exp(qnorm(1:n/(n+1), args[1], args[2]))
        return(distfun(target_qs, test_qs))
    }
}

qq_beta_approx <- function(args, target_qs, n, distfun=sqdist) {
    if (args[1] <= 0 | args[2] <= 0) {
        return(Inf)
    } else {
        test_qs <- qbeta(1:n/(n+1), args[1], args[2])
        return(distfun(target_qs, test_qs))
    }
}

qq_gamma_approx <- function(args, target_qs, n, distfun=sqdist) {
    if (args[1] <= 0 | args[2] <= 0) {
        return(Inf)
    } else {
        test_qs <- qgamma(1:n/(n+1), args[1], args[2])
        return(distfun(target_qs, test_qs))
    }
}


sqdist <- function(a,b) {
    return(mean((a-b)^2))
}

absdist <- function(a,b) {
    return(mean(abs(a-b)))
}

pctdist <- function(a,b) {
    return(mean(abs((a-b)/pmin(abs(a),abs(b)))))
}

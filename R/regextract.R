#' Wrapper around extract to allow regular expressions
#'
#' @param fit The fitted stan object
#' @param pars Pattern for which parameters to use, defaults to '.' to select everything
#' @param excl_pars Pattern for which parameters to exclude. Takes precedence over pars. Defaults to raw and lp__
#' @param as_df Whether to return the default list thing like stan, or a tibble
#'
#' @export
#'
regextract <- function(fit, pars='.', excl_pars='raw|^lp__$', as_df=TRUE) {
    poi <- regex_pars(fit, pars=pars, excl_pars=excl_pars)
    if ('stanfit' %in% class(fit)) {
        ext <- rstan::extract(fit,pars=poi)
    } else if ('CmdStanFit' %in% class(fit)) {
        ext <- fit$draws(variables = poi)
        nms <- dimnames(ext)[[3]]
        ext <- array(ext, dim=c(prod(dim(ext)[1:2]), dim(ext)[3]))
        dimnames(ext) <- list(NULL, nms)
    } else {
        stop('fit not from rstan or cmdstanr')
    }

    if (as_df) {
        return(tibble::as_tibble(as.data.frame(ext)))
    } else {
        return(ext)
    }
}

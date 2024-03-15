#' Wrapper for rstan's summary that returns a prettier tibble and uses a regex to filter parameters
#'
#' @param fit Fitted stan object
#' @param pars Pattern for which parameters to include
#' @param excl_pars Pattern for which parameters to exclude (takes precedence over pars). Defaults to excluding anything with 'raw' and lp__
#' @param excl_probs Whether to exclude the posterior probability calculates, defaults to TRUE
#' @param excl_se Whether to exclude the SE of the posterior mean, defaults to TRUE
#' @param probs Which posterior probabilities to include. Only does anything if excl_probs is FALSE
#'
#' @export
#'
get_smry <- function(fit,
                     pars='.', excl_pars='raw|^lp__$',
                     excl_probs=TRUE,
                     excl_se=TRUE,
                     probs=c(0.025, 0.25, 0.50, 0.75, 0.975)) {
    poi <- regex_pars(fit, pars=pars, excl_pars=excl_pars)

    if (excl_probs) {
        probs <- NULL
    }

    smry <- rstan::summary(fit, pars=poi, probs=probs)$summary %>%
        tibble::as_tibble(rownames = 'par')

    if (excl_se) {
        smry <- dplyr::select(smry, -se_mean)
    }
    return(smry)
}

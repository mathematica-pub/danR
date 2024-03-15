#' Wrapper for rstan's monitor that returns a prettier tibble
#'
#' @param chains Stan-style array of iters*chains*pars
#' @param warmup what number of draws is excluded as warmup. Default assumes 0 for sampling only
#' @param probs What probabilities to calculate
#' @param extra_cols Which additional columns, like mcmc SE, to keep. If you want probs then you need to specify them here too because Iam lazy
#'
#' @export
#'
monitor <- function(chains,
                    warmup=0,
                    probs=.5,
                    extra_cols=NULL) {
    tokeep <- c('par', 'mean', 'sd', 'n_eff', 'Rhat', extra_cols)

    smry <- rstan::monitor(chains, warmup=warmup, print=FALSE, probs=probs) %>%
        tibble::as_tibble(rownames = 'par') %>%
        dplyr::select(dplyr::all_of(tokeep))

    return(smry)
}

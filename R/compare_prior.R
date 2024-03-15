#' Utility to compare a batch of parameters to their prior
#'
#' @param fit Fitted stan object
#' @param pars Pattern for which parameters to include, defaults to all things sigma
#' @param excl_pars Pattern for which parameters to exclude (takes precedence over pars). Defaults to excluding anything with 'raw' and lp__
#' @param prior A vector with draws from the prior; defaults to N+(0,1)
#'
#' @export
#'
compare_prior <- function(fit, pars='sigma', excl_pars='raw|^lp__$', prior=abs(rnorm(1e5,0,1))) {
    data <- regextract(fit, pars=pars, excl_pars=excl_pars) %>%
        tidyr::pivot_longer(everything(), names_to='par') %>%
        dplyr::bind_rows(tibble::tibble(par='prior',value=prior)) %>%
        dplyr::mutate(is_prior = par=='prior')

    ggplot2::ggplot(data) +
        ggplot2::geom_density(ggplot2::aes(x=value, color=par, linetype=is_prior)) +
        ggplot2::scale_linetype_manual(values=c('solid','dashed')) +
        ggplot2::guides(linetype=FALSE)
}

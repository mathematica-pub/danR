#' Compare parameter posteriors from fitted models
#'
#' @param fitlist Named list of fitted stan objects
#' @param pars Pattern matching which parameters to explore, defaults to 'sigma'
#' @param excl_pars Pattern for which parameters to exclude. Takes precedence over pars. Defaults to raw and lp__
#' @param print Whether to print the density plots
#' @param ret Whether to return the data comparing parameters (TRUE) or the plot (FALSE)
#'
#' @export
#'
compare_pars <- function(fitlist, pars='sigma', excl_pars='raw|^lp__$', print=TRUE, ret=FALSE) {

    data <- lapply(fitlist, regextract, pars=pars, excl_pars=excl_pars) %>%
        dplyr::bind_rows(.id='fit') %>%
        tidyr::pivot_longer(-fit, names_to='par') %>%
        dplyr::mutate(fit=factor(fit, levels=names(fitlist)))

    plot <- ggplot2::ggplot(data) +
        ggplot2::geom_density(ggplot2::aes(x=value, color=fit)) +
        ggplot2::facet_wrap(~par, scales='free')

    if (print) {
        print(plot)
    }

    if(ret) {
        return(data)
    } else {
        return(plot)
    }
}

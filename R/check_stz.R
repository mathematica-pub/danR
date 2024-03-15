#' See how closely batches of parameters are summing to 0
#'
#' @param fit Fitted stan object
#' @param pars Pattern matching which parameters to explore, defaults to everything named theta
#' @param excl_pars Pattern for which parameters to exclude. Takes precedence over pars. Defaults to sigma
#' @param print Whether to print the density plots
#' @param ret Whether to return the data comparing parameters (TRUE) or the plot (FALSE)
#'
#' @export
#'
check_stz <- function(fit, pars='theta', excl_pars='sigma', print=TRUE, ret=FALSE) {
    poi <- regex_pars(fit, pars=pars, excl_pars=excl_pars)

    sums <- lapply(poi, function(par) {
        tibble::tibble(par=par,
               sum=rstan::extract(fit, par) %>%
                   as.data.frame %>%
                   apply(1, sum))
    }) %>%
        dplyr::bind_rows()

    plot <- ggplot2::ggplot(sums) +
        ggplot2::geom_density(ggplot2::aes(x=sum, color=par))

    if (print) {
        print(plot)
    }

    if(ret) {
        return(sums)
    } else {
        return(plot)
    }
}

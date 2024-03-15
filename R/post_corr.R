#' Utility to create a pretty-ish heatmap of posterior correlations
#'
#' @param fit Fitted stan object
#' @param pars Pattern for which parameters to include
#' @param excl_pars Pattern for which parameters to exclude (takes precedence over pars). Defaults to excluding anything with 'raw' and lp__
#' @param label Whether to print corrs in the matrix, defaults to TRUE
#' @param label_round Number of digits for correlation labels, defaults to 2
#' @param ... options passed to GGally::ggcorr()
#'
#' @export
#'
post_corr <- function(fit, pars='sigma', excl_pars='raw|lp__', label=TRUE, label_round=2, ...) {
    regextract(fit, pars=pars, excl_pars=excl_pars, as_df=TRUE) %>%
        GGally::ggcorr(hjust=1, high='#0000ff', low='#ff0000', label=label, label_round=label_round, ...)
}

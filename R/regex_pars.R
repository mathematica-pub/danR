#' Utility to get a list of parameters via regex
#'
#' @param fit The fitted stan object
#' @param pars Pattern for which parameters to use, defaults to '.' to select everything
#' @param excl_pars Pattern for which parameters to exclude. Takes precedence over pars
#'
#' @export
#'
regex_pars <- function(fit, pars='.', excl_pars=NULL) {
    if ('stanfit' %in% class(fit)) {
        poi <- fit@sim$pars_oi
    } else if ('CmdStanFit' %in% class(fit)) {
        poi <- fit$metadata()$stan_variables
    } else {
        stop('fit have class stanfit or CmdStanFit')
    }

    poi <- poi[stringr::str_detect(poi,pars)]
    if (!is.null(excl_pars)) {
        poi <- poi[!stringr::str_detect(poi, excl_pars)]
    }
    return(poi)
}

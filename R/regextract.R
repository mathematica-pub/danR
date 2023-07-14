#' Wrapper around extract to allow regular expressions
#'
#' @param fit The fitted stan object
#' @param regex Pattern, defaults to '.' to select everything
#' @param as_df whether to return the default list thing like stan, or a tibble
#'
#' @export
#'
regextract <- function(fit, regex='.', as_df=TRUE) {
    pars <- fit@sim$pars_oi[stringr::str_detect(fit@sim$pars_oi,regex)]
    ext <- rstan::extract(fit,pars=pars)
    if (as_df) {
        return(tibble::as_tibble(as.data.frame(ext)))
    } else {
        return(ext)
    }
}

#' Utility to check some basic diagnostics
#'
#' @param fit Fitted stan object
#'
#' @export
#'
basic_diags <- function(fit) {
    dts <- sum(rstan::get_divergent_iterations(fit))

    max_td <- fit@stan_args[[1]]$control$max_treedepth
    if(is.null(max_td)) max_td <- 10
    iter <- fit@stan_args[[1]]$iter
    warmup <- fit@stan_args[[1]]$warmup
    chains <- length(fit@stan_args)

    maxed_td <- rstan::get_sampler_params(fit) %>%
        lapply(function(x) {
            tds <- x[(warmup+1):iter, 'treedepth__']
            return(sum(tds==max_td))
        }) %>%
        unlist %>%
        sum

    runtimes <- rstan::get_elapsed_time(fit) %>% apply(1, sum)
    smry <- get_smry(fit, pars='.', excl_pars='^lp__') %>% dplyr::filter(!is.nan(Rhat))
    worst_neff <- smry %>% dplyr::arrange(n_eff) %>% dplyr::filter(dplyr::row_number()==1)
    worst_Rhat <- smry %>% dplyr::arrange(-Rhat) %>% dplyr::filter(dplyr::row_number()==1)

    if (dts==0) {
        print(glue::glue('No DTs'))
    } else {
        dtplural <- ifelse(dts==1,'','s')
        warning(glue::glue('{dts} DT{dtplural}'))
    }

    tdplural <- ifelse(maxed_td==1,'','s')
    print(glue::glue('{maxed_td} iteration{tdplural} maxed out treedepth'))

    time_msg <- glue::glue('Longest chain took {round(max(runtimes))} seconds')
    if (max(runtimes) < 1.25*min(runtimes)) {
        print(time_msg)
    } else {
        time_msg <- glue::glue('{time_msg}, but shortest took {round(min(runtimes))}')
        warning(time_msg)
    }

    neff_msg <- glue::glue('Worst n_eff was {round(worst_neff$n_eff)} for {worst_neff$par}')
    if (worst_neff$n_eff > 100) {
        print(neff_msg)
    } else {
        n_bad_neff <- sum(smry$n_eff < 100)
        neff_msg <- glue::glue('{neff_msg}. {n_bad_neff} had n_eff < 100')
        warning(neff_msg)
    }

    Rhat_msg <- glue::glue('Worst Rhat was {round(worst_Rhat$Rhat,2)} for {worst_Rhat$par}')
    if (worst_Rhat$Rhat < 1.1) {
        print(Rhat_msg)
    } else {
        n_bad_Rhat <- sum(smry$Rhat > 1.1)
        Rhat_msg <- glue::glue('{Rhat_msg}. {n_bad_Rhat} had Rhat > 1.1')
        warning(Rhat_msg)
    }

    return(NULL)
}

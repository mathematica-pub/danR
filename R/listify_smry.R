#' Convert stan summary (e.g. from get_smry) to named list of posterior means
#'
#' @param smry object to convert
#'
#' @export
#'
listify_smry <- function(smry) {
    parns <- smry$par
    par_vars <- stringr::str_remove(parns,'\\[[0-9,]+\\]')
    vars <- unique(par_vars)
    names(vars) <- vars

    results <- lapply(vars, function(v) {
        pars_touse <- parns[par_vars==v]
        touse <- smry$mean[par_vars==v]
        if (length(touse)==1) {
            return(touse)
        }

        n_dims <- stringr::str_count(pars_touse[1],',')+1

        idxs <- stringr::str_match(pars_touse,'\\[([0-9,]+)\\]')[,2] %>%
            str_split_fixed(',', n=n_dims)
        idxs <- matrix(as.numeric(idxs), nrow=nrow(idxs), ncol=ncol(idxs))
        final_dims <- apply(idxs, 2, max)
        out <- array(NA_real_, final_dims)

        for (i in 1:length(pars_touse)) {
            idx <- idxs[i,]
            #This is so dumb
            eval_string <- glue::glue('out[{paste(idx, collapse=",")}] <- touse[i]')
            eval(parse(text=eval_string))
        }
        return(out)
    })

    return(results)

}

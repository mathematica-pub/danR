#' Convert draws_array from cmdstan to rstan-type named list of parameters, with vectors/matrices/arrays unflattened
#'
#' @param fit cmdstan fit object
#' @param draws draws from cmdstan
#' @param flatten_chains whether to leave iters & chains as separate dimensions or combine
#'
#' @export
#'
lisitfy_draws <- function(fit, draws, flatten_chains=FALSE) {
    vars <- fit$metadata()$stan_variables
    dims <- fit$metadata()$stan_variable_sizes
    classes <- lapply(dims, get_par_class)
    stopifnot(identical(names(dims), vars))

    #Convert from draws array to just array
    class(draws) <- 'array'

    iters <- dim(draws)[1]
    chains <- dim(draws)[2]
    pars <- dim(draws)[3]
    parns <- dimnames(draws)[[3]]
    par_vars <- stringr::str_remove(parns, '\\[[0-9,]+\\]')

    vars_touse <- unique(par_vars)
    stopifnot(length(setdiff(vars_touse, vars))==0)
    names(vars_touse) <- vars_touse

    results <- lapply(vars_touse, function(x) {
        print(x)
        draws_touse <- draws[,,par_vars==x]
        dims_touse <- dims[[x]]
        class_touse <- classes[[x]]

        if (class_touse=='scalar') {
            out <- draws_touse
            dimnames(out) <- NULL
        } else {
            idxs <- stringr::str_match(dimnames(draws_touse)[[3]],'\\[([0-9,]+)\\]')[,2] %>%
                str_split_fixed(',', n=length(dims_touse))
            idxs <- matrix(as.numeric(idxs), nrow=nrow(idxs), ncol=ncol(idxs))


            out <- array(NA_real_, dim=c(dim(draws_touse)[1:2], dims_touse))
            for (i in 1:dim(draws_touse)[3]) {
                idx <- idxs[i,]
                #This is so dumb
                eval_string <- glue::glue('out[,,{paste(idx, collapse=",")}] <- draws_touse[,,i]')
                eval(parse(text=eval_string))
            }
        }

        return(out)
    })

    if(flatten_chains) {
        results <- lapply(results, function(x) {
            dims_touse <- dim(x)
            if (length(dims_touse) == 2) {
                x <- as.vector(x)
            } else {
                x <- array(x, c(dims_touse[1] * dims_touse[2], dims_touse[3:length(dims_touse)]))
            }
            return(x)
        })
    }

    return(results)
}

get_par_class <- function(dims) {
    cdim <- paste(dims,collapse=',')
    if (length(dims)>2) {
        return(paste0('array[', cdim, ']'))
    } else if (length(dims)==2) {
        return(paste0('matrix[', cdim, ']'))
    } else if (dims>1) {
        return(paste0('vector[', cdim, ']'))
    } else {
        return('scalar')
    }
}

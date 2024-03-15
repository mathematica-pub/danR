#' Utility to turn mean/variance into gamma or NB parameters
#'
#' @param mean mean
#' @param var variance
#'
#' @export
#'
calc_distpar <- function(mean, sd) {
    var = sd^2
    return(list(gamma = list(shape = mean^2/var,
                             scale = var/mean,
                             rate  = mean/var),
                nb = list(r = -mean^2/(mean-var),
                          p = mean/var),
                beta = list(alpha = -mean*(mean^2-mean+var)/var,
                            beta  = (mean-1)*(mean^2-mean+var)/var),
                norm = list(mean=mean,
                            sd=sd),
                lognorm = list(mean = log((mean^2/(sqrt(mean^2+var)))),
                               sd   = sqrt(2)*sqrt(log(sqrt(mean^2+var)/mean)))))
}

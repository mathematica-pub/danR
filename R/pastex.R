#' Crosspaste two vectors of strings
#'
#' @param x First string vector
#' @param y Decond string vector
#' @param sep Separator between values of x and y
#' @param collapse If NULL, the default, then returns a vector of each x-y pair;
#' if not null returns a single string, with each x-y pair separated by the value of collapse
#'
#' @export
#'
pastex <- function(x, y, sep='', collapse=NULL) {
    z <- as.vector(t(outer(x, y, FUN='paste', sep=sep)))
    if (!is.null(collapse)) {
        z <- paste(z, collapse=collapse)
    }

    return(z)
}

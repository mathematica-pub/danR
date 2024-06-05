#' Compare variable names in two different datasets
#'
#' @param x First dataset
#' @param y Second dataset
#' @param quiet Whether to print info about overlap
#' @param ret Whether to return list with vector of overlapping vars
#'
#' @export
#'
var_diff <- function(x, y, quiet=FALSE, ret=FALSE) {
    xn <- colnames(x)
    yn <- colnames(y)

    xo <- setdiff(xn, yn)
    yo <- setdiff(yn, xn)
    xy <- intersect(xn, yn)

    if (!quiet) {
        if(length(xo)==0 & length(yo)==0) {
            print(glue::glue('Variable names identical'))
        } else {
            if (length(xy) == 0) {
                print(glue::glue('No shared variables'))
            } else {
                xyp <- ifelse(length(xy)>1,'s','')
                print(glue::glue('{length(xy)} shared variable{xyp}: {paste(xy, collapse=", ")}'))
            }

            if (length(xo) == 0) {
                print(glue::glue('All vars in x also in y'))
            } else {
                xop <- ifelse(length(xo)>1,'s','')
                print(glue::glue('{length(xo)} variable{xop} only in x: {paste(xo, collapse=", ")}'))
            }

            if (length(yo) == 0) {
                print(glue::glue('All vars in y also in x'))
            } else {
                yop <- ifelse(length(yo)>1,'s','')
                print(glue::glue('{length(yo)} variable{yop} only in y: {paste(yo, collapse=", ")}'))
            }
        }
    }

    if (ret) {
        return(list(both=xy,
                    x_only=xo,
                    y_only=yo))
    } else {
        invisible(NULL)
    }

}

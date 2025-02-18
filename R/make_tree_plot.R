#' Utility to make basic CART tree
#'
#' @param x x
#' @param y y
#' @param cp CART's complexity parameter thingy
#' @param pfx Prefix for plot
#' @param print_cp_table whether or not to print out the table of splits
#' @param ... additional arguments passed to rpart::rpart
#'
#' @export
#'

make_tree_plot <- function (y, x, cp=.1, pfx='y', print_cp_table=FALSE, ...) {
    tree_data <- data.frame(x, yhat = y)
    tree_fit <- rpart::rpart(yhat ~ .,data = tree_data, ...)
    if (print_cp_table) {
        print(tree_fit$cptable)
    }

    tree_fit_pruned <- if(is.null(cp)){tree_fit} else {rpart::prune(tree_fit, cp = cp)}
    rpart.plot::prp(tree_fit_pruned, faclen = 0, clip.facs = TRUE,
                    box.palette = "YlGnBl",
                    compress = TRUE, ycompress = TRUE, extra = 101, prefix = paste0(pfx,': '),
                    shadow.col = "gray", type = 4, varlen = 0)
}

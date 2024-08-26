#' Utility to calculate a logit score from a probability
#'
#' @param p vector of probabilities
#'
#' @export
#'
logit <- function(p) {
    log(p/(1-p))
}

#' Utility to calculate the probability form a logit score
#'
#' @param x vector of logits
#'
#' @export
#'
invlogit <- function(x) {
    1/(1+exp(-x))
}

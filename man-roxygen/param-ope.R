#' @param ope the number of observations per 'epoch'. The Sharpe ratio is
#'   typically quoted in 'annualized' units for some epoch, that is, 
#'   'per square root epoch', though returns are observed at a 
#'   frequency of \code{ope} per epoch. The default value is 1, meaning
#'   the observation period is the epoch, and no annualization adjustments
#'   are made.

#' @param ope the number of observations per 'epoch'. For convenience of
#'   interpretation, The Sharpe ratio is typically quoted in 'annualized' 
#'   units for some epoch, that is, 'per square root epoch', though returns 
#'   are observed at a frequency of \code{ope} per epoch. 
#'   The default value is 1, meaning the code will not attempt to guess,
#'   what the observation frequency is, and no annualization adjustments
#'   will be made.
#' @seealso \code{\link{reannualize}}

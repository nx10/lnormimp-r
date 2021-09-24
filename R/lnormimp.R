

#' Fit censored lognormal distribution.
#' (Uses 'fitdistrplus')
#'
#' @param data Numeric data vector
#' @param censn Number of lower and upper censored values: c(lower, upper)
#' @param cutoff Lower and upper cutoff limits: c(lower, upper)
#'
#' @return 'fitdistrplus' distribution fit object
#' @export
#'
lnormimp_fit <- function(data, censn, cutoff) {
  if (length(data) < 2) {
    stop("Not enough data") # fitdistrplus crashes if there is very little data
  }
  if (length(censn) != 2) {
    stop("'censn' has the wrong dimensions")
  }
  if (length(cutoff) != 2) {
    stop("'cutoff' has the wrong dimensions")
  }

  cdat <- data.frame(left = c(rep(NA, censn[1]),
                              rep(cutoff[2], censn[2]),
                              data),
                     right = c(rep(cutoff[1], censn[1]),
                               rep(NA, censn[2]),
                               data))

  return(fitdistrplus::fitdistcens(censdata = cdat, distr = "lnorm"))
}

#' Random draws from an interval of a lognormal distribution.
#' (Uses 'EnvStats')
#'
#' @param n Number of random draws
#' @param fit 'fitdistrplus' distribution fit object
#' @param min Minimum value for sampling
#' @param max Maximum value for sampling
#'
#' @return Vector of sampled values
#' @export
#'
lnormimp_sample <- function(n, fit, min, max) {
  if (n <= 0) {
    return(c())
  }
  return(
    EnvStats::rlnormTrunc(
      n = n,
      meanlog = fit$estimate[[1]],
      sdlog = fit$estimate[[2]],
      min = min,
      max = max
    ))
}


#' Truncated lognormal distribution imputation. (Herbers et al. 2020)
#'
#' @param data Numeric data vector
#' @param censn Number of lower and upper censored values: `c(lower, upper)`
#' @param cutoff Lower and upper cutoff limits: `c(lower, upper)`
#' @param range Lower and upper measurement range: `c(lower, upper)`
#' @param as_list When set to `TRUE`, results will be returned as a list and
#'   separated into original `data`, imputed lower values `imp_lower`, and
#'   imputed upper values `imp_upper`
#'
#' @return 'data' vector with imputed values appended. If `as_list` is set
#'   to `TRUE` results will be returned as a list and separated into original
#'   `data`, imputed lower values `imp_lower`, and imputed upper values
#'   `imp_upper`
#' @export
#'
lnormimp <- function(data, censn, cutoff, range = c(0, Inf), as_list = FALSE) {
  if (length(range) != 2) {
    stop("'range' has the wrong dimensions")
  }
  fit <- lnormimp_fit(data, censn, cutoff)
  imp_lower <- lnormimp_sample(censn[1], fit, range[1], cutoff[1])
  imp_upper <- lnormimp_sample(censn[2], fit, cutoff[2], range[2])
  if (as_list) {
    return(list(
      data = data,
      imp_lower = imp_lower,
      imp_upper = imp_upper
    ))
  }
  return(c(data, imp_lower, imp_upper))
}

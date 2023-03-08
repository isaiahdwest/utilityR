
#' @title Standard Error
#' @description Calculate the standard error of a variable.
#' Calculated as the standard error over the square root of the
#' sample size.
#' @param .x Variable to calculate the standard error for
#' @export
se <- function(.x) {
  sd(.x) / sqrt(length(.x))
}

#' @title Confidence Interval of Mean
#' @description Calculate the confidence interval of a variable.
#' @param .x Variable to calulate the confidence interval for
#' @param cf.perc Confidence interval, i.e. 95% certainty would be 0.95. Default is 0.95
#' @param cf.dist Whether to use a Z-score critical value or a t-score. Default is to check whether there are n > 30 samples in .x, if so a Z-score is used
#' @export

cf.int <- function(.x, cf.perc = 0.95, cf.dist = NULL) {

  if (is.null(cf.dist)) {
    cf.dist <-  ifelse(length(.x) > 30, "z", "t")
  }

  if (cf.dist == "z") {
    crit <- z.critical(cf.perc = cf.perc)
  } else if (cf.dist == "t") {
    crit <- t.critical(length(.x) - 1, cf.perc = cf.perc)
  } else {
    stop("Distribution not supported yet, please use `t` or `z`")
  }

  s.err <- se(.x)
  crit * s.err
}

z.critical <- function(cf.perc) {
  stats::qnorm(0.5 + (cf.perc / 2))
}

t.critical <- function(df, cf.perc = 0.95) {
  # calculate the critical value of t
  qt(1 - (1 - cf.perc) / 2, df)
}

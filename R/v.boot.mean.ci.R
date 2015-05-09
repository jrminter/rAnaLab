#' Compute the mean and confidence interval using the bootstrap
#'
#' A helper function
#'
#' @param x A vector of observations
#' @param nSamples The number of samples (default 999)
#'
#' @return the lower 95 percent CL
#'
#' @export
#'
#' @import boot
#'
v.boot.mean.ci <- function(x, nSamples=999){

  # define the functions we need
  mean.boot <- function(x, idx) {
    # arguments:
    # x data to be resampled
    # idx vector of scrambled indices created
    # by boot() function
    # value:
    # ans mean value computed using resampled
    # data
    ans = mean(x[idx])
    ans
  }

  mean.sd <- function(x, idx) {
    # arguments:
    # x data to be resampled
    # idx vector of scrambled indices created
    # by boot() function
    # value:
    # ans mean value computed using resampled
    # data
    ans = sd(x[idx])
    ans
  }

  x.mean.boot <- boot(x, statistic = mean.boot, R=nSamples)
  x.ci.boot   <- boot.ci(x.mean.boot, conf = 0.95,
                         type=c("norm","perc"))

  v.mean.boot <- x.mean.boot$t0
  v.norm.l.ci <- x.ci.boot$normal[2]
  v.norm.u.ci <- x.ci.boot$normal[3]
  v.perc.l.ci <- x.ci.boot$percent[4]
  v.perc.u.ci <- x.ci.boot$percent[5]

  ret <- c(v.mean.boot, v.norm.l.ci, v.norm.u.ci,
           v.perc.l.ci, v.perc.u.ci)
  names(ret) <- c("mean", "norm.l.ci", "norm.u.ci",
                  "perc.l.ci", "perc.u.ci")
  ret
}

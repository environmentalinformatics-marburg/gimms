#' Compute (pre-whitened) Kendall's tau
#'
#' @description
#' Apply the Mann-Kendall trend test (Mann, 1945) to a series of observations
#' and return Kendall's tau (Kendall, 1938) based on a predefined significance
#' level. In contrast to other readily available implementations, it is left to
#' the user to decide whether or not to apply pre-whitening as described in the
#' \strong{zyp} package vignette (Bronaugh and Werner, 2013).
#'
#' @param x A 'numeric' vector.
#' @param p Significance level to be tested.
#' @param prewhitening 'logical'. If \code{TRUE}, pre-whitening is applied prior
#' to the Mann-Kendall trend test.
#' @param df 'logical'. If \code{TRUE}, a 'data.frame' holding the value of
#' Kendall's tau and the referring significance level.
#' @param ... Further arguments passed on to \code{\link{zyp.trend.vector}}.
#'
#' @return If \code{df = FALSE} (default) and \code{p} was not exceeded, a
#' single 'numeric'; else a 'data.frame' with Kendall's tau and the referring
#' significance level.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{MannKendall}}, \code{\link{zyp.trend.vector}}.
#'
#' @references
#' Kendall, M.G. (1938). A new measure of rank correlation. \emph{Biometrika}
#' 30(1/2), 81-93, doi: 10.2307/2332226. Available online at
#' \url{http://www.jstor.org/stable/2332226} (accessed 2015-11-06).
#'
#' Mann, H.B. (1945). Nonparametric tests against trend. \emph{Econometrica}
#' 13(3), 245-259, doi: 10.2307/1907187. Available online at
#' \url{http://www.jstor.org/stable/1907187} (accessed 2015-11-06).
#'
#' Zhang, X., Vincent, L.A., Hogg, W.D. and A. Niitsoo (2000). Temperature and
#' Precipitation Trends in Canada during the 20th Century. \emph{Atmosphere-Ocean}
#' 38(3), 395-429, doi: 10.1080/07055900.2000.9649654. Available online at
#' \url{http://www.tandfonline.com/doi/abs/10.1080/07055900.2000.9649654}
#' (accessed 2015-11-06).
#'
#' Yue, S., Pilon, P., Phinney, B. and G. Cavadias (2002). The influence of
#' autocorrelation on the ability to detect trend in hydrological series.
#' \emph{Hydrological Processes} 16, 1807-1829, doi: 10.1002/hyp.1095.
#' Available online at \url{http://onlinelibrary.wiley.com/doi/10.1002/hyp.1095/abstract}
#' (accessed 2015-11-06).
#'
#' @examples
#' ## Example taken from ?Kendall::MannKendall
#' library(Kendall)
#' data(PrecipGL)
#' plot(PrecipGL)
#'
#' ## Mann-Kendall trend test without pre-whitening
#' significantTau(PrecipGL, p = 0.001)
#'
#' ## Mann-Kendall trend test with pre-whitening
#' significantTau(PrecipGL, p = 0.001, prewhitening = TRUE)
#'
#' @export significantTau
#' @name significantTau
significantTau <- function(x, p = 0.001, prewhitening = FALSE, df = FALSE, ...) {

  # if only one unique value exists in 'x', return NA
  if (length(unique(x)) == 1)
    return(NA)

  # with prewhitening
  if (prewhitening) {

    # try to compute pre-whitened mann-kendall trend test
    try(mk <- zyp::zyp.trend.vector(x, ...), silent = TRUE)

    # if previous computation fails, return NA
    if (!exists("mk")) {
      sig <- tau <- NA
      # else return kendall's tau and referring p value
    } else {

      id_sig <- grep("sig", names(mk))
      sig <- mk[id_sig]

      id_tau <- grep("tau", names(mk))
      tau <- mk[id_tau]
    }

    # without prewhitening
  } else {
    mk <- Kendall::MannKendall(x)

    sig <- mk$sl
    tau <- mk$tau
  }

  # return data.frame
  if (df) {
    return(data.frame(tau = tau, p = sig))

    # reject value of tau if p >= 0.001
  } else {

    if (is.logical(sig) | is.logical(p)) {
      return(NA)
    } else {
      if (sig >= p) {
        return(NA)
        # keep value of tau if p < 0.001
      } else {
        return(tau)
      }
    }
  }
}

if ( !isGeneric("significantTau") ) {
  setGeneric("significantTau", function(x, ...)
    standardGeneric("significantTau"))
}
#' Compute (Pre-Whitened) Kendall's \eqn{\tau}
#'
#' @description
#' Apply the Mann-Kendall trend test (Mann, 1945) to a series of observations
#' and return Kendall's \eqn{\tau} (Kendall, 1938) based on a predefined
#' significance level. In contrast to other readily available implementations,
#' it is up to the user to decide whether or not to apply pre-whitening as
#' described in the \strong{zyp} package vignette (Bronaugh and Werner, 2013).
#'
#' @param x Either a \code{Raster*} object or a \code{numeric} vector.
#' @param p \code{numeric}, defaults to \code{0.001}. Significance level to be
#' tested.
#' @param prewhitening \code{logical}. If \code{TRUE} (default), pre-whitening
#' is applied prior to the Mann-Kendall trend test.
#' @param method \code{character}. The prewhitening method to apply, see
#' \code{\link{zyp.trend.vector}}.
#' @param df \code{logical}, defaults to \code{FALSE}. If \code{TRUE}, a
#' \code{data.frame} holding the value of Kendall's \eqn{\tau} and the referring
#' significance level.
#' @param filename \code{character}. Optional output filename.
#' @param ... Further arguments passed to \code{\link{writeRaster}}.
#'
#' @return
#' \itemize{
#' \item{\code{numeric} input: }{If \code{df = FALSE} (default), a single
#' \code{numeric} or \code{logical} (i.e., \code{NA}) depending on whether or
#' not 'p' was exceeded; else a \code{data.frame} with Kendall's \eqn{\tau} and
#' the corresponding significance level.}
#' \item{\code{RasterStackBrick} input: }{A \code{RasterLayer} with values of
#' Kendall's \eqn{\tau}. Values exceeding the specified 'p' are discarded.}
#' }
#'
#' @details
#' If available, the function will automatically use open multi-core clusters
#' for parallel processing (see \code{\link{beginCluster}} and Examples).
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
#' x <- as.numeric(PrecipGL)
#' significantTau(x, p = 0.001, prewhitening = FALSE, df = TRUE)
#'
#' ## Mann-Kendall trend test with pre-whitening
#' significantTau(x, p = 0.001, prewhitening = TRUE, df = TRUE)
#'
#' #############################################################################
#' ### use case: significant mann-kendall trends in ndvi3g.v0          #########
#' #############################################################################
#'
#' \dontrun{
#' ## Sample data from 1982 to 2013
#' data("kili3g.v0")
#' rst <- kili3g.v0[[13:nlayers(kili3g.v0)]]
#'
#' ## Remove seasonal signal
#' library(remote)
#' dsn <- deseason(rst, cycle.window = 24)
#'
#' ## Apply trend-free pre-whitened Mann-Kendall test (note that
#' ## non-significant pixels are set to NA)
#' trd1 <- significantTau(dsn, p = 0.01, prewhitening = TRUE)
#' plot(trd1)
#'
#' ## Or, alternatively, use multi-core functionality
#' cores <- parallel::detectCores() - 1
#' if (require(snow)) {
#'   beginCluster(cores)
#'   trd2 <- significantTau(dsn, p = 0.01, prewhitening = TRUE)
#'   endCluster()
#' }
#' }
#'
#' @export significantTau
#' @name significantTau
NULL

################################################################################
### function using 'numeric' input #############################################
#' @aliases significantTau,numeric-method
#' @rdname significantTau
setMethod("significantTau",
          signature(x = "numeric"),
          function(x, p = 0.001, prewhitening = TRUE,
                   method = c("yuepilon", "zhang"), df = FALSE) {

            # if only one unique value exists in 'x', return NA
            if (length(unique(x)) == 1)
              return(NA)

            # with prewhitening
            if (prewhitening) {

              # try to compute pre-whitened mann-kendall trend test
              try(mk <- zyp::zyp.trend.vector(x, method = method[1]), silent = TRUE)

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
)


################################################################################
### function using 'RasterStack' or 'RasterBrick' input ########################
#' @aliases significantTau,RasterStackBrick-method
#' @rdname significantTau
setMethod("significantTau",
          signature(x = "RasterStackBrick"),
          function(x, p = 0.001, prewhitening = TRUE,
                   method = c("yuepilon", "zhang"), filename = "", ...) {

  ## custom tau function passed to calc()
  tau <- function(y) {
    significantTau(y, p, prewhitening, method, df = FALSE)
  }

  ## single-core: calc()
  cl <- try(raster::getCluster(), silent = TRUE)
  if (inherits(cl, "try-error")) {
    raster::calc(x, fun = tau, filename = filename, ...)

  ## multi-core: clusterR()
  } else {
    on.exit(raster::returnCluster())
    parallel::clusterExport(cl, envir = environment(),
                            varlist = c("p", "prewhitening", "method", "tau"))

    raster::clusterR(x, raster::calc, args = list(fun = tau),
                     filename = filename, ...)
  }
})

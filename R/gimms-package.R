#' Download and Process GIMMS3g Data
#'
#' We provide a set of functions to retrieve information about GIMMS NDVI3g
#' files currently available online; download and re-arrange the bi-monthly
#' datasets according to creation time; import downloaded files from native
#' binary (ENVI) format directly into R based on the widely applied 'raster'
#' package; and calculate monthly value composites (e.g. maximum value
#' composites, MVC) from the bi-monthly input data.
#'
#' @name gimms-package
#' @aliases gimmspackage
#' @docType package
#' @title Download and Process GIMMS3g Data
#' @author Florian Detsch
#'
#' @import methods raster parallel
#' @importFrom RCurl getURL
#' @importFrom Kendall MannKendall
#' @importFrom zyp zyp.trend.vector
#' @importFrom utils download.file
#'
#' @references
#' Pinzon, JE & Tucker, CJ (2014). A Non-Stationary 1981-2012 AVHRR
#' NDVI3g Time Series. Remote Sensing, 6(8), 6929-6960. Available online at
#' \url{http://www.mdpi.com/2072-4292/6/8/6929/htm}.
#'
#' Pinzon, JE & Tucker, CJ (2016). A Non-Stationary 1981-2015 AVHRR
#' NDVI3g.v1 Time Series: an update. In preparation for submission to Remote
#' Sensing.
#'
#' @keywords package
#'
NULL
#'
#' @docType data
#' @name bale3g.v1
#' @title Bale Mountains NDVI3g.v1
#' @description Bale Mountains NDVI3g.v1
#' @details This dataset contains NDVI3g.v1 observations for the Bale Mountains
#' National Park, southern Ethiopia (Jul 1981 to Dec 2015).
#' @format \code{raster::RasterStack}
NULL
#'
#' @docType data
#' @name kili3g.v0
#' @title Kilimanjaro NDVI3g.v0
#' @description Kilimanjaro NDVI3g.v0
#' @details This dataset contains NDVI3g.v0 observations for the Kilimanjaro
#' region, northern Tanzania (Jul 1981 to Dec 2013).
#' @format \code{raster::RasterStack}
NULL

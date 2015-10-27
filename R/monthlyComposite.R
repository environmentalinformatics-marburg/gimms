if ( !isGeneric("monthlyComposite") ) {
  setGeneric("monthlyComposite", function(x, ...)
    standardGeneric("monthlyComposite"))
}
#' Calculate monthly composite images
#'
#' @description
#' Based on a user-defined function, e.g. \code{sum} for maximum value
#' composites (MVC), aggregate bi-monthly GIMMS datasets to monthly composites.
#'
#' @param x RasterStack (requires 'indices') or character vector of filenames.
#' If the latter applies and 'pos1', 'pos2' are not specified, the function will
#' try to retrieve monthly indices from \code{\link{monthlyIndices}}. Note that
#' the function does not work with binary data, but expects files that have
#' previously been created via \code{\link{rasterizeGimms}}.
#' @param indices Numeric. Indices indicating RasterLayers or files from
#' identical months; see \code{\link{stackApply}}.
#' @param fun Function. Used to calculate monthly composite layers, defaults to
#' \code{sum}, i.e. MVC; see \code{\link{stackApply}}.
#' @param pos1,pos2 Numeric. If 'x' is a vector of filenames, the first and last
#' element of the date string to build monthly indices from. Defaults to the
#' GIMMS naming convention; see \code{\link{monthlyIndices}} and
#' \code{\link{substr}}.
#' @param ... Further arguments passed on to \code{\link{writeRaster}}.
#'
#' @return
#' A 'RasterStack' object with monthly composite layers.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{stackApply}}, \code{\link{monthlyIndices}}.
#'
#' @examples
#' \dontrun{
#' ## Destination folder for data download
#' gimms_dir <- paste0(getwd(), "/data")
#'
#' ## Download GIMMS NDVI3g binary data from 2000-2005
#' gimms_files <- downloadGimms(x = 2000, y = 2005, dsn = gimms_dir)
#'
#' ## Rasterize downloaded GIMMS files from 2000
#' gimms_raster <- rasterizeGimms(x = gimms_files[1:24], remove_header = TRUE)
#'
#' ## Calculate monthly maximum value composites
#' indices <- monthlyIndices(gimms_files[1:24])
#' gimms_raster_mvc <- monthlyComposite(gimms_raster, indices = indices)
#'
#' plot(gimms_raster_mvc[[1:4]])
#' }
#'
#' @export monthlyComposite
#' @name monthlyComposite

################################################################################
### function using 'RasterStack' ###############################################
#' @aliases monthlyComposite,RasterStack-method
#' @rdname monthlyComposite
setMethod("monthlyComposite",
          signature(x = "RasterStack"),
          function(x, indices, fun = max, ...) {

            ## stop if 'indices' is missing
            if (missing(indices))
              stop("Please supply a valid set of indices, e.g. returned by monthlyIndices().")

            ## immediately run 'stackApply'
            raster::stackApply(x, indices = indices, fun = fun, ...)

          })


################################################################################
### function using 'character' #################################################
#' @aliases monthlyComposite,character-method
#' @rdname monthlyComposite
setMethod("monthlyComposite",
          signature(x = "character"),
          function(x, pos1 = 4L, pos2 = 8L, fun = max, ...) {

            ## extract timestamp from 'x'
            indices <- monthlyIndices(x, pos1 = pos1, pos2 = pos2)

            ## stack files and run 'stackApply'
            rst <- raster::stack(x)
            raster::stackApply(rst, indices = indices, fun = fun, ...)

          })


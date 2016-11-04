if ( !isGeneric("qualityControl") ) {
  setGeneric("qualityControl", function(x, ...)
    standardGeneric("qualityControl"))
}
#' Perform quality control on GIMMS NDVI3g data
#'
#' @description
#' Perform quality control on GIMMS NDVI3g data based on the companion flag
#' information.
#'
#' @param x Typically a \code{list} of 2-layered \code{RasterStack} objects
#' (NDVI and flags) or a single 2-layered \code{RasterStack} object created from
#' \code{\link{rasterizeGimms}}. If a \code{character} vector of filenames is
#' supplied, it is assumed that these represent NDVI layers, and hence, flag
#' layers must be supplied to 'y'.
#' @param y \code{character} vector of filenames, only considered if 'x' is a
#' \code{character} object as well. See 'Details'
#' @param keep Integer. Accepted flag values, defaults to \code{1:2}
#' (\emph{i.e.}, 'good' values; see 'References'.)
#' @param cores Integer. Number of cores for parallel computing.
#' @param ... Arguments passed on to \code{\link{writeRaster}}.
#'
#' @return
#' A quality-controlled 'RasterStack'.
#'
#' @details
#' (to be continued...)
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{rasterizeGimms}}, \code{\link{writeRaster}}.
#'
#' @references
#' \url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00READMEgeo.txt}
#' (accessed on January 15, 2016).
#'
#' @examples
#' \dontrun{
#' ## Download sample data
#' gimms_dir <- paste0(getwd(), "/data")
#'
#' gimms_files <- downloadGimms(x = as.Date("2000-01-01"),
#'                              y = as.Date("2000-06-30"), dsn = gimms_dir)
#'
#' ## Rasterize files
#' gimms_qc <- qualityControl(gimms_files)
#' plot(gimms_qc[[1:4]])
#' }
#'
#' @export qualityControl
#' @name qualityControl

################################################################################
### function using 'RasterStack' or 'RasterBrick' ##############################
#' @aliases qualityControl,RasterStackBrick-method
#' @rdname qualityControl
setMethod("qualityControl",
          signature(x = "RasterStackBrick"),
          function(x, keep = 0:2, ...) {

            ## overlay ndvi and flag layer
            raster::overlay(x[[1]], x[[2]], fun = function(y, z) {
              y[!z[] %in% keep] <- NA
              return(y)
            }, ...)
          })


# ################################################################################
# ### function using 'character' -----
# #' @aliases qualityControl,character-method
# #' @rdname qualityControl
# setMethod("qualityControl",
#           signature(x = "character"),
#           function(x, y, keep = 0:2, cores = 1L, ...) {
#
#             ## check 'cores'
#             cores <- checkCores(cores)
#
#             ## rasterize binary data
#             x <- raster::stack(x)
#             y <- raster::stack(y)
#
#             qualityControl(x = x, flag = flag, keep = keep, cores = cores, ...)
#           })


################################################################################
### function using 'list' -----
#' @aliases qualityControl,list-method
#' @rdname qualityControl
setMethod("qualityControl",
          signature(x = "list"),
          function(x, keep = 0:2, cores = 1L, ...) {

            ## check 'cores'
            cores <- checkCores(cores)

            ## initialize cluster
            cl <- parallel::makePSOCKcluster(cores)

            ## export relevant objects to cluster
            parallel::clusterExport(cl, c("x", "keep"), envir = environment())

            ## perform quality control
            rst <- parallel::parLapply(cl, 1:length(x), function(i) {
              qualityControl(x[[i]], keep = keep, ...)
            })

            ## deregister parallel backend
            parallel::stopCluster(cl)

            ## return quality-controlled raster layer
            return(rst)
          })

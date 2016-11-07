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
#' \code{\link{rasterizeGimms}}. Alternatively, a \code{character} vector of raw
#' GIMMS NDVI3g filenames (see 'Details').
#' @param keep Integer. Accepted flag values, defaults to \code{1:2}
#' (\emph{i.e.}, 'good' values; see 'References'.)
#' @param dsn \code{character}. Destination folder for rasterized files.
#' Defaults to \code{dirname(x)} if not further specified.
#' @param water2na \code{logical}. Determines whether or not to discard pixels
#' with 'mask-water' value (see 'References').
#' @param nodata2na \code{logical}. Determines whether or not to discard pixels
#' with 'mask-nodata' value (see 'References').
#' @param scaling \code{logical}. If \code{TRUE} (default), scaling is enabled
#' and both the NDVI and flag layer per file in 'x' are returned.
#' @param cores Integer. Number of cores for parallel computing.
#' @param ... Arguments passed on to \code{\link{writeRaster}}.
#'
#' @return
#' A quality-controlled 'RasterStack'.
#'
#' @details
#' If 'x' is a \code{character} vector of raw GIMMS NDVI3g filenames,
#' \code{\link{rasterizeGimms}} is automatically invoked prior to performing
#' quality control. Note, however, that in such a case, arguments 'format' and
#' 'overwrite' are not passed on to \code{\link{rasterizeGimms}}. Instead, the
#' default values (\code{format = "GTiff"} and \code{overwrite = FALSE}) are
#' used. For a finer control, run \code{rasterizeGimms(x, ...)} manually prior
#' to quality control.
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
#' gimms_files <- downloadGimms(x = as.Date("2000-01-01"),
#'                              y = as.Date("2000-06-30"))
#'
#' ## Rasterize files
#' gimms_qc <- qualityControl(gimms_files)
#' gimms_qc
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


################################################################################
### function using 'character' -----
#' @aliases qualityControl,character-method
#' @rdname qualityControl
setMethod("qualityControl",
          signature(x = "character"),
          function(x, keep = 0:2, dsn = dirname(x), water2na = TRUE,
                   nodata2na = TRUE, scaling = TRUE, cores = 1L, ...) {

            ## check 'cores'
            cores <- checkCores(cores)

            ## rasterize input files, then apply quality control
            x <- rasterizeGimms(x, dsn, water2na, nodata2na, scaling, cores)

            qualityControl(x = x, keep = keep, cores = cores, ...)
          })


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
            lst <- parallel::parLapply(cl, 1:length(x), function(i) {
              qualityControl(x[[i]], keep = keep, ...)
            })

            rst <- raster::stack(lst); rm(lst)

            ## deregister parallel backend
            parallel::stopCluster(cl)

            ## return quality-controlled raster layer
            return(rst)
          })

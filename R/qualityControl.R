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
#' @param x 'RasterStack' with NDVI values or 'character' vector of ENVI binary
#' files. If the latter applies, \code{flag} is created internally via
#' \code{\link{rasterizeGimms}}, but note that this comes at a high memory cost
#' since the rasterized images cannot be written to disk at the moment.
#' @param flag 'RasterStack' with flag values. Ignored if \code{x} is a
#' 'character' vector.
#' @param keep Integer. Accepted flag values, defaults to \code{1:2}
#' (\emph{i.e.}, 'good' values; see 'References'.)
#' @param cores Integer. Number of cores for parallel computing.
#' @param filename Character. Optional output filename(s); see
#' \code{\link{writeRaster}}. If \code{cores > 1}, the number of supplied
#' filenames must match up with the number of layers in \code{x}.
#' @param ... Further arguments passed on to \code{\link{writeRaster}} (except
#' for 'bylayer' and 'suffix').
#'
#' @return
#' A quality-controlled 'RasterStack'.
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
          function(x, flag, keep = 1:2, cores = 1L, filename = "", ...) {

            ## check 'cores'
            cores <- checkCores(cores)

            ## check number of layers
            if (!identical(raster::nlayers(x), raster::nlayers(flag)))
              stop("Number of layers in 'x' and 'flag' are not identical.\n")

            ### single core --------------------------------------------------------------

            if (cores == 1L) {

              ## overlay raw ndvi and flags
              rst_qc <- raster::overlay(x, flag, fun = function(y, z) {
                y[!z[] %in% keep] <- NA
                return(y)
              }, filename = filename, ...)

              ### multi-core -------------------------------------------------------------

            } else {

              ## initialize cluster
              cl <- parallel::makePSOCKcluster(cores)
              doParallel::registerDoParallel(cl)

              ## loop over layers
              i <- 1
              lst_qc <- foreach::foreach(i = 1:(raster::nlayers(x)),
                                         .packages = c("raster", "rgdal")) %dopar% {

                ## overlay raw ndvi and flags ('RasterLayer'-method)
                qualityControl(x[[i]], flag[[i]], keep = keep,
                               filename = ifelse(length(filename) == raster::nlayers(x),
                                                 filename[i], ""), ...)
              }

              rst_qc <- raster::stack(lst_qc)

              ## deregister parallel backend
              parallel::stopCluster(cl)
            }

            return(rst_qc)
          })


################################################################################
### function using 'RasterLayer' ###############################################
#' @aliases qualityControl,RasterLayer-method
#' @rdname qualityControl
setMethod("qualityControl",
          signature(x = "RasterLayer"),
          function(x, flag, keep = 1:2, filename = "", ...) {

            ## check number of layers
            if (!identical(raster::nlayers(x), raster::nlayers(flag)))
              stop("Number of layers in 'x' and 'flag' are not identical.\n")

            ## overlay raw ndvi and flags
            raster::overlay(x, flag, fun = function(y, z) {
              y[!z[] %in% keep] <- NA
              return(y)
            }, filename = filename, ...)
          })


################################################################################
### function using 'character' #################################################
#' @aliases qualityControl,character-method
#' @rdname qualityControl
setMethod("qualityControl",
          signature(x = "character"),
          function(x, flag, keep = 1:2, cores = 1L, filename = "", ...) {

            ## check 'cores'
            cores <- checkCores(cores)

            ## rasterize binary data
            flag <- rasterizeGimms(x, flag = TRUE, cores = cores)
            x <- rasterizeGimms(x, cores = cores)

            qualityControl(x = x, flag = flag, keep = keep, cores = cores,
                           filename = filename, ...)
          })

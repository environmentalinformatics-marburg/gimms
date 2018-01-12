if ( !isGeneric("qualityControl") ) {
  setGeneric("qualityControl", function(x, ...)
    standardGeneric("qualityControl"))
}
#' Perform Quality Control on GIMMS NDVI3g Data
#'
#' @description
#' Perform quality control on GIMMS NDVI3g data based on the companion flag
#' information. This is merely a wrapper around \code{\link{overlay}} and, since
#' quality control is readily available through \code{\link{rasterizeGimms}}, it
#' is strongly recommended to use the latter function for this purpose.
#'
#' @param x A single 2-layered \code{RasterStack} object (NDVI and flags).
#' @param keep \code{integer}. Accepted flag values (see 'Details').
#' @param filename \code{character}. Optional output filename.
#' @param ... Further arguments passed to \code{\link{writeRaster}}.
#'
#' @return
#' A quality-controlled 'RasterLayer' object.
#'
#' @details
#' If 'keep' is missing, the function will automatically skip quality control
#' and return the input object.
#'
#' @seealso
#' \code{\link{rasterizeGimms}}, \code{\link{overlay}}.
#'
#' @examples
#' \dontrun{
#' tmp <- tempdir()
#'
#' ## Download NDVI3g.v1 sample data
#' ecocast <- system.file("extdata", "inventory_ecv1.rds", package = "gimms")
#' gimms_files <- downloadGimms(readRDS(ecocast)[1], dsn = tmp)
#'
#' ## Import data as 'Raster*' objects
#' ndvi <- raster::raster(gimms_files, varname = "ndvi")
#' ndvi[ndvi[] %in% c(-32768, -3000)] <- NA
#' ndvi <- ndvi / 1e4
#'
#' flag <- floor(raster::raster(gimms_files, varname = "percentile") / 2e3)
#'
#' ## Perform quality control and visualize
#' to_check <- stack(ndvi[[1]], flag[[1]])
#' qcl <- qualityControl(to_check, keep = 0)
#'
#' plot(qcl)
#' }
#'
#' ## see also 'Examples' section in ?rasterizeGimms for automated quality check
#'
#' @export qualityControl
#' @name qualityControl

################################################################################
### function using 'RasterStack' or 'RasterBrick' ##############################
#' @aliases qualityControl,RasterStackBrick-method
#' @rdname qualityControl
setMethod("qualityControl",
          signature(x = "RasterStackBrick"),
          function(x, keep = NULL, filename = "", ...) {

            ## if keep is not specified, return ndvi layer
            if (is.null(keep)) {
              raster::overlay(x[[1]], fun = function(y) {
                return(y)
              }, filename = filename, ...)

            ## else overlay ndvi and flag layer
            } else {
              raster::overlay(x[[1]], x[[2]], fun = function(y, z) {
                y[!z[] %in% keep] <- NA
                return(y)
              }, filename = filename, ...)
            }
          })


# ################################################################################
# ### function using 'character' -----
# #' @aliases qualityControl,character-method
# #' @rdname qualityControl
# setMethod("qualityControl",
#           signature(x = "character"),
#           function(x, keep = NULL, cores = 1L, filename = "", ...) {
#
#             ## check 'cores'
#             cores <- checkCores(cores)
#
#             ## if not specified, use standard values for 'keep'
#             if (missing(keep)) {
#               v <- productVersion(x, TRUE)
#               keep <- if (unique(v) == 0) 1:2 else 0
#             }
#
#             ## rasterize input files, then apply quality control
#             x <- rasterizeGimms(x, dsn, cores)
#
#             qualityControl(x = x, keep = keep, cores = cores,
#                            filename = filename, ...)
#           })
#
#
# ################################################################################
# ### function using 'list' -----
# #' @aliases qualityControl,list-method
# #' @rdname qualityControl
# setMethod("qualityControl",
#           signature(x = "list"),
#           function(x, keep, cores = 1L, filename = "", ...) {
#
#             ## check 'cores'
#             cores <- checkCores(cores)
#
#             ## if not specified, use standard values for 'keep'
#             if (missing(keep)) {
#               nms <- unlist(lapply(x, names))
#               v <- productVersion(nms, TRUE)
#               keep <- if (unique(v) == 0) 1:2 else 0
#             }
#
#             ## initialize cluster
#             cl <- parallel::makePSOCKcluster(cores)
#
#             ## export relevant objects to cluster
#             dots <- list(...)
#             parallel::clusterExport(cl, c("x", "keep", "filename", "dots"),
#                                     envir = environment())
#
#             ## perform quality control
#             lst <- parallel::parLapply(cl, 1:length(x), function(i) {
#
#               fls <- ifelse(length(filename) == 1, filename, filename[i])
#
#               dots_sub <- list(x = x[[i]], keep = keep, filename = fls)
#               dots_sub <- append(dots, dots_sub)
#
#               do.call(qualityControl, args = dots_sub)
#             })
#
#             rst <- raster::stack(lst); rm(lst)
#
#             ## if only one layer was supplied, return 'RasterLayer'
#             if (nlayers(rst) == 1)
#               rst <- unstack(rst)[[1]]
#
#             ## deregister parallel backend
#             parallel::stopCluster(cl)
#
#             ## return quality-controlled raster layer
#             return(rst)
#           })

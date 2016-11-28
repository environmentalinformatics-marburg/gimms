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
#' @param x A single 2-layered \code{RasterStack} object (NDVI and flags).
#' @param keep \code{integer}. Accepted flag values (see 'Details').
#' @param filename \code{character}. Optional output filename(s), see
#' \code{\link{writeRaster}}. If specified, this must be of the same length as
#' 'x'.
#' @param ... Arguments passed to \code{\link{writeRaster}}.
#'
#' @return
#' A quality-controlled 'RasterLayer' object.
#'
#' @details
#' If 'keep' is missing, the function will automatically skip quality control
#' and return the input object. See the 'References' section for more
#' information about product-specific flag values.
#'
#' @seealso
#' \code{\link{overlay}}.
#'
#' @references
#' \url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00READMEgeo.txt}
#' (accessed on January 15, 2016).
#'
#' @examples
#' ## see 'Examples' section in ?rasterizeGimms
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

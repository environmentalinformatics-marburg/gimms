if ( !isGeneric("monthlyComposite") ) {
  setGeneric("monthlyComposite", function(x, ...)
    standardGeneric("monthlyComposite"))
}
#' Calculate Monthly Composite Images
#'
#' @description
#' Based on a user-defined function, e.g. \code{max} for maximum value
#' composites (MVC), aggregate half-monthly GIMMS data sets to monthly composites.
#'
#' @param x Multi-layered \code{Raster*} object or \code{character} vector of
#' filenames. If the latter applies and 'pos1', 'pos2' are not specified, the
#' function will try to retrieve monthly indices from
#' \code{\link{monthlyIndices}}. Note that the specification of NDVI3g.v0 ENVI
#' binary files is hereby not allowed and in such a case,
#' \code{\link{rasterizeGimms}} should be run beforehand.
#' @param indices \code{numeric}. Indices to denote layers or files from
#' identical months.
#' @param fun \code{function}. Used to calculate monthly composite layers,
#' defaults to \code{\link{max}}. Note that a separate 'na.rm' argument is
#' passed down to \code{\link{stackApply}} via '...' and hence should not be
#' included here.
#' @param cores \code{integer}. Number of cores for parallel computing.
#' @param filename \code{character}. Optional output filename passed to
#' \code{\link{stackApply}}.
#' @param version \code{integer} (or any other class convertible to
#' \code{integer}). Specifies GIMMS NDVI3g product version, see 'Details' in
#' \code{\link{updateInventory}}.
#' @param pos1,pos2 \code{integer}. If 'x' is a vector of filenames, the first
#' and last element of the date string to build monthly indices from. Defaults
#' to the 'version'-specific NDVI3g naming convention, see
#' \code{\link{monthlyIndices}}.
#' @param ... Further arguments passed to \code{\link{stackApply}} (i.e.,
#' 'na.rm') and its underlying \code{\link{writeRaster}} call.
#'
#' @return
#' If \code{length(x) == 2}, a single \code{RasterLayer} object, else a
#' \code{RasterStack} object with monthly composite layers.
#'
#' @seealso
#' \code{\link{stackApply}}, \code{\link{monthlyIndices}},
#' \code{\link{writeRaster}}.
#'
#' @examples
#' data("bale3g.v1")
#'
#' ## select layers from 1981 only
#' fls <- system.file("extdata/inventory_ecv1.rds", package = "gimms")
#' fls <- readRDS(fls)[1]
#' rst <- bale3g.v1[[1:12]]
#'
#' ## aggregate to monthly mvc layers
#' mvc <- monthlyComposite(rst, indices = monthlyIndices(fls))
#'
#' @export monthlyComposite
#' @name monthlyComposite

################################################################################
### function using 'RasterStack' or 'RasterBrick' ##############################
#' @aliases monthlyComposite,RasterStackBrick-method
#' @rdname monthlyComposite
setMethod("monthlyComposite",
          signature(x = "RasterStackBrick"),
          function(x, indices, fun = max, cores = 1L, filename = "", ...) {

  ## stop if 'indices' is missing
  if (missing(indices))
    stop("Please supply a valid set of indices, e.g. returned by monthlyIndices().")

  ## check 'cores'
  cores <- checkCores(cores)

  ## initialize cluster
  cl <- parallel::makePSOCKcluster(cores)

  ## export relevant objects to cluster
  dots <- list(...)
  parallel::clusterExport(cl, c("x", "indices", "fun", "filename", "dots"),
                          envir = environment())

  # loop over unique layer indices and apply 'fun'
  lst_out <- parallel::parLapply(cl, unique(indices), function(i) {
    x_sub <- raster::subset(x, which(indices == i))

    dots_sub <- list(x = raster::subset(x, which(indices == i)),
                     fun = fun,
                     indices = rep(1, length(which(indices == i))),
                     filename = filename)
    dots_sub <- append(dots, dots_sub)

    do.call(raster::stackApply, args = dots_sub)
  })

  # deregister parallel backend
  parallel::stopCluster(cl)

  # stack layers
  if (length(lst_out) == 1) {
    lst_out[[1]]
  } else {
    raster::stack(lst_out)
  }
})


################################################################################
### function using 'character' #################################################
#' @aliases monthlyComposite,character-method
#' @rdname monthlyComposite
setMethod("monthlyComposite",
          signature(x = "character"),
          function(x, version = 1L,
                   pos1 = ifelse(version == 1, 15L, 4L),
                   pos2 = ifelse(version == 1, 23L, 8L),
                   fun = max, cores = 1L, filename = "", ...) {

            ## check 'cores'
            cores <- checkCores(cores)

            ## extract timestamp from 'x'
            indices <- monthlyIndices(x, pos1 = pos1, pos2 = pos2)

            ## stack files and run 'monthlyComposite,RasterStackBrick-method'
            rst <- raster::stack(x)
            monthlyComposite(rst, indices = indices, fun = fun, cores = cores,
                             filename = filename, ...)

          })


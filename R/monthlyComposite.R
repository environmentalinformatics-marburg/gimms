if ( !isGeneric("monthlyComposite") ) {
  setGeneric("monthlyComposite", function(x, ...)
    standardGeneric("monthlyComposite"))
}
#' Calculate monthly composite images
#'
#' @description
#' Based on a user-defined function, e.g. \code{max} for maximum value
#' composites (MVC), aggregate half-monthly GIMMS datasets to monthly composites.
#'
#' @param x 'RasterStack' or 'character' vector of filenames. If the latter
#' applies and 'pos1', 'pos2' are not specified, the function will try to
#' retrieve monthly indices from \code{\link{monthlyIndices}}. Note that the
#' function does not work with binary data, but expects files that have
#' previously been created via either \code{\link{rasterizeGimms}} or
#' \code{\link{qualityControl}}.
#' @param indices 'numeric'. Indices to denote layers or files from identical
#' months.
#' @param fun 'function'. Used to calculate monthly composite layers, defaults
#' to \code{max}. Note that a separate 'na.rm' argument is passed down to
#' \code{\link{stackApply}} via '...' and hence should not be included here.
#' @param cores 'integer'. Number of cores for parallel computing.
#' @param filename 'character'. Optional output filename(s), see
#' \code{\link{writeRaster}}. If specified, this must be of the same length as
#' the unique monthly indices.
#' @param pos1,pos2 'numeric'. If 'x' is a vector of filenames, the first and last
#' element of the date string to build monthly indices from. Defaults to the
#' GIMMS naming convention, see \code{\link{monthlyIndices}}.
#' @param ... Further arguments passed on to \code{\link{writeRaster}}.
#'
#' @return
#' If \code{length(x) == 2}, a single 'RasterLayer' object, else a 'RasterStack'
#' object with monthly composite layers.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{stackApply}}, \code{\link{monthlyIndices}},
#' \code{\link{writeRaster}}.
#'
#' @examples
#' \dontrun{
#' ## Download sample data
#' gimms_dir <- paste0(getwd(), "/data")
#'
#' gimms_files <- downloadGimms(x = as.Date("2000-01-01"),
#'                              y = as.Date("2000-12-31"), dsn = gimms_dir)
#'
#' ## Rasterize files
#' gimms_raster <- rasterizeGimms(x = gimms_files, remove_header = TRUE)
#'
#' ## Create monthly maximum value composites
#' indices <- monthlyIndices(gimms_files)
#' gimms_raster_mvc <- monthlyComposite(gimms_raster, indices = indices)
#'
#' ## Visualize data
#' library(sp)
#' names(gimms_raster_mvc) <- paste(month.abb, 2000)
#' spplot(gimms_raster_mvc)
#' }
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
                     filename = ifelse(length(filename) == length(unique(indices)),
                                       filename[which(unique(indices) == i)], ""))
    dots_sub <- append(dots, dots_sub)

    do.call(raster::stackApply, args = dots_sub)

    # raster::stackApply(raster::subset(x, which(indices == i)),
    #                    fun = fun,
    #                    indices = rep(1, length(which(indices == i))),
    #                    filename = ifelse(length(filename) == length(unique(indices)),
    #                                      filename[which(unique(indices) == i)], ""),
    #                    ...)
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
          function(x, pos1 = 4L, pos2 = 8L, fun = max, cores = 1L,
                   filename = "", ...) {

            ## check 'cores'
            cores <- checkCores(cores)

            ## extract timestamp from 'x'
            indices <- monthlyIndices(x, pos1 = pos1, pos2 = pos2)

            ## stack files and run 'monthlyComposite,RasterStackBrick-method'
            rst <- raster::stack(x)
            monthlyComposite(rst, indices = indices, fun = fun, cores = cores,
                             filename = filename, ...)

          })


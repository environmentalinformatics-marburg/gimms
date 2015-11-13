#' Rasterize GIMMS NDVI3g binary data
#'
#' @description
#' Import GIMMS NDVI3g binary data into R as 'Raster*' objects based on a
#' companion header file.
#'
#' @param x Character. Vector of local filepaths.
#' @param header Character. Companion header files corresponding to the binary data
#' in 'x'. If missing, the standard header file for GIMMS NDVI3g binary data as
#' created by \code{\link{createHeader}} will be used.
#' @param water2na Logical. If \code{TRUE} (default), pixels with 'mask-water'
#' value (-10000) will be discarded. See also
#' \url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00READMEgeo.txt}.
#' @param nodata2na Logical. If \code{TRUE} (default), pixels with 'mask-nodata'
#' value (-5000) will be discarded.
#' @param scaling Logical. If \code{TRUE} (default), initial values will be
#' scaled by a factor of 1/10000.
#' @param remove_header Logical. If \code{FALSE} (default), the header file
#' specified in 'header' or, if not specified, created internally via
#' \code{\link{createHeader}} will be removed after all operations have finished.
#' @param filename Character. Optional output filename, see
#' \code{\link{writeRaster}}.
#' @param ... Further arguments passed on to \code{\link{writeRaster}}.
#'
#' @return
#' If 'x' is a single filename, an object of class 'RasterLayer';
#' if 'x' is a vector of filenames, an object of class 'RasterStack'.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{createHeader}}, \code{\link{raster}}, \code{\link{writeRaster}}.
#'
#' @examples
#' \dontrun{
#' ## Destination folder for data download
#' gimms_dir <- paste0(getwd(), "/data")
#'
#' ## Download GIMMS NDVI3g binary data from 2000-2005
#' gimms_files <- downloadGimms(x = 2000, y = 2005, dsn = gimms_dir)
#'
#' ## Rasterize downloaded GIMMS files from January and February 2000
#' gimms_raster <- rasterizeGimms(x = gimms_files[1:4], remove_header = TRUE)
#'
#' plot(gimms_raster)
#' }
#'
#' @export rasterizeGimms
#' @name rasterizeGimms
rasterizeGimms <-   function(x,
                             header = NULL,
                             water2na = TRUE,
                             nodata2na = TRUE,
                             scaling = TRUE,
                             remove_header = FALSE,
                             filename = '',
                             ...) {

  ## stop if 'x' and 'filename' are of unequal length
  if (length(x) != length(filename) & all(nchar(filename)) > 0)
    stop("Parameters 'x' and 'filename' are of unequal length.")

  ## loop over files in 'x'
  ls_rst <- lapply(1:length(x), function(i) {

    # import binary data as 'RasterLayer' and flip
    if (is.null(header))
      header <- createHeader(x[i])

    rst <- raster::raster(x[i])
    rst <- raster::t(rst)

    # set extent and projection
    ext <- raster::extent(c(-180 + 1/24, 180 - 1/24, -90 + 1/24, 90 - 1/24))
    rst <- raster::setExtent(rst, ext)
    projection(rst) <- "+init=epsg:4326"

    # discard 'water-mask' pixels (optional)
    if (water2na)
      rst[rst[] == -10000] <- NA

    # discard 'nodata-mask' pixels (optional)
    if (nodata2na)
      rst[rst[] == -5000] <- NA

    # scale values (optional)
    if (scaling)
      rst <- rst / 10000

    # store output (optional)
    if (length(filename) >= i & nchar(filename[i]) > 0)
      rst <- raster::writeRaster(rst, filename = filename[i], ...)

    # if not otherwise specified, remove temporary header file
    if (remove_header)
      file.remove(header)

    # return raster
    return(rst)
  })

  ## return 'RasterLayer' (single file in 'x') or 'RasterStack'
  if (length(ls_rst) == 1) {
    return(ls_rst[[1]])
  } else {
    return(stack(ls_rst))
  }
}

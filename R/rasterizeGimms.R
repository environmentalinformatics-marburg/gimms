#' Rasterize GIMMS NDVI3g Data
#'
#' @description
#' Import GIMMS NDVI3g (binary or NetCDF) data into R as \code{Raster*} objects.
#'
#' @param x \code{character}. Vector of local filepaths. Note that product
#' versions must not be mixed, i.e. 'x' should represent files originating from
#' either NDVI3g.v1 or NDVI3g.v0 only.
#' @param ext \code{Extent}, or any object from which an \code{Extent} can be
#' extracted, see \code{\link[raster]{crop}}.
#' @param snap \code{character}, defaults to "out". Other available options are
#' "in" and "near", see \code{\link[raster]{crop}}.
#' @param keep \code{integer}. Flag values of NDVI3g pixels to spare during
#' quality control. Pixels with non-included flag values are set to \code{NA}.
#' If not specified (i.e., \code{NULL}; default), quality control is skipped.
#' @param split \code{logical}, defaults to \code{FALSE}. If \code{TRUE}, a
#' \code{list} of \code{RasterStack} objects (of \code{length(x)}) is returned
#' rather than a single \code{RasterStack}.
#' @param cores \code{integer}. Number of cores for parallel computing.
#' @param filename \code{character}. Optional output filename. If specified,
#' this must be of the same length as 'x'.
#' @param ... Further arguments passed to \code{\link{writeRaster}}.
#'
#' @return
#' If \code{split = TRUE}, a list of NDVI3g \code{RasterStack} objects
#' corresponding to the files specified in 'x'; else a single NDVI3g
#' \code{RasterStack} object.
#'
#' @seealso
#' \code{\link[raster]{crop}}, \code{\link{qualityControl}},
#' \code{\link{writeRaster}}.
#'
#' @examples
#' \dontrun{
#' tmp <- tempdir()
#'
#' ## Download NDVI3g.v1 sample data
#' gimms_files <- downloadGimms(x = as.Date("2000-01-01"),
#'                              y = as.Date("2000-12-31"),
#'                              dsn = tmp)
#'
#' ## Extent for clipping
#' shp <- getData("GADM", country = "DEU", level = 0, path = tmp)
#'
#' ## Rasterize without quality control
#' gimms_raster <- rasterizeGimms(x = gimms_files,
#'                                ext = shp) # clipping
#' plot(gimms_raster[[1]])
#' lines(shp)
#'
#' ## Rasterize with quality control
#' gimms_rasterq <- rasterizeGimms(x = gimms_files,
#'                                 ext = shp, # clipping
#'                                 keep = 0)  # quality control
#' plot(gimms_rasterq[[1]])
#' lines(shp)
#' }
#'
#' @export rasterizeGimms
#' @name rasterizeGimms
rasterizeGimms <- function(x,
                           ext = NULL,
                           snap = "out",
                           keep = NULL,
                           split = FALSE,
                           cores = 1L,
                           filename = "",
                           ...) {

  ## check 'cores'
  cores <- checkCores(cores)

  ## check 'filename'
  filename <- checkFls(x, filename)

  ## determine product version
  version <- productVersion(x, uniform = TRUE)

  if (all(version == 0)) {
    rasterizeGimmsV0(x, ext, snap, keep, split, cores, filename, ...)
  } else {
    rasterizeGimmsV1(x, ext, snap, keep, split, cores, filename, ...)
  }
}


################################################################################
### rasterize function for version 0 -----
rasterizeGimmsV0 <- function(x,
                             ext = NULL,
                             snap = "out",
                             keep = NULL,
                             split = FALSE,
                             cores = 1L,
                             filename = "",
                             ...) {

  ## initialize cluster
  cl <- parallel::makePSOCKcluster(cores)

  ## create header files
  headers <- createHeader(x)

  ## export relevant objects to cluster
  dots <- list(...)
  parallel::clusterExport(cl, c("x", "ext", "snap", "keep", "filename", "dots"),
                          envir = environment())

  ## loop over files in 'x'
  lst <- parallel::parLapply(cl, 1:length(x), function(i) {

    # import binary data as 'RasterLayer' and flip
    rst <- raster::raster(x[i])
    rst <- raster::t(rst)

    # set extent and projection
    ref <- raster::extent(c(-180, 180, -90, 90))
    rst <- raster::setExtent(rst, ref)
    raster::projection(rst) <- "+init=epsg:4326"

    # clip images (optional)
    if (!is.null(ext))
      rst <- raster::crop(rst, ext, snap = snap)

    # discard 'water-mask' (-10000) and 'nodata-mask' pixels (-5000)
    for (z in c(-10000, -5000)) {
      val <- raster::getValues(rst)
      ids <- which(val == z)
      val[ids] <- NA
      rst <- raster::setValues(rst, val)
    }

    # retrieve ndvi and flags
    ndvi <- floor(rst/10) / 1000
    flag <- rst - floor(rst/10) * 10 + 1

    rst <- raster::stack(ndvi, flag)
    names(rst) <- c("ndvi", "flag")

    # carry out quality control (optional)
    rst <- if (!is.null(keep)) {
      qualityControl(rst, keep = keep)
    } else {
      rst[[1]]
    }

    # write to file (optional)
    if (nchar(filename[i]) > 0) {
      dots_sub <- list(x = rst, filename = filename[i])
      dots_sub <- append(dots, dots_sub)

      do.call(raster::writeRaster, args = dots_sub)
    }

    return(rst)
  })

  ## remove header files
  file.remove(headers)

  ## deregister parallel backend
  parallel::stopCluster(cl)

  ## return (list of) ndvi raster stack(s)
  if (!split) {
    return(raster::stack(lst))
  } else {
    return(lst)
  }
}


################################################################################
### rasterize function for version 1 -----
rasterizeGimmsV1 <- function(x,
                             ext = NULL,
                             snap = "out",
                             keep = NULL,
                             split = FALSE,
                             cores = 1L,
                             filename = "",
                             ...) {

  ## initialize cluster
  cl <- parallel::makePSOCKcluster(cores)

  ## export relevant objects to cluster
  dots <- list(...)
  parallel::clusterExport(cl, c("ext", "snap", "keep", "filename", "dots"),
                          envir = environment())

  ## loop over files in 'x'
  lst <- lapply(1:length(x), function(i) {

    ndvi <- raster::stack(x[i], varname = "ndvi")
    flag <- raster::stack(x[i], varname = "percentile")

    ## export relevant sub-objects to cluster
    parallel::clusterExport(cl, c("ndvi", "flag"), envir = environment())

    ## loop over half-monthly layers
    nc4 <- parallel::parLapply(cl, 1:(raster::nlayers(ndvi)), function(j) {

      # clip images (optional)
      rst_ndvi <- ndvi[[j]]
      rst_flag <- flag[[j]]

      if (!is.null(ext)) {
        rst_ndvi <- raster::crop(rst_ndvi, ext, snap = snap)
        rst_flag <- raster::crop(rst_flag, ext, snap = snap)
      }

      # discard 'water-mask' (-32768) and 'nodata-mask' pixels (-3000)
      for (z in c(-32768, -3000)) {
        val <- raster::getValues(rst_ndvi)
        ids <- which(val == z)
        val[ids] <- NA
        rst_ndvi <- raster::setValues(rst_ndvi, val)
      }

      # apply scale factor and add 'flag' layer to raster stack
      rst_ndvi <- rst_ndvi / 10000
      rst_flag <- floor(rst_flag / 2000)

      rst <- raster::stack(rst_ndvi, rst_flag)
      names(rst) <- c("ndvi", "flag")

      # carry out quality control (optional)
      if (!is.null(keep)) {
        qualityControl(rst, keep = keep)
      } else {
        rst[[1]]
      }
    })

    nc4 <- raster::stack(nc4)

    ## write to file
    if (nchar(filename[i]) > 0)
      nc4 <- writeRaster(nc4, filename[i], ...)

    return(nc4)
  })

  ## deregister parallel backend
  parallel::stopCluster(cl)

  ## return (list of) ndvi raster stack(s)
  if (!split) {
    return(raster::stack(lst))
  } else {
    return(lst)
  }
}

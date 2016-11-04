#' Rasterize GIMMS NDVI3g data
#'
#' @description
#' Import GIMMS NDVI3g (binary or NetCDF) data into R as \code{Raster*} objects.
#'
#' @param x \code{character}. Vector of local filepaths.
#' @param dsn \code{character}. Destination folder for rasterized files.
#' Defaults to \code{dirname(x)} if not further specified.
#' @param water2na \code{logical}. Determines whether or not to discard pixels
#' with 'mask-water' value (see 'References').
#' @param nodata2na \code{logical}. Determines whether or not to discard pixels
#' with 'mask-nodata' value (see 'References').
#' @param scaling \code{logical}. If \code{TRUE} (default), scaling is enabled
#' and both the NDVI and flag layer per file in 'x' are returned.
#' @param cores \code{integer}. Number of cores for parallel computing.
#' @param format \code{character}. Output file type, see
#' \code{\link{writeRaster}}.
#' @param overwrite \code{logical}. If \code{TRUE}, existing output files will
#' be overwritten, see \code{\link{writeRaster}}.
#'
#' @return
#' If \code{scaling = TRUE} and \code{length(x) > 1}, a \code{list} of
#' \code{length(x)} consisting of 2-layered \code{RasterStack} objects (NDVI and
#' flags);
#'
#' Else if \code{scaling = TRUE} and \code{length(x) == 1}, a 2-layered
#' \code{RasterStack} objects (NDVI and flags);
#'
#' Else if \code{scaling = FALSE}, a \code{list} of \code{length(x)} consisting
#' of \code{RasterLayer} objects with raw GIMMS NDVI3g values.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{writeRaster}}.
#'
#' @references
#' \url{https://nex.nasa.gov/nex/projects/1349/wiki/general_data_description_and_access/}
#' (accessed on 11/04/2016).
#' \url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00READMEgeo.txt}
#' (accessed on 11/04/2016).
#'
#' @examples
#' \dontrun{
#' ## Download sample data
#' gimms_files <- downloadGimms(x = as.Date("2000-01-01"),
#'                              y = as.Date("2000-06-30"))
#'
#' ## Rasterize files
#' gimms_raster <- rasterizeGimms(x = gimms_files)
#' gimms_raster
#' }
#'
#' @export rasterizeGimms
#' @name rasterizeGimms
rasterizeGimms <- function(x,
                           dsn = dirname(x),
                           water2na = TRUE,
                           nodata2na = TRUE,
                           scaling = TRUE,
                           cores = 1L,
                           format = "GTiff",
                           overwrite = FALSE) {

  ## check 'cores'
  cores <- checkCores(cores)

  ## determine product version
  version <- if (unique(substr(x[1], 1, 3)) == "geo") 0 else 1

  if (version == 0) {
    rasterizeGimmsV0(x, dsn, water2na, nodata2na, scaling, cores, format,
                     overwrite)
  } else {
    rasterizeGimmsV1(x, dsn, water2na, nodata2na, scaling, cores, format,
                     overwrite)
  }
}


################################################################################
### rasterize function for version 0 -----
rasterizeGimmsV0 <- function(x,
                             dsn = dirname(x),
                             water2na = TRUE,
                             nodata2na = TRUE,
                             scaling = TRUE,
                             cores = 1L,
                             format = "GTiff",
                             overwrite = FALSE) {

  ## stop if 'x' and 'filename' are of unequal length
  if (length(x) != length(filename) & all(nchar(filename)) > 0)
    stop("Parameters 'x' and 'filename' are of unequal length.")

  ## check 'cores'
  cores <- checkCores(cores)

  ## initialize cluster
  cl <- parallel::makePSOCKcluster(cores)

  ## create header files
  headers <- createHeader(x)

  fls <- paste(dsn, basename(x), sep = "/")
  fls_ext <- raster:::.getExtension(fls, format)

  ## export relevant objects to cluster
  parallel::clusterExport(cl, c("x", "water2na", "nodata2na", "scaling",
                                "fls", "fls_ext", "format", "overwrite"),
                          envir = environment())

  ## loop over files in 'x'
  lst <- parallel::parLapply(cl, 1:length(x), function(i) {

    # import binary data as 'RasterLayer' and flip
    rst <- raster::raster(x[i])
    rst <- raster::t(rst)

    # set extent and projection
    ext <- raster::extent(c(-180, 180, -90, 90))
    rst <- raster::setExtent(rst, ext)
    raster::projection(rst) <- "+init=epsg:4326"

    # discard 'water-mask' pixels (optional)
    if (water2na) {
      val <- raster::getValues(rst)
      ids <- which(val == -10000)
      val[ids] <- NA
      rst <- raster::setValues(rst, val)
    }

    # discard 'nodata-mask' pixels (optional)
    if (nodata2na) {
      val <- raster::getValues(rst)
      ids <- which(val == -5000)
      val[ids] <- NA
      rst <- raster::setValues(rst, val)
    }

    # scale values (optional)
    if (scaling) {
      rst <- floor(rst/10) / 1000          ## ndvi
      rst <- raster::stack(rst, floor(rst/10) * 10 + 1)  ## flags
      names(rst) <- c("ndvi", "flag")

      rst <- try(raster::writeRaster(rst, filename = fls_ext[i], format = format,
                                     overwrite = overwrite, bylayer = TRUE,
                                     suffix = "names"), silent = TRUE)

      if (class(rst) == "try-error")
        rst <- raster::stack(
          raster:::.getExtension(paste0(fls[i], c("_ndvi", "_flag")), format)
        )

    } else {
      rst <- try(raster::writeRaster(rst, filename = fls_ext[i], format = format,
                                     overwrite = overwrite), silent = TRUE)

      if (class(rst) == "try-error")
        rst <- raster::stack(raster:::.getExtension(fls[i], format))
    }

    # return raster
    return(rst)
  })

  # remove temporary header file
  file.remove(headers)

  ## deregister parallel backend
  parallel::stopCluster(cl)

  ## return list with ndvi/flag raster stacks
  if (length(lst) == 1) {
    return(lst[[1]])
  } else {
    return(lst)
  }
}


################################################################################
### rasterize function for version 1 -----
rasterizeGimmsV1 <- function(x,
                             dsn = gsub(".nc4$", "", x),
                             water2na = TRUE,
                             nodata2na = TRUE,
                             scaling = TRUE,
                             cores = 1L,
                             format = "GTiff",
                             overwrite = FALSE) {

  ## import version 1 dates
  dates <- readRDS(system.file("extdata", "dates_ecv1.rds", package = "gimms"))

  if (length(dsn) == 1)
    dsn <- rep(dsn, length(x))

  ## check 'cores'
  cores <- checkCores(cores)

  ## initialize cluster
  cl <- parallel::makePSOCKcluster(cores)

  ## export relevant objects to cluster
  parallel::clusterExport(cl, c("water2na", "nodata2na", "scaling", "format",
                                "overwrite"), envir = environment())

  ## loop over files in 'x'
  lst <- lapply(1:length(x), function(i) {

    ndvi <- raster::stack(x[i], varname = "ndvi")
    flag <- raster::stack(x[i], varname = "percentile")

    if (!dir.exists(dsn[i]))
      dir.create(dsn[i])

    dts <- dates[[grep(basename(x[i]), names(dates))]]
    fls <- format(dts, "%m%d")
    fls <- paste0(dsn[i], "/", paste(strsplit(basename(x[i]), "_")[[1]][1:4],
                                     collapse = "_"), fls)

    ## export relevant sub-objects to cluster
    parallel::clusterExport(cl, c("ndvi", "flag", "fls"), envir = environment())

    ## loop over half-monthly layers
    parallel::parLapply(cl, 1:(raster::nlayers(ndvi)), function(j) {

      # discard 'water-mask' pixels (optional)
      if (water2na) {
        val <- raster::getValues(ndvi[[j]])
        ids <- which(val == -32768)
        val[ids] <- NA; rm(ids)
        rst <- raster::setValues(ndvi[[j]], val); rm(val)
      }

      # discard 'nodata-mask' pixels (optional)
      if (nodata2na) {
        val <- raster::getValues(if (water2na) rst else ndvi[[j]])
        ids <- which(val == -3000)
        val[ids] <- NA; rm(ids)
        rst <- raster::setValues(if (water2na) rst else ndvi[[j]], val); rm(val)
      }

      # if scaling, apply scale factor and add 'flag' layer to raster stack
      if (scaling) {
        rst <- (if (any(water2na, nodata2na)) rst else ndvi[[j]]) / 10000
        rst <- raster::stack(rst, floor(flag[[j]] / 2000))
        names(rst) <- c("ndvi", "flag")

        rst <- try(raster::writeRaster(rst, filename = fls[j], format = format,
                                       overwrite = overwrite, bylayer = TRUE,
                                       suffix = "names"), silent = TRUE)

        if (class(rst) == "try-error")
          rst <- raster::stack(
            raster:::.getExtension(paste0(fls[j], c("_ndvi", "_flag")), format)
          )

        # else proceed with raw values
      } else {
        rst <- if (any(water2na, nodata2na)) rst else ndvi[[j]]

        rst <- try(raster::writeRaster(rst, filename = fls[j], format = format,
                                       overwrite = overwrite), silent = TRUE)

        if (class(rst) == "try-error")
          rst <- raster::stack(raster:::.getExtension(fls[j], format))
      }

      return(rst)
    })
  })

  ## deregister parallel backend
  parallel::stopCluster(cl)

  ## return list with ndvi/flag raster stacks
  return(unlist(lst))
}

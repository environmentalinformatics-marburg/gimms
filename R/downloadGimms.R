if ( !isGeneric("downloadGimms") ) {
  setGeneric("downloadGimms", function(x, y, ...)
    standardGeneric("downloadGimms"))
}
#' Download GIMMS NDVI3g Data
#'
#' @description
#' Download GIMMS NDVI3g data, optionally for a given period of time. NDVI3g.v1 
#' (NetCDF, until end 2015) is available from the \code{"poles"} and 
#' \code{"ecocast"} servers, whereas NDVI3g.v0 (ENVI binary, until end 2013) is 
#' \code{"ecocast"} and \code{"nasanex"} exclusive.
#'
#' @param x Start time for data download as either \code{Date} object (e.g.,
#' \code{as.Date("2000-01-01")}) or \code{numeric} year (e.g., \code{2000}).
#' Alternatively, a \code{character} vector of online filepaths to download
#' created from \code{\link{updateInventory}}. If \code{missing}, all files
#' available online are being downloaded.
#' @param y End time for data download as either \code{Date} object or
#' \code{numeric} year. Ignored if 'x' is a \code{character} object or missing.
#' @param version \code{integer} (or any other convertible class), defaults to
#' \code{1L}. Specifies desired GIMMS NDVI3g product version, see 'Details' in
#' \code{\link{updateInventory}}. Ignored if 'x' is a \code{character} object.
#' @param dsn \code{character}, defaults to the current working directory.
#' Target folder for file download.
#' @param overwrite \code{logical}, defaults to \code{FALSE}. If \code{TRUE},
#' identically named files in 'dsn' will be overwritten.
#' @param quiet \code{logical}. If \code{TRUE} (default), console output is
#' reduced.
#' @param mode \code{character}. See \code{\link{download.file}}.
#' @param cores \code{integer}, defaults to \code{1L}. Number of cores used for
#' parallel processing. Note that a fast internet connection is required in
#' order for parallelization to take effect.
#' @param server See \code{\link[gimms]{updateInventory}}.
#' @param ... Further arguments passed to \code{\link{download.file}}, e.g.
#' 'method'.
#'
#' @return
#' A \code{character} vector of local filepaths.
#'
#' @seealso
#' \code{\link{updateInventory}}, \code{\link{download.file}}.
#'
#' @examples
#' \dontrun{
#' tmp <- tempdir()
#'
#' ## 'Date' method
#' gimms_files_date <- downloadGimms(x = as.Date("2000-01-01"),
#'                                   y = as.Date("2000-12-31"),
#'                                   dsn = tmp)
#'
#' ## 'numeric' method (i.e., particular years)
#' gimms_files_year <- downloadGimms(x = 2000,
#'                                   y = 2002,
#'                                   dsn = tmp)
#'
#' ## 'character' method (i.e., particular files)
#' ecocast <- system.file("extdata", "inventory_ecv1.rds", package = "gimms")
#' gimms_files_char <- readRDS(ecocast)
#' gimms_files_char <- downloadGimms(x = gimms_files_char[1:6],
#'                                   dsn = tmp)
#'
#' ## 'missing' method (i.e., entire collection)
#' gimms_files_full <- downloadGimms(dsn = tmp)
#' }
#'
#' @export downloadGimms
#' @name downloadGimms

################################################################################
### 'Date' input ----

setClassUnion("dateORmissing", c("Date", "missing"))

#' @aliases downloadGimms,Date-method
#' @rdname downloadGimms
setMethod(
  "downloadGimms"
  , signature(
    x = "dateORmissing"
    , y = "dateORmissing"
  )
  , function(
    x
    , y
    , version = 1L
    , dsn = getwd()
    , overwrite = FALSE
    , quiet = TRUE
    , mode = "wb"
    , cores = 1L
    , server = c("poles", "nasanex", "ecocast")
    , ...
  ) {
    
    ## available files and corresponding dates
    fls <- updateInventory(server = server, version = version)
    dts <- monthlyIndices(fls, version = attr(fls, "version"), timestamp = TRUE)
    
    ## start (finish) with the first (last) timestamp available if 'x'
    ## ('y') is not specified
    if (missing(x)) x <- dts[1]
    if (missing(y)) y <- dts[length(dts)]
    
    ## select files covering user-defined temporal range...
    usr_dts <- seq(x, y, 1)
    usr_dts <- usr_dts[which(substr(usr_dts, 9, 10) %in% c("01", "15"))]
    
    ## ...from version 0
    fls <- if (attr(fls, "version") == 0) {
      fls[dts %in% usr_dts]
      
      ## ...from version 1
    } else {
      # identify .nc4 files with at least 1 required date
      lst <- readRDS(
        system.file(
          "extdata"
          , switch(
            attr(fls, "server")
            , "ecocast" = "dates_ecv1.rds"
            , "poles"   = "dates_plv1.rds"
          )
          , package = "gimms"
        )
      )
      
      mat <- sapply(usr_dts, function(i) {
        sapply(lst, function(j) any(i == j))
      })
      rownames(mat)[rowSums(mat) > 0]
    }
    
    ## download files
    downloader(fls, dsn = dsn, overwrite = overwrite,
               quiet = quiet, mode = mode, cores = cores, ...)
  }
)


################################################################################
### 'numeric' input (i.e. years) ----

setClassUnion("numericORmissing", c("numeric", "missing"))

#' @aliases downloadGimms,numeric-method
#' @rdname downloadGimms
setMethod(
  "downloadGimms"
  , signature(
    x = "numericORmissing"
    , y = "numericORmissing"
  )
  , function(
    x
    , y
    , version = 1L
    , dsn = getwd()
    , overwrite = FALSE
    , quiet = TRUE
    , mode = "wb"
    , cores = 1L
    , server = c("poles", "nasanex", "ecocast")
    , ...
  ) {
    
    ## available files
    fls <- updateInventory(server = server, version = version)
    
    ## if specified, subset available files by time frame
    bsn <- basename(fls)
    yrs <- substr(bsn, ifelse(attr(fls, "version") == 1, 15, 4),
                  ifelse(attr(fls, "version") == 1, 18, 5))
    yrs <- as.numeric(yrs)
    
    ## if version 0, add leading century
    if (attr(fls, "version") == 0) {
      id_old <- yrs >= 81
      id_new <- yrs <= 80
      yrs[id_old] <- paste0("19", yrs[id_old])
      yrs[id_new] <- paste0("20", formatC(as.integer(yrs[id_new]),
                                          width = 2, flag = "0"))
      yrs <- as.numeric(yrs)
    }
    
    ## start (finish) with the first (last) year available if 'x' ('y')
    ## is not specified
    if (missing(x)) x <- yrs[1]
    if (missing(y)) y <- yrs[length(yrs)]
    
    ## subset files
    fls <- fls[yrs %in% seq(x, y)]
    
    ## download
    downloader(fls, dsn = dsn, overwrite = overwrite,
               quiet = quiet, mode = mode, cores = cores, ...)
  }
)


################################################################################
### 'character' input (i.e. files) ----

#' @aliases downloadGimms,character-method
#' @rdname downloadGimms
setMethod(
  "downloadGimms"
  , signature(
    x = "character"
    , y = "ANY"
  )
  , function(
    x
    , dsn = getwd()
    , overwrite = FALSE
    , quiet = TRUE
    , mode = "wb"
    , cores = 1L
    , ...
  ) {
    
    ## download
    downloader(x, dsn = dsn, overwrite = overwrite,
               quiet = quiet, mode = mode, cores = cores, ...)
  }
)


################################################################################
### 'missing' input (i.e. download entire collection) ----
#' @aliases downloadGimms,missing-method
#' @rdname downloadGimms
setMethod(
  "downloadGimms"
  , signature(
    x = "missing"
    , y = "missing"
  )
  , function(
    version = 1L
    , dsn = getwd()
    , overwrite = FALSE
    , quiet = TRUE
    , mode = "wb"
    , cores = 1L
    , server = c("poles", "nasanex", "ecocast")
    , ...
  ) {
    
    ## download all available files
    downloader(updateInventory(server = server, version = version), dsn = dsn,
               overwrite = overwrite, quiet = quiet, mode = mode,
               cores = cores, ...)
  }
)

if ( !isGeneric("downloadGimms") ) {
  setGeneric("downloadGimms", function(x, ...)
    standardGeneric("downloadGimms"))
}
#' Download GIMMS NDVI3g data
#'
#' @description
#' Download GIMMS NDVI3g binary data for a given time span from the NASA Ames
#' Ecological Forecasting Lab's FTP server
#' (\url{http://ecocast.arc.nasa.gov/data/pub/gimms/}, accessed on
#' January 15, 2016).
#'
#' @param x If 'Date', start date for download (e.g. "2000-01-01"). If
#' 'numeric', start year for download (e.g. 2000). If 'character', a vector of
#' full online filepath(s) to download, typically returned from
#' \code{\link{updateInventory}}. If not supplied, download will start from the
#' oldest file available.
#' @param y If 'Date', end date for download. If 'numeric', end year for
#' download. If not supplied, download will stop with the latest file available.
#' @param version \code{integer} (or any other class convertible to
#' \code{integer}). Specifies GIMMS NDVI3g product version, see 'Details' in
#' \code{\link{updateInventory}}.
#' @param dsn 'character'. Destination folder for file download. If not supplied,
#' all downloaded files will be stored in the current working directory.
#' @param overwrite Logical. If \code{TRUE}, already downloaded files in 'dsn'
#' will be overwritten.
#' @param quiet Logical. If \code{TRUE}, console output is reduced.
#' @param mode See \code{\link{download.file}}.
#' @param cores Integer. Number of cores for parallel computing.
#' @param ... Further arguments passed on to \code{\link{download.file}}, e.g.
#' 'method'.
#'
#' @return
#' A vector of filepaths.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{download.file}}
#'
#' @examples
#' \dontrun{
#' ## Destination folder for data download
#' gimms_dir <- paste0(getwd(), "/data")
#'
#' ## 'Date' method
#' gimms_files_date <- downloadGimms(x = as.Date("2000-01-01"),
#'                                   y = as.Date("2000-06-30"),
#'                                   dsn = gimms_dir)
#'
#' ## 'numeric' method, i.e. full years
#' gimms_files_year <- downloadGimms(x = 2000, y = 2002, dsn = gimms_dir)
#'
#' ## 'character' method, i.e. file names
#' gimms_files <- updateInventory(version = 0)
#' gimms_files <- gimms_files[grep("geo00", gimms_files)]
#' gimms_files_char <- downloadGimms(x = gimms_files, dsn = gimms_dir)
#'
#' ## 'missing' method, i.e. entire collection
#' gimms_files_full <- downloadGimms(dsn = gimms_dir)
#' }
#'
#' @export downloadGimms
#' @name downloadGimms

################################################################################
### function using 'Date' input ################################################
#' @aliases downloadGimms,Date-method
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "Date"),
          function(x, y, version = 1L,
                   dsn = getwd(), overwrite = FALSE, quiet = TRUE,
                   mode = "wb", cores = 1L, ...) {

            ## check if target folder exists
            checkDsn(dsn)

            ## check 'cores'
            cores <- checkCores(cores)

            ## jump to downloadGimms,missing-method if neither 'x' nor 'y' is specified
            if (missing(x) & missing(y))
              downloadGimms(version = version, dsn = dsn, overwrite = overwrite,
                            quiet = quiet, mode = mode, cores = cores, ...)

            ## available files and corresponding dates
            fls <- updateInventory(version = version)
            dts <- monthlyIndices(fls, timestamp = TRUE)

            ## start (finish) with the first (last) timestamp available if 'x'
            ## ('y') is not specified
            if (missing(x)) x <- dts[1]
            if (missing(y)) y <- dts[length(dts)]

            ## select files covering user-defined temporal range...
            usr_dts <- seq(x, y, 1)
            usr_dts <- usr_dts[which(substr(usr_dts, 9, 10) %in% c("01", "15"))]

            ## ...from version 0
            fls <- if (version == 0) {
              fls[dts %in% usr_dts]

            ## ...from version 1
            } else {
              # identify .nc4 files with at least 1 required date
              lst <- readRDS(system.file("extdata", "dates_ecv1.rds",
                                         package = "gimms"))

              mat <- sapply(usr_dts, function(i) {
                sapply(lst, function(j) any(i == j))
              })
              rownames(mat)[rowSums(mat) > 0]
            }

            ## download files
            downloader(fls, dsn = dsn, overwrite = overwrite,
                       quiet = quiet, mode = mode, cores = cores, ...)
          })


################################################################################
### function using numeric input (i.e. years) ##################################
#' @aliases downloadGimms,numeric-method
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "numeric"),
          function(x, y, version = 1L,
                   dsn = getwd(), overwrite = FALSE, quiet = TRUE,
                   mode = "wb", cores = 1L, ...) {

            ## check if target folder exists
            checkDsn(dsn)

            ## check 'cores'
            cores <- checkCores(cores)

            ## jump to downloadGimms,missing-method if neither 'x' nor 'y' is specified
            if (missing(x) & missing(y))
              downloadGimms(dsn = dsn, overwrite = overwrite, quiet = quiet,
                            mode = mode, cores = cores, ...)

            ## available files
            fls <- updateInventory(version = version)

            ## if specified, subset available files by time frame
            bsn <- basename(fls)
            yrs <- substr(bsn, ifelse(version == 1, 15, 4),
                          ifelse(version == 1, 18, 5))
            yrs <- as.numeric(yrs)

            ## if version 0, add leading century
            if (version == 0) {
              id_old <- yrs >= 81
              id_new <- yrs <= 80
              yrs[id_old] <- paste0("19", yrs[id_old])
              yrs[id_new] <- paste0("20", yrs[id_new])
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
          })


################################################################################
### function using character input (i.e. files) ################################
#' @aliases downloadGimms,character-method
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "character"),
          function(x, dsn = getwd(), overwrite = FALSE, quiet = TRUE,
                   mode = "wb", cores = 1L, ...) {

            ## check if target folder exists
            checkDsn(dsn)

            ## check 'cores'
            cores <- checkCores(cores)

            ## download
            downloader(x, dsn = dsn, overwrite = overwrite,
                       quiet = quiet, mode = mode, cores = cores, ...)
          })


################################################################################
### function using no input (i.e. download entire collection) ##################
#' @aliases downloadGimms,missing-method
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "missing"),
          function(version = 1L, dsn = getwd(), overwrite = FALSE, quiet = TRUE,
                   mode = "wb", cores = 1L, ...) {

            ## check if target folder exists
            checkDsn(dsn)

            ## check 'cores'
            cores <- checkCores(cores)

            ## download all available files
            downloader(updateInventory(version = version), dsn = dsn,
                       overwrite = overwrite, quiet = quiet, mode = mode,
                       cores = cores, ...)
          })

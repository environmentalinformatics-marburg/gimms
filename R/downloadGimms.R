if ( !isGeneric("downloadGimms") ) {
  setGeneric("downloadGimms", function(x, ...)
    standardGeneric("downloadGimms"))
}
#' Download GIMMS 3G data
#'
#' @description
#' Download GIMMS 3G binary data for a given time span from NASA FTP server
#' (\url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/}).
#'
#' @param x If 'numeric', start year for download (e.g. 2000). If 'character',
#' a vector of full online filepath(s) to download, typically returned from
#' \code{\link{updateInventory}}. If not supplied, download will start from the
#' first year available.
#' @param y 'numeric'. End year for download. If not supplied, download will
#' stop at the last year available.
#' @param dsn 'character'. Destination folder for file download. If not supplied,
#' all downloaded files will be stored in the current working directory.
#' @param overwrite Logical. If \code{TRUE}, already downloaded files in 'dsn'
#' will be overwritten.
#' @param quiet Logical. If \code{TRUE}, information sent to the console is
#' reduced.
#' @param mode See \code{\link{download.file}}.
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
#' ## Download GIMMS NDVI3g binary data from 2000-2005 (this might take some time...)
#' gimms_files <- downloadGimms(x = 2000, y = 2005,
#'                              dsn = paste0(getwd(), "/data"))
#' gimms_files[1:10]
#' }
#' @export downloadGimms
#' @name downloadGimms

################################################################################
### function using numeric input (i.e. years) ##################################
#' @aliases downloadGimms,numeric-method
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "numeric"),
          function(x, y,
                   dsn = getwd(), overwrite = FALSE, quiet = TRUE,
                   mode = "wb", ...) {

  ## jump to downloadGimms,missing-method if neither 'x' nor 'y' is specified
  if (missing(x) & missing(y))
    downloadGimms(dsn = dsn, overwrite = overwrite, quiet = quiet,
                  mode = mode, ...)

  ## available files
  gimms_fls <- updateInventory(sort = TRUE)

  ## if specified, subset available files by time frame
  gimms_bsn <- basename(gimms_fls)
  gimms_yrs_chr <- substr(gimms_bsn, 4, 5)
  gimms_yrs_num <- as.numeric(gimms_yrs_chr)

  id_old <- gimms_yrs_num >= 81
  id_new <- gimms_yrs_num <= 80
  gimms_yrs_chr[id_old] <- paste0("19", gimms_yrs_chr[id_old])
  gimms_yrs_chr[id_new] <- paste0("20", gimms_yrs_chr[id_new])
  gimms_yrs_num <- as.numeric(gimms_yrs_chr)

  ## start (finish) with the first (last) year available if 'x' ('y') is not
  ## specified
  if (missing(x)) x <- gimms_yrs_num[1]
  if (missing(y)) y <- gimms_yrs_num[length(gimms_yrs_num)]

  ## subset files
  gimms_fls <- gimms_fls[gimms_yrs_chr %in% seq(x, y)]

  ## download
  for (i in gimms_fls) {
    destfile <- paste0(dsn, "/", basename(i))
    if (file.exists(destfile) & !overwrite) {
      if (!quiet)
        cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
    } else {
      try(download.file(i, destfile = destfile, mode = mode, ...),
          silent = TRUE)
    }
  }

  ## return vector with output files
  gimms_out <- paste0(dsn, "/", basename(gimms_fls))
  return(gimms_out)

})


################################################################################
### function using character input (i.e. files) ################################
#' @aliases downloadGimms,character-method
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "character"),
          function(x, dsn = getwd(), overwrite = FALSE, quiet = TRUE,
                   mode = "wb", ...) {

  ## download
  for (i in x) {
    destfile <- paste0(dsn, "/", basename(i))
    if (file.exists(destfile) & !overwrite) {
      cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
    } else {
      try(download.file(i, destfile = destfile, mode = mode, ...),
          silent = TRUE)
    }
  }

  ## return vector with output files
  gimms_out <- paste0(dsn, "/", basename(x))
  return(gimms_out)

})


################################################################################
### function using character input (i.e. files) ################################
#' @aliases downloadGimms,missing-method
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "missing"),
          function(dsn = getwd(), overwrite = FALSE, quiet = TRUE,
                   mode = "wb", ...) {

  ## available files
  gimms_fls <- updateInventory()

  ## download
  for (i in gimms_fls) {
    destfile <- paste0(dsn, "/", basename(i))
    if (file.exists(destfile) & !overwrite) {
      cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
    } else {
      try(download.file(i, destfile = destfile, mode = mode, ...),
          silent = TRUE)
    }
  }

  ## return vector with output files
  gimms_out <- paste0(dsn, "/", basename(gimms_fls))
  return(gimms_out)

})

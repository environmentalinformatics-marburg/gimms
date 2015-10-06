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
#' @param ... Further arguments. Currently not in use.
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
#' # Download GIMMS 3G data from 1990 to 2009 (this might take some time...)
#' downloadGimms(begin = 2000, dsn = paste(getwd(), "data", sep = "/"))
#' }
#' @export downloadGimms
#' @name downloadGimms

################################################################################
### function using numeric input (i.e. years) ##################################
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "numeric"),
          function(x = 1981, y = 2013,
                   dsn = getwd(), overwrite = FALSE,
                   ...) {

  ## available files
  gimms_fls <- updateInventory()

  ## if specified, subset available files by time frame
  gimms_bsn <- basename(gimms_fls)
  gimms_yrs_chr <- substr(gimms_bsn, 4, 5)
  gimms_yrs_num <- as.numeric(gimms_yrs_chr)

  id_old <- gimms_yrs_num >= 81
  id_new <- gimms_yrs_num <= 80
  gimms_yrs_chr[id_old] <- paste0("19", gimms_yrs_chr[id_old])
  gimms_yrs_chr[id_new] <- paste0("20", gimms_yrs_chr[id_new])
  gimms_yrs_num <- as.numeric(gimms_yrs_chr)

  gimms_fls <- gimms_fls[gimms_yrs_chr %in% seq(begin, end)]

  ## download
  for (i in gimms_fls) {
    destfile <- paste0(dsn, "/", basename(i))
    if (file.exists(destfile) & !overwrite) {
      cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
    } else {
      try(download.file(i, destfile = destfile), silent = TRUE)
    }
  }

  ## return vector with output files
  gimms_out <- paste0(dsn, "/", gimms_fls)
  return(gimms_out)

})


################################################################################
### function using character input (i.e. files) ################################
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "character"),
          function(x, dsn = getwd(), overwrite = FALSE, ...) {

  ## download
  for (i in gimms_fls) {
    destfile <- paste0(dsn, "/", basename(i))
    if (file.exists(destfile) & !overwrite) {
      cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
    } else {
      try(download.file(i, destfile = destfile), silent = TRUE)
    }
  }

  ## return vector with output files
  gimms_out <- paste0(dsn, "/", gimms_fls)
  return(gimms_out)

})


################################################################################
### function using character input (i.e. files) ################################
#' @rdname downloadGimms
setMethod("downloadGimms",
          signature(x = "missing"),
          function(dsn = getwd(), overwrite = FALSE, ...) {

  ## available files
  gimms_fls <- updateInventory()

  ## download
  for (i in gimms_fls) {
    destfile <- paste0(dsn, "/", basename(i))
    if (file.exists(destfile) & !overwrite) {
      cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
    } else {
      try(download.file(i, destfile = destfile), silent = TRUE)
    }
  }

  ## return vector with output files
  gimms_out <- paste0(dsn, "/", gimms_fls)
  return(gimms_out)

})

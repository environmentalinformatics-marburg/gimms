#' Rearrange GIMMS files by date
#'
#' @description
#' Rearrange a vector of GIMMS files in ascending order of time. If no file
#' vector is supplied, the function will look for compatible files in the
#' current working directory.
#'
#' @param x Character. Vector of (local or online) filepaths. If \code{NULL},
#' \code{dsn} will be searched for available files via pattern matching.
#' @param dsn Character. Path to look for GIMMS data. If not supplied and
#' \code{is.null(fls)}, this argument defaults to the current working directory.
#' @param pattern Character. A regular expression passed on to
#' \code{\link{list.files}}.
#' @param ... Further arguments passed on to \code{\link{list.files}}.
#'
#' @return
#' A vector of filepaths arranged in ascending order of time.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{list.files}}
#'
#' @examples
#' ## latest version of files inventory
#' gimms_files <- updateInventory()
#' head(gimms_files)
#'
#' ## re-arrange vector with available files according to date
#' gimms_files_arr <- rearrangeFiles(gimms_files)
#' head(gimms_files_arr)
#'
#' ## (or simply use the two-in-one solution)
#' gimms_files <- updateInventory(sort = TRUE)
#'
#' @export rearrangeFiles
#' @name rearrangeFiles
rearrangeFiles <- function(x = NULL,
                           dsn = getwd(),
                           pattern = "^geo",
                           ...) {

  ## if `is.null(fls)`, apply pattern matching in 'dsn'
  if (is.null(x))
    x <- list.files(dsn, pattern = pattern, ...)

  ## vector to data.frame
  gimms_df <- data.frame(file = x, stringsAsFactors = FALSE)

  ## switch current locale time to us standard
  systime_locale <- Sys.getlocale(category = "LC_TIME")
  invisible(Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8"))

  ## create columns 'year', 'month' and 'day'
  gimms_df <- transform(gimms_df,
                        "year" = substr(basename(file), 4, 5),
                        "month" = substr(basename(file), 6, 8),
                        "day" = ifelse(substr(basename(file), 11, 11) == "a", 1, 15))

  ## create column 'date'
  gimms_df$date <- as.Date(paste0(gimms_df$day, gimms_df$month, gimms_df$year),
                           format = "%d%b%y")

  ## re-arrange rows by 'date'
  gimms_df <- gimms_df[order(gimms_df$date), ]

  ## revoke locale time adjustment
  Sys.setlocale(category = "LC_TIME", locale = systime_locale)

  ## return rearranged files
  gimms_fls <- gimms_df$file
  return(gimms_fls)
}

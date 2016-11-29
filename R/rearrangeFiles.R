#' Rearrange GIMMS NDVI3g.v0 Files
#'
#' @description
#' Rearrange local GIMMS NDVI3g.v0 files in ascending order of time. Since the
#' naming convention has significantly changed towards NDVI3g.v1, such a measure
#' should only be relevant for older file formats.
#'
#' @param x \code{character}. Vector of local filepaths. If missing, 'dsn' will
#' be searched for available files via pattern matching.
#' @param dsn \code{character}, defaults to the current working directory. Path
#' to look for GIMMS-related data if 'x' is missing.
#' @param pattern \code{character}, defaults to \code{"^geo.*.VI3g$"} for
#' standard NDVI3g.v0 files. A regular expression passed to \code{\link{list.files}}.
#' @param pos \code{integer}, defaults to \code{c(4, 6, 11)} for standard
#' NDVI3g.v0 files. The start positions of year, month and part of the month
#' ('a' or 'b') in the target GIMMS files.
#' @param ... Further arguments passed to \code{\link{list.files}}.
#'
#' @return
#' A \code{character} vector of filepaths arranged in ascending order of time.
#'
#' @seealso
#' \code{\link{list.files}}
#'
#' @export rearrangeFiles
#' @name rearrangeFiles
rearrangeFiles <- function(x,
                           dsn = getwd(),
                           pattern = "^geo.*.VI3g$",
                           pos = c(4, 6, 11),
                           ...) {

  if (length(pos) != 3)
    stop("'pos' must be a vector of length 3 (i.e., start position of year, month and day); see ?rearrangeFiles. \n")

  ## if `is.null(fls)`, apply pattern matching in 'dsn'
  if (missing(x))
    x <- list.files(dsn, pattern = pattern, ...)

  ## vector to data.frame
  gimms_df <- data.frame(file = x, stringsAsFactors = FALSE)

  ## backup current locale and switch to us standard
  locale <- Sys.getlocale(category = "LC_TIME")
  setLocale()

  ## create columns 'year', 'month' and 'day'
  gimms_df <- transform(gimms_df,
                        "year" = substr(basename(file), pos[1], pos[1] + 1),
                        "month" = substr(basename(file), pos[2], pos[2] + 2),
                        "day" = ifelse(substr(basename(file), pos[3], pos[3]) == "a", 1, 15))

  ## create column 'date'
  gimms_df$date <- as.Date(paste0(gimms_df$day, gimms_df$month, gimms_df$year),
                           format = "%d%b%y")

  ## re-arrange rows by 'date'
  gimms_df <- gimms_df[order(gimms_df$date), ]

  ## revoke locale time adjustment
  setLocale(TRUE, locale = locale)

  ## return rearranged files
  gimms_fls <- gimms_df$file
  return(gimms_fls)
}

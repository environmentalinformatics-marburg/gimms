#' Monthly indices from GIMMS filenames
#'
#' @description
#' Create numeric monthly indices from (local or online) GIMMS filenames as
#' input for the \code{\link{monthlyComposite}} function.
#'
#' @param x Character. Vector of (local or online) filenames.
#' @param version \code{integer} (or any other class convertible to
#' \code{integer}). Specifies GIMMS NDVI3g product version, see 'Details' in
#' \code{\link{updateInventory}}.
#' @param pos1,pos2 Numeric. The first and last element of the date string in
#' 'x', defaults to the GIMMS naming convention of the specified product
#' \code{version}.
#' @param timestamp Logical. If \code{TRUE}, an actual timestamp (formatted
#' according to \code{...}) is returned rather than a vector of indices.
#' @param ... Currently not used.
#'
#' @return
#' A 'numeric' vector with unique monthly indices or,
#' if \code{timestamp = TRUE}, a 'character' vector with formatted timestamps.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{monthlyComposite}}.
#'
#' @examples
#' ## vector of filenames
#' gimms_files <- c("geo85aug15a.n09-VI3g", "geo85aug15b.n09-VI3g",
#'                  "geo85sep15a.n09-VI3g", "geo85sep15b.n09-VI3g",
#'                  "geo85oct15a.n09-VI3g", "geo85oct15b.n09-VI3g")
#'
#' ## extract monthly indices
#' monthlyIndices(gimms_files, version = 0)
#' monthlyIndices(gimms_files, version = 0, timestamp = TRUE)
#'
#' @export monthlyIndices
#' @name monthlyIndices
monthlyIndices <- function(x, version = 1L,
                           pos1 = ifelse(version == 1, 15L, 4L),
                           pos2 = ifelse(version == 1, 23L, 8L),
                           timestamp = FALSE, ...) {

  ## extract timestamp
  ch_id <- if (version == 1) {
    getV1dates(x, pos1, pos2, suffix = FALSE)
  } else {
    substr(basename(x), pos1, pos2)
  }

  ## return formatted date
  if (timestamp) {

    # back-up current locale and subsequently swith to us standard
    locale <- Sys.getlocale(category = "LC_TIME")
    setLocale()

    # year
    ch_year <- substr(ch_id, 1, 2)
    # month
    ch_month <- substr(ch_id, 3, 5)
    for (i in 1:length(ch_month)) {
      ch_month[i] <- month.abb[which(tolower(month.abb) == ch_month[i])]
    }
    # day
    ch_day <- if (version == 1L) {
        substr(getV1dates(x, pos1, pos2), 8, 8)
      } else {
        substr(basename(x), pos2+3, pos2+3)
      }
    ch_day <- ifelse(ch_day == "a", 1, 15)

    # concatenate and reformat date string
    ch_date <- paste0(ch_day, ch_month, ch_year)
    dt_time <- as.Date(ch_date, format = "%d%b%y")

    # revoke locale time adjustment
    setLocale(reset = TRUE, locale = locale)
    return(dt_time)

  ## return numeric indices
  } else {
    fc_id <- factor(ch_id, levels = unique(ch_id))
    num_id <- as.numeric(fc_id)
    return(num_id)
  }

}

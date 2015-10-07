#' Monthly indices from GIMMS filenames
#'
#' @description
#' Create numeric monthly indices from (local or online) GIMMS filenames as
#' input for the \code{\link{monthlyComposite}} function.
#'
#' @param x Character. Vector of (local or online) filenames.
#' @param pos1,pos2 Numeric. The first and last element of the date string in
#' 'x', defaults to the GIMMS naming convention; see \code{\link{substr}}.
#'
#' @return
#' A numeric vector with unique monthly indices.
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
#' monthlyIndices(gimms_files)
#'
#' @export monthlyIndices
#' @name monthlyIndices
monthlyIndices <- function(x, pos1 = 4L, pos2 = 8L) {

  ## extract timestamp
  ch_id <- substr(basename(x), pos1, pos2)
  fc_id <- factor(ch_id, levels = unique(ch_id))

  ## return numeric indices
  num_id <- as.numeric(fc_id)
  return(num_id)

}

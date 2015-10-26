#' Update GIMMS 3G file inventory
#'
#' @description
#' Download the latest version of the GIMMS 3G file inventory from NASA FTP
#' server (\url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/}) or, if
#' server is not accessible (e.g. due to missing internet connection), load
#' local version of file inventory.
#'
#' @param sort Logical. If \code{TRUE} (default), the available files are sorted
#' by date prior to return.
#'
#' @return
#' A vector of online filepaths.
#'
#' @author
#' Florian Detsch
#'
#' @examples
#' updateInventory()
#'
#' @export updateInventory
#' @name updateInventory
updateInventory <- function(sort = FALSE) {

  ## available files (online)
  cat("Trying to update GIMMS inventory from server...\n")
  gimms_fls <-
    suppressWarnings(
      try(
        readLines("http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00FILE-LIST.txt"),
        silent = TRUE
      )
    )

  ## remove duplicate entries
  gimms_fls <- gimms_fls[!duplicated(basename(gimms_fls))]

  ## available files (offline)
  if (class(gimms_fls) == "try-error") {
    cat("Online update failed. Using local inventory...\n")
    gimms_fls <- readRDS(system.file("extdata", "inventory.rds",
                                     package = "gimms"))
  } else {
    cat("Online update of the GIMMS file inventory successful!\n")
  }

  ## sort files (optional)
  if (sort)
    gimms_fls <- rearrangeFiles(gimms_fls)

  ## return files
  return(gimms_fls)
}

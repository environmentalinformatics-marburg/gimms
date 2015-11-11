#' Update GIMMS 3G file inventory
#'
#' @description
#' Download the latest version of the GIMMS 3G file inventory from NASA FTP
#' server (\url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/}) or, if
#' server is not accessible (e.g. due to missing internet connection), load
#' local version of file inventory.
#'
#' @param sort 'logical'. If \code{TRUE}, the list of available files is sorted
#' by date prior to return.
#' @param quiet 'logical'. If \code{FALSE}, some details about the online file
#' retrieval are printed to the console.
#'
#' @return
#' A 'character' vector of online filepaths.
#'
#' @author
#' Florian Detsch
#'
#' @examples
#' updateInventory()
#'
#' @export updateInventory
#' @name updateInventory
updateInventory <- function(sort = TRUE, quiet = TRUE) {

  ## available files (online)
  if (!quiet)
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
    if (!quiet)
      cat("Online update failed. Using local inventory...\n")

    gimms_fls <- readRDS(system.file("extdata", "inventory.rds",
                                     package = "gimms"))
  } else {
    if (!quiet)
      cat("Online update of the GIMMS file inventory successful!\n")
  }

  ## sort files (optional)
  if (sort)
    gimms_fls <- rearrangeFiles(gimms_fls)

  ## return files
  return(gimms_fls)
}

#' Update GIMMS NDVI3g file inventory
#'
#' @description
#' Download the latest version of the GIMMS NDVI3g file inventory either from
#' NASA Ames Ecological Forecasting Lab (ECOCAST) or, if not reachable, from
#' NASA Earth Exchange (NEX) Amazon AWS. If none of the specified endpoints is
#' reachable (e.g., if there is no active internet connection), the latest local
#' version of the file inventory derived from ECOCAST is used.
#'
#' @param server \code{character}. Specifies the remote server to use. Currently
#' available options are \code{"ecocast"}
#' (\url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/}) and
#' \code{"nasanex"} (\url{https://aws.amazon.com/de/nasa/nex/}).
#'
#' @return
#' A vector of online filepaths.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{rearrangeFiles}}.
#'
#' @examples
#' updateInventory()
#' updateInventory(server = "nasanex")
#'
#' @export updateInventory
#' @name updateInventory
updateInventory <- function(server = c("ecocast", "nasanex")) {

  ## available files (online)
  is_ecocast <- server[1] == "ecocast"
  fls <- if (is_ecocast) updateEcocast() else updateNasanex()

  ## if first-choice server is not available, try alternative server
  if (class(fls) == "try-error" & length(server) == 2) {
    cat("Priority server ('", server[1],
        "') is not available. Contacting alternative server ('", server[2],
        "').\n", sep = "")
    fls <- if (is_ecocast) updateNasanex() else updateEcocast()
  }

  ## available files (offline)
  if (class(fls) == "try-error") {
    cat("Failed to retrieve online information. Using local file inventory...\n")
    fls <- readRDS(system.file("extdata", "inventory.rds", package = "gimms"))
  }

  ## remove duplicates and sort according to date
  fls <- fls[!duplicated(basename(fls))]
  fls <- rearrangeFiles(fls)

  ## return files
  return(fls)
}


### update from ecocast -----

updateEcocast <- function() {
  con <- "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00FILE-LIST.txt"

  suppressWarnings(
    try(readLines(con), silent = TRUE)
  )
}


### update from nasanex -----

updateNasanex <- function() {
  con <- "https://nasanex.s3.amazonaws.com/"

  cnt <- try(RCurl::getURL(con, dirlistonly = TRUE), silent = TRUE)

  if (class(cnt) != "try-error") {
    cnt <- sapply(strsplit(strsplit(cnt, "<Key>")[[1]], "</Key>"), "[[", 1)

    id <- sapply(cnt, function(i) {
      length(grep("^AVHRR/GIMMS/3G.*VI3g$", i)) == 1
    })
    cnt <- cnt[id]
  }

  return(cnt)
}

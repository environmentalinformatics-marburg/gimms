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
#' available options are \code{"ecocast"} (versions 0 and 1, see 'Details' and
#' 'References') and \code{"nasanex"} (version 0).
#' @param version \code{integer} (or any other class convertible to
#' \code{integer}). Specifies GIMMS NDVI3g product version, see 'Details'.
#' Currently ignored if \code{server = "nasanex"}.
#'
#' @return
#' A vector of online filepaths.
#'
#' @author
#' Florian Detsch
#'
#' @details
#' Only recently, GIMMS NDVI3g version 1 has been released which is currently
#' available until end 2015 and comes in \code{.nc4} format (as apposed to raw
#' binary associated with version 0 which is currently available until end 2013).
#'
#' @references
#' ECOCAST (2016). Available online (accessed on 27 October 2016):
#' \itemize{
#' \item{Version 0 (raw binary, until end 2013)}
#' {\url{https://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/}}
#' \item{Version 1 (\code{.nc4}, until end 2015)}
#' {\url{https://ecocast.arc.nasa.gov/data/pub/gimms/3g.v1/}}
#' }
#'
#' NASANEX (2016). Available online (accessed on 27 October 2016):
#' \itemize{
#' \item{Version 0 (raw binary, until end 2012)}
#' \url{https://nasanex.s3.amazonaws.com/}
#' }
#'
#' @seealso
#' \code{\link{rearrangeFiles}}.
#'
#' @examples
#' updateInventory()
#'
#' @export updateInventory
#' @name updateInventory
updateInventory <- function(server = c("ecocast", "nasanex"), version = 1L) {

  ## available files (online)
  is_ecocast <- server[1] == "ecocast"
  fls <- if (is_ecocast) updateEcocast(version) else updateNasanex()

  ## if first-choice server is not available, try alternative server
  if (class(fls) == "try-error" & length(server) == 2) {
    cat("Priority server ('", server[1],
        "') is not available. Contacting alternative server ('", server[2],
        "').\n", sep = "")
    fls <- if (is_ecocast) updateNasanex() else updateEcocast(version)
  }

  ## available files (offline)
  if (class(fls) == "try-error") {
    cat("Failed to retrieve online information. Using local file inventory...\n")

    fls <- if (server == "nasanex") {
      readRDS(system.file("extdata", "inventory_nnv0.rds", package = "gimms"))
    } else {
      readRDS(system.file("extdata", paste0("inventory_ec",
                                            ifelse(version == 1, "v1", "v0"),
                                            ".rds"), package = "gimms"))
    }
  }

  ## remove duplicates and sort according to date
  fls <- fls[!duplicated(basename(fls))]
  fls <- rearrangeFiles(fls)

  ## return files
  return(fls)
}


### update from ecocast -----

updateEcocast <- function(version = 1) {

  version <- as.integer(version)
  con <- if (version == 1) {
    "https://ecocast.arc.nasa.gov/data/pub/gimms/3g.v1/00FILE-LIST.txt"
  } else if (version == 0) {
    "https://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00FILE-LIST.txt"
  } else {
    stop("Invalid product version specified.\n")
  }

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


### import local inventory -----

readInventory <- function(server = c("ecocast", "nasanex"), version = 1) {
  if (server[1] == "ecocast") {
    version <- as.integer(version)
    if (version == 1) {
      readRDS(system.file("extdata", "inventory_ecv1.rds", package = "gimms"))
    } else if (version == 0) {
      readRDS(system.file("extdata", "inventory_ecv0.rds", package = "gimms"))
    } else {
      stop("Invalid product version specified.\n")
    }
  } else if (server[1] == "nasanex") {
    readRDS(system.file("extdata", "inventory_nnv0.rds", package = "gimms"))
  } else {
    stop("Invalid product server specified.\n")
  }
}

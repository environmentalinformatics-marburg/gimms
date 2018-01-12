#' Update GIMMS NDVI3g File Inventory
#'
#' @description
#' Download the latest version of the GIMMS NDVI3g file inventory from the NASA
#' Ames Ecological Forecasting Lab (ECOCAST) or NASA Earth Exchange (NEX) Amazon
#' AWS. If none of the specified endpoints is reachable (e.g., if there is no
#' active internet connection), the latest local version of the file inventory
#' is used.
#'
#' @param server \code{character}. Specifies the remote server to use. Currently
#' available options are \code{"ecocast"} (default) and \code{"nasanex"}.
#' @param version \code{integer} (or any other convertible class), defaults to
#' \code{1L}. Specifies desired GIMMS NDVI3g product version, see 'Details'.
#' Currently ignored if \code{server = "nasanex"}.
#' @param quiet \code{logical}, defaults to \code{FALSE}. If \code{TRUE},
#' console output is disabled.
#'
#' @return
#' A \code{character} vector of online filepaths.
#'
#' @details
#' GIMMS NDVI3g.v1 is currently available from ECOCAST until end 2015 and comes
#' in NetCDF (\code{.nc4}) format. In contrast, NDVI3g.v0 is available as ENVI
#' binary imagery and available from ECOCAST (NASANEX) until end 2013 (2012)
#' only.
#'
#' @seealso
#' \code{\link{rearrangeFiles}}.
#'
#' @examples
#' \dontrun{
#' updateInventory()            # NDVI3g.v1
#' updateInventory(version = 0) # NDVI3g.v0
#' }
#'
#' ## note that local versions of the online file inventories are also available
#' ofl_v1 <- system.file("extdata", "inventory_ecv1.rds", package = "gimms")
#' readRDS(ofl_v1)
#'
#' ofl_v0 <- system.file("extdata", "inventory_ecv0.rds", package = "gimms")
#' readRDS(ofl_v0)
#'
#' @export updateInventory
#' @name updateInventory
updateInventory <- function(server = c("ecocast", "nasanex"), version = 1L,
                            quiet = FALSE) {

  ## available files (online)
  is_ecocast <- server[1] == "ecocast"
  fls <- if (is_ecocast) updateEcocast(version) else updateNasanex()

  ## if first-choice server is not available, try alternative server
  if (class(fls) == "try-error" & length(server) == 2) {
    if (!quiet)
      cat("Priority server ('", server[1],
          "') is not available. Contacting alternative server ('", server[2],
          "').\n", sep = "")
    fls <- if (is_ecocast) updateNasanex() else updateEcocast(version)
  }

  ## available files (offline)
  if (class(fls) == "try-error") {
    if (!quiet)
      cat("Failed to retrieve online information. Using local file inventory...\n")

    fls <- if (server[1] == "nasanex") {
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

updateEcocast <- function(version = 1L) {

  version <- as.integer(version)
  con <- paste0(serverPath(version = version), "/00FILE-LIST.txt")

  suppressWarnings(
    try(readLines(con), silent = TRUE)
  )
}


### update from nasanex -----

updateNasanex <- function() {
  con <- serverPath("nasanex")

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

readInventory <- function(server = c("ecocast", "nasanex"), version = 1L) {
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


### server paths -----

serverPath <- function(server = c("ecocast", "nasanex"), version = 1L) {
  if (server[1] == "ecocast") {
    paste0("https://ecocast.arc.nasa.gov/data/pub/gimms/3g.",
           ifelse(as.integer(version) == 1, "v1", "v0"))
  } else {
    "https://nasanex.s3.amazonaws.com"
  }
}

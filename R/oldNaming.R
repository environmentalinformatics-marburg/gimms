#' Create Old GIMMS NDVI3g File Names
#'
#' @description
#' Transform NDVI3g.v1 file names into "traditional" NDVI3g.v0 naming
#' convention.
#'
#' @param x \code{character}. GIMMS NDVI3g.v1 file names to transform.
#' @param sat \code{character}. Satellite number, see 'Details'.
#' @param suffix \code{character}. Optional suffix to append to the resulting
#' file names, e.g. \code{".tif"} for GeoTiff files (see also
#' \code{\link{writeFormats}}).
#'
#' @return
#' A \code{character} vector of file names following the "traditional" NDVI3g.v0
#' naming convention.
#'
#' @details
#' The satellite number was an essential constituent of the NDVI3g.v0 file
#' naming convention (see 'References'), but has been removed in NDVI3g.v1. In
#' order to maintain a uniform file naming, the 'sat' argument accepts a single
#' \code{character} placeholder (defaults to \code{"XX"}) which is inserted in
#' newer files where no such information is available from NDVI3g.v0.
#'
#' @references
#' \url{https://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00READMEgeo.txt}
#'
#' @examples
#' fls <- system.file("extdata", "inventory_ecv1.rds", package = "gimms")
#' fls <- basename(readRDS(fls))
#'
#' oldNaming(fls[1])
#' oldNaming(fls[2:3], suffix = ".tif")
#'
#' @export oldNaming
#' @name oldNaming
oldNaming <- function(x, sat = "XX", suffix = "") {

  ## available ndvi3g.v0 files (hold satellite number)
  fls_v0 <- basename(updateInventory(version = 0))
  dts_v0 <- getV0dates(fls_v0)

  ## determine which files in 'x' are available from v0, and which aren't
  dts_v1 <- getV1dates(x)

  inc <- match(dts_v1, dts_v0)
  exc <- which(!dts_v1 %in% dts_v0)

  ## create output names
  out <- c(if (length(inc) > 0) paste0(fls_v0[inc], suffix) else NULL,
           if (length(exc) > 0) {
             paste0("geo", dts_v1[exc], ".n", sat, "-VI3g", suffix)
           } else {
             NULL
           })

  paste0(dirname(x), "/", out)
}

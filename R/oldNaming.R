#' Create Traditional GIMMS NDVI3g File Names
#'
#' @description
#' Reformat the names of local NDVI3g.v1 files according to "traditional"
#' NDVI3g.v0 naming convention.
#'
#' @param x \code{character}. GIMMS NDVI3g.v1 file names to reformat, see
#' 'Details'.
#' @param suffix \code{character}. Optional suffix to append to the resulting
#' file names, e.g. \code{".tif"} for GeoTiff files (see also
#' \code{\link{writeFormats}}).
#'
#' @return
#' A \code{character} vector of file names following the "traditional" NDVI3g.v0
#' naming convention.
#'
#' @details
#' The satellite number, which was an essential constituent of the NDVI3g.v0
#' file naming convention (see 'References'), has been removed from NDVI3g.v1
#' file names. However, it can easily be accessed via \code{\link{ncvar_get}}
#' given that the file has previously been downloaded. If an NDVI3g.v1 file
#' specified to 'x' is not available locally, by contrast, the satellite number
#' is retrieved from the built-in NDVI3g.v0 file inventory (until end 2013; see
#' 'Examples') and newer files are tagged with \code{"XX"} indicating missing
#' information.
#'
#' @seealso
#' \code{\link{nc_open}}, \code{\link{ncvar_get}}.
#'
#' @references
#' \url{https://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00READMEgeo.txt}
#' (accessed on 2017-01-04).
#'
#' @examples
#' \dontrun{
#' fls <- system.file("extdata", "inventory_ecv1.rds", package = "gimms")
#' fls <- basename(readRDS(fls))
#'
#' oldNaming(fls[64:65], suffix = ".tif") # adds .tif extension
#' oldNaming(fls[65:66])                  # tags newer files with 'XX'
#' }
#'
#' @export oldNaming
#' @name oldNaming
oldNaming <- function(x, suffix = "") {

  ## available ndvi3g.v0 files (hold satellite number)
  fls_v0 <- basename(updateInventory(version = 0, quiet = TRUE))
  dts_v0 <- getV0dates(fls_v0)

  dts_v1 <- getV1dates(x)

  ## loop over input files
  nmb <- lapply(x, function(i) {

    # if current file exists, read satellite number
    if (file.exists(i)) {
      nc4 <- ncdf4::nc_open(i)
      formatC(ncdf4::ncvar_get(nc4, "satellites"), width = 2, flag = "0")

    # else determine satellite number from corresponding ndvi3g.v0 file (if
    # available)
    } else {
      dts <- getV1dates(i)
      ids <- match(dts, dts_v0)

      if (sum(is.na(ids)) == 0) {
        substr(fls_v0[ids], 14, 15)
      } else {
        rep("XX", length(ids))
      }
    }
  })

  nmb <- unlist(nmb)

  ## create and return output names
  out <- paste0("geo", dts_v1, ".n", nmb, "-VI3g", suffix)
  paste0(dirname(x), "/", out)
}

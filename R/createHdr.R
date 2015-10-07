#' Create ENVI header file
#'
#' @description
#' Create an ENVI header file (see \url{http://www.exelisvis.com/docs/ENVIHeaderFiles.html})
#' to properly import GIMMS binary data into native 'Raster*' format.
#'
#' @param hdr Character. If not supplied, defaults to the companion header file
#' for GIMMS3g binary data. See \code{\link{read.ENVI}} for further information
#' and section 'Examples' below for required file contents.
#' @param file Character. Output filepath, defaults to
#' \code{paste0(raster::rasterOptions()$tmpdir, "/tmp.hdr")}.
#'
#' @return
#' A filename with the location of the (temporary) header file.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{read.ENVI}}
#'
#' @examples
#' # Create standard GIMMS3G header file
#' gimms_header <- createHdr()
#' readLines(gimms_header)
#'
#' @export createHdr
#' @name createHdr
createHdr <- function(hdr, file) {

  ## default filepath of envi header file (.hdr)
  if (missing(file)) {
    { sink("/dev/null"); tmp_dir <- raster::rasterOptions()$tmpdir; sink(); }

    raster::tmpDir(create = TRUE)
    file <- paste0(tmp_dir, "/tmp.hdr")
  }

  ## default header
  if (missing(hdr))
    hdr <- paste("ENVI",
                 "description = { R-language data }",
                 "samples = 2160",
                 "lines = 4320",
                 "bands = 1",
                 "data type = 2",
                 "header offset = 0",
                 "interleave = bsq",
                 "byte order = 1", sep = "\n")

  ## write and return file
  writeLines(hdr, file)
  return(file)
}

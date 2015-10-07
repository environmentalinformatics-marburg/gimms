#' Create ENVI header file
#'
#' @description
#' Create an ENVI header file (see \url{http://www.exelisvis.com/docs/ENVIHeaderFiles.html})
#' to properly import GIMMS binary data into native 'Raster*' format.
#'
#' @param file Character. Absolute input filepath of the GIMMS binary file being
#' processed.
#' @param hdr Character. If not supplied, defaults to the default content of the
#' companion header file for GIMMS3g binary data. See seection 'Examples' below
#' for required file contents.
#'
#' @return
#' A filename with the absolute location of the header file.
#'
#' @author
#' Florian Detsch
#'
#' @examples
#' # Create standard GIMMS3g header file
#' gimms_header <- createHdr("data/geo13jul15a.n19-VI3g")
#'
#' gimms_header
#' readLines(gimms_header)
#'
#' @export createHdr
#' @name createHdr
createHdr <- function(file, hdr) {

  ## default filepath of envi header file (.hdr)
  if (missing(file)) {
    { sink("/dev/null"); tmp_dir <- raster::rasterOptions()$tmpdir; sink(); }

    raster::tmpDir(create = TRUE)
    file <- paste0(tmp_dir, "/tmp.hdr")
  }
  ## location of header file
  file_hdr <- paste0(file, ".hdr")

  ## default content of gimms3g-related header file
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
  writeLines(hdr, file_hdr)
  return(file_hdr)
}

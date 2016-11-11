#' Get NDVI or flag layers
#'
#' @description
#' Extract all NDVI or flag layers from the output \code{list} of
#' \code{\link{rasterizeGimms}}.
#'
#' @param x \code{list} of 2-layered \code{RasterStack} objects. If not created
#' from \code{\link{rasterizeGimms}}, make sure that the NDVI (flag) layer is
#' always the first (second) layer per \code{RasterStack}.
#'
#' @return
#' A \code{RasterStack} object with all NDVI or flag layers in 'x'.
#'
#' @author
#' Florian Detsch
#'
#' @export getNDVILayers
#' @name getNDVILayers
getNDVILayers <- function(x) {
  raster::stack(lapply(x, "[[", 1))
}

#' @export getFlagLayers
#' @name getNDVILayers
getFlagLayers <- function(x) {
  raster::stack(lapply(x, "[[", 2))
}

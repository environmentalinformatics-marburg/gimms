#' Rearrange GIMMS NDVI3g.v0 Files
#'
#' @description
#' Rearrange local GIMMS NDVI3g.v0 files in ascending order of time. Since the
#' naming convention has significantly changed towards NDVI3g.v1, such a measure
#' should only be relevant for older file formats.
#'
#' @param x \code{character}. Vector of local filepaths. If missing, 'dsn' will
#' be searched for available files via pattern matching.
#' @param dsn \code{character}, defaults to the current working directory. Path
#' to look for GIMMS-related data if 'x' is missing.
#' @param pattern \code{character}, defaults to \code{"^geo.*.VI3g$"} for
#' standard NDVI3g.v0 files. A regular expression passed to \code{\link{list.files}}.
#' @param pos \code{integer}, defaults to \code{c(4, 6, 11)} for standard
#' NDVI3g.v0 files. The start positions of year, month and part of the month
#' ('a' or 'b') in the target GIMMS files.
#' @param ... Further arguments passed to \code{\link{list.files}}.
#'
#' @return
#' A \code{character} vector of filepaths arranged in ascending order of time.
#'
#' @seealso
#' \code{\link{list.files}}
#'
#' @export rearrangeFiles
#' @name rearrangeFiles
rearrangeFiles <- function(x,
                           dsn = getwd(),
                           pattern = "^geo.*.VI3g$",
                           pos = c(4, 6, 11),
                           ...) {
  
  if (length(pos) != 3)
    stop("'pos' must be a vector of length 3 (i.e., start position of year, month and day); see ?rearrangeFiles. \n")
  
  ## if `is.null(fls)`, apply pattern matching in 'dsn'
  if (missing(x))
    x <- list.files(dsn, pattern = pattern, ...)
  
  ## map month abbreviations to numeric strings
  mns = c(
    jan = "01"
    , feb = "02"
    , mar = "03"
    , apr = "04"
    , may = "05"
    , jun = "06"
    , jul = "07"
    , aug = "08"
    , sep = "09"
    , oct = "10"
    , nov = "11"
    , dec = "12"
  )
  
  nfo = rep(
    basename(x)
    , each = 3L
  ) |> 
    substring(
      first = c(pos[1], pos[2], pos[3])
      , last = c(pos[1] + 1L, pos[2] + 2L, pos[3])
    ) |> 
    split(
      f = rep(
        1:3
        , times = length(x)
      )
    )

  dts = with(
    nfo
    , {
      mn = mns[
        match(
          `2`
          , names(mns)
        )
      ]
      
      dy = ifelse(
        `3` == "a"
        , "01"
        , "15"
      )
      
      as.Date(
        paste(dy, mn, `1`)
        , format = "%d%m%y"
      )
    }
  )

  x[
    order(dts)
  ]
}

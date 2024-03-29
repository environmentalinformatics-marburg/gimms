% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadGimms.R
\docType{class}
\name{downloadGimms}
\alias{downloadGimms}
\alias{downloadGimms,dateORmissing,dateORmissing-method}
\alias{downloadGimms,Date-method}
\alias{downloadGimms,numericORmissing,numericORmissing-method}
\alias{downloadGimms,numeric-method}
\alias{downloadGimms,character,ANY-method}
\alias{downloadGimms,character-method}
\alias{downloadGimms,missing,missing-method}
\alias{downloadGimms,missing-method}
\title{Download GIMMS NDVI3g Data}
\usage{
\S4method{downloadGimms}{dateORmissing,dateORmissing}(
  x,
  y,
  version = 1L,
  dsn = getwd(),
  overwrite = FALSE,
  quiet = TRUE,
  mode = "wb",
  cores = 1L,
  server = c("poles", "nasanex", "ecocast"),
  ...
)

\S4method{downloadGimms}{numericORmissing,numericORmissing}(
  x,
  y,
  version = 1L,
  dsn = getwd(),
  overwrite = FALSE,
  quiet = TRUE,
  mode = "wb",
  cores = 1L,
  server = c("poles", "nasanex", "ecocast"),
  ...
)

\S4method{downloadGimms}{character,ANY}(
  x,
  dsn = getwd(),
  overwrite = FALSE,
  quiet = TRUE,
  mode = "wb",
  cores = 1L,
  ...
)

\S4method{downloadGimms}{missing,missing}(
  version = 1L,
  dsn = getwd(),
  overwrite = FALSE,
  quiet = TRUE,
  mode = "wb",
  cores = 1L,
  server = c("poles", "nasanex", "ecocast"),
  ...
)
}
\arguments{
\item{x}{Start time for data download as either \code{Date} object (e.g.,
\code{as.Date("2000-01-01")}) or \code{numeric} year (e.g., \code{2000}).
Alternatively, a \code{character} vector of online filepaths to download
created from \code{\link{updateInventory}}. If \code{missing}, all files
available online are being downloaded.}

\item{y}{End time for data download as either \code{Date} object or
\code{numeric} year. Ignored if 'x' is a \code{character} object or missing.}

\item{version}{\code{integer} (or any other convertible class), defaults to
\code{1L}. Specifies desired GIMMS NDVI3g product version, see 'Details' in
\code{\link{updateInventory}}. Ignored if 'x' is a \code{character} object.}

\item{dsn}{\code{character}, defaults to the current working directory.
Target folder for file download.}

\item{overwrite}{\code{logical}, defaults to \code{FALSE}. If \code{TRUE},
identically named files in 'dsn' will be overwritten.}

\item{quiet}{\code{logical}. If \code{TRUE} (default), console output is
reduced.}

\item{mode}{\code{character}. See \code{\link{download.file}}.}

\item{cores}{\code{integer}, defaults to \code{1L}. Number of cores used for
parallel processing. Note that a fast internet connection is required in
order for parallelization to take effect.}

\item{server}{See \code{\link[gimms]{updateInventory}}.}

\item{...}{Further arguments passed to \code{\link{download.file}}, e.g.
'method'.}
}
\value{
A \code{character} vector of local filepaths.
}
\description{
Download GIMMS NDVI3g data, optionally for a given period of time. NDVI3g.v1
(NetCDF, until end 2015) is available from the \code{"poles"} and
\code{"ecocast"} servers, whereas NDVI3g.v0 (ENVI binary, until end 2013) is
\code{"ecocast"} and \code{"nasanex"} exclusive.
}
\examples{
\dontrun{
tmp <- tempdir()

## 'Date' method
gimms_files_date <- downloadGimms(x = as.Date("2000-01-01"),
                                  y = as.Date("2000-12-31"),
                                  dsn = tmp)

## 'numeric' method (i.e., particular years)
gimms_files_year <- downloadGimms(x = 2000,
                                  y = 2002,
                                  dsn = tmp)

## 'character' method (i.e., particular files)
ecocast <- system.file("extdata", "inventory_ecv1.rds", package = "gimms")
gimms_files_char <- readRDS(ecocast)
gimms_files_char <- downloadGimms(x = gimms_files_char[1:6],
                                  dsn = tmp)

## 'missing' method (i.e., entire collection)
gimms_files_full <- downloadGimms(dsn = tmp)
}

}
\seealso{
\code{\link{updateInventory}}, \code{\link{download.file}}.
}

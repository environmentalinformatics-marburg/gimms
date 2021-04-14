#' Update GIMMS NDVI3g File Inventory
#'
#' @description
#' Download the latest version of the GIMMS NDVI3g file inventory from the NASA
#' Ames Ecological Forecasting Lab (ECOCAST), NASA Earth Exchange (NEX) Amazon
#' AWS or A Big Earth Data Platform for Three Poles. If the specified endpoint 
#' is not reachable (e.g., if there is no active internet connection), the 
#' latest local version of the file inventory is used.
#'
#' @param server \code{character}. Specifies the remote server to use. Currently
#' available options are \code{"ecocast"} (default), \code{"nasanex"} and 
#' \code{"poles"}.
#' @param version \code{integer} (or any other convertible class), defaults to
#' \code{1L}. Specifies desired GIMMS NDVI3g product version, see 'Details'.
#' Currently ignored if \code{server != "ecocast"}.
#' @param quiet \code{logical}, defaults to \code{FALSE}. If \code{TRUE},
#' console output is disabled.
#'
#' @return
#' A \code{character} vector of online filepaths.
#'
#' @details
#' GIMMS NDVI3g.v1 is currently available from ECOCAST and A Big Earth Data 
#' Platform for Three Poles until end 2015 and comes in NetCDF (\code{.nc4}) 
#' format. In contrast, NDVI3g.v0 is available as ENVI binary imagery and 
#' available from ECOCAST (NASANEX) until end 2013 (2012) only.
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
#' ofl_ecv1 <- system.file("extdata", "inventory_ecv1.rds", package = "gimms")
#' readRDS(ofl_ecv1)
#'
#' ofl_v0 <- system.file("extdata", "inventory_ecv0.rds", package = "gimms")
#' readRDS(ofl_v0)
#'
#' ofl_plv1 <- system.file("extdata", "inventory_plv1.rds", package = "gimms")
#' readRDS(ofl_plv1)
#' 
#' @export updateInventory
#' @name updateInventory
updateInventory <- function(
  server = c("ecocast", "nasanex", "poles")
  , version = 1L
  , quiet = FALSE
) {
  
  server = match.arg(server)
  
  ## available files (online)
  fls = do.call(
    switch(
      server
      , "ecocast" = updateEcocast
      , "nasanex" = updateNasanex
      , "poles"   = updatePoles
    )
    , args = list(
      version = version
    )
  )
  
  ## available files (offline)
  if (inherits(fls, "try-error")) {
    if (!quiet) {
      cat("Failed to retrieve online information. Using local file inventory...\n")
    }
    
    extfile = paste0(
      "inventory_"
      , switch(
        server
        , "ecocast" = paste0(
          "ec"
          , ifelse(
            version == 1
            , "v1"
            , "v0"
          )
        )
        , "nasanex" = "nnv0"
        , "poles" = "plv1" 
      )
      , ".rds"
    )
    
    fls = readRDS(
      system.file(
        "extdata"
        , extfile
        , package = "gimms"
      )
    )
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
  
  ## handle expired certificate using 'curl'
  h = curl::new_handle(
    ssl_verifypeer = 0L
  )
  con = curl::curl(
    paste0(serverPath(version = version), "/00FILE-LIST.txt")
    , handle = h
  )
  on.exit(
    close(con)
  )
  
  suppressWarnings(
    try(readLines(con), silent = TRUE)
  )
}


### update from nasanex -----

updateNasanex <- function(...) {
  
  # list available folders
  con = serverPath("nasanex")
  cnt = try(readLines(con, warn = FALSE)[2], silent = TRUE)
  if (inherits(cnt, "try-error")) {
    return(cnt)
  }
  
  cnt <- sapply(strsplit(strsplit(cnt, "<Key>")[[1]], "</Key>"), "[[", 1)
  
  id <- sapply(cnt, function(i) {
    length(grep("^AVHRR/GIMMS/3G.*VI3g$", i)) == 1
  })
  
  return(file.path(con, cnt[id]))
}


### update from poles -----

updatePoles = function(...) {
  
  h = curl::new_handle(
    dirlistonly = TRUE
    , userpwd = "download_403193:72855006"
  )
  con = curl::curl(
    serverPath("poles")
    , handle = h
  )
  on.exit(
    close(con)
  )
  
  cnt = try(readLines(con), silent = TRUE)
  if (inherits(cnt, "try-error")) {
    return(cnt)
  }
  
  file.path(
    serverPath("poles")
    , grep(
      "ndvi3g_geo_v1.*.nc4$"
      , cnt
      , value = TRUE
    )
  )
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

serverPath = function(
  server = c("ecocast", "nasanex", "poles")
  , version = 1L
) {
  switch(
    match.arg(server)
    , "ecocast" = sprintf(
      "https://ecocast.arc.nasa.gov/data/pub/gimms/3g.%s"
      , ifelse(as.integer(version) == 1, "v1", "v0")
    )
    , "nasanex" = "https://nasanex.s3.amazonaws.com"
    , "poles"   = "ftp://210.72.14.198"
  )
}

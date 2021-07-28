#' Update GIMMS NDVI3g File Inventory
#'
#' @description
#' Download the latest version of the GIMMS NDVI3g file inventory from the 
#' National Center for Atmospheric Research, NASA Earth Exchange (NEX) Amazon
#' AWS or Ames Ecological Forecasting Lab (ECOCAST). If the specified endpoint 
#' is not reachable (e.g., if there is no active internet connection), the 
#' latest local version of the file inventory is used.
#'
#' @param server \code{character}. Specifies the remote server to use. Currently
#' available options are \code{"poles"} (default), \code{"nasanex"} and 
#' \code{"ecocast"}.
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
#' GIMMS NDVI3g.v1 is currently available from ECOCAST and The National Center 
#' for Atmospheric Research until end 2015 and comes in NetCDF (\code{.nc4}) 
#' format. In contrast, NDVI3g.v0 is available as ENVI binary imagery and 
#' available from ECOCAST (NASANEX) until end 2013 (2012) only.
#'
#' @seealso
#' \code{\link{rearrangeFiles}}.
#' 
#' @references 
#' The National Center for Atmospheric Research (2018). A Big Earth Data 
#' Platform for Three Poles. Global GIMMS NDVI3g v1 dataset (1981-2015). 
#' Available online at \url{http://poles.tpdc.ac.cn/en/data/9775f2b4-7370-4e5e-a537-3482c9a83d88/}
#' (accessed on 2021-04-15). 
#' 
#' @examples
#' \dontrun{
#' updateInventory()
#' updateInventory(server = "nasanex", version = 0)
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
  server = c("poles", "nasanex", "ecocast")
  , version = 1L
  , quiet = FALSE
) {
  
  server = match.arg(server)
  
  ## check version
  version = checkVersion(
    server = server
    , version = version
  )
  
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
  
  ## append `server` and `version` attributes
  attributes(fls) = list(
    server = server
    , version = version
  )
  
  ## return files
  return(fls)
}


### update from ecocast -----

updateEcocast <- function(version = 1L) {
  
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
  
  ## query `poles` ftp info
  nfo = getPolesFTPOpts()
  
  h = curl::new_handle(
    connecttimeout = 30L
    , dirlistonly = TRUE
    , userpwd = paste(
      nfo[["gimms.poles.username"]]
      , nfo[["gimms.poles.password"]]
      , sep = ":"
    )
  )
  con = curl::curl(
    serverPath(
      "poles"
      , ip = nfo[["gimms.poles.server"]]
    )
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
    serverPath(
      "poles"
      , ip = nfo[["gimms.poles.server"]]
    )
    , grep(
      "ndvi3g_geo_v1.*.nc4$"
      , cnt
      , value = TRUE
    )
  )
}


getPolesFTPOpts = function() {
  
  ## query local `poles` ftp info
  opts = c(
    "gimms.poles.server"
    , "gimms.poles.username"
    , "gimms.poles.password"
  )
  
  nfo = Map(
    getOption
    , opts
  )
  
  ## if not available, retrieve online `poles` ftp info
  if (any(lengths(nfo) == 0)) {
    
    # read from website
    nfo = try(
      getPolesFTPInfo()
      , silent = TRUE
    )
    if (inherits(nfo, "try-error")) {
      return(nfo)
    }
    
    # save to `options()`
    do.call(
      options
      , nfo
    )
  }
  
  return(
    nfo
  )
}


getPolesFTPInfo = function(
  con = NULL # enables testing with local files
) {
  
  ## read website content
  if (is.null(con)) {
    con = url(
      "http://poles.tpdc.ac.cn/en/data/9775f2b4-7370-4e5e-a537-3482c9a83d88/"
    )
    on.exit(
      close(
        con
      )
    )
  }
  
  lns = readLines(
    con
  )
  
  ## find lines with ftp download info
  ftp_dl_info = grep(
    "Ftp (server|username|password)"
    , lns
    , value = TRUE
  )
  
  ## extract ftp components
  Map(
    function(x, y) {
      txt = grep(
        x
        , ftp_dl_info
        , value = TRUE
      )
      
      regmatches(
        txt
        , regexpr(
          y
          , text = txt
        )
      )
    }
    # ftp components
    , x = list(
      "gimms.poles.server" = "Ftp server"
      , "gimms.poles.username" = "Ftp username"
      , "gimms.poles.password" = "Ftp password"
    )
    # regex
    , y = list(
      paste(
        rep("\\d{2,3}", 4)
        , collapse = "."
      )
      , "download_\\d+"
      , "\\d+"
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
  , ip = NULL
) {
  switch(
    match.arg(server)
    , "ecocast" = sprintf(
      "https://ecocast.arc.nasa.gov/data/pub/gimms/3g.%s"
      , ifelse(as.integer(version) == 1, "v1", "v0")
    )
    , "nasanex" = "https://nasanex.s3.amazonaws.com"
    , "poles"   = paste0(
      "ftp://"
      , ip
    )
  )
}

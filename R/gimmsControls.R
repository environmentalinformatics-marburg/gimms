### download function ----------------------------------------------------------

downloader <- function(x, dsn = getwd(), overwrite = FALSE, quiet = TRUE,
                       mode = "wb", cores = 1L, ...) {

  ### single core

  if (cores == 1L) {

    ## download files one after another
    for (i in x) {
      destfile <- paste0(dsn, "/", basename(i))
      if (file.exists(destfile) & !overwrite) {
        if (!quiet)
          cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
      } else {
        try(download.file(i, destfile = destfile, mode = mode,
                          quiet = quiet, ...), silent = TRUE)
      }
    }


    ### multi-core

  } else {

    ## initialize cluster
    cl <- parallel::makePSOCKcluster(cores)

    ## export required variables
    parallel::clusterExport(cl, c("x", "cores", "dsn", "overwrite", "quiet",
                                  "mode"), envir = environment())

    ## download files in parallel
    parallel::parLapply(cl, x, function(i) {
      destfile <- paste0(dsn, "/", basename(i))
      if (file.exists(destfile) & !overwrite) {
        if (!quiet)
          cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
      } else {
        try(download.file(i, destfile = destfile, mode = mode,
                          quiet = quiet, ...), silent = TRUE)
      }
    })

    ## deregister parallel backend
    parallel::stopCluster(cl)
  }

  ## return downloaded files
  gimms_out <- paste(dsn, basename(x), sep = "/")
  return(gimms_out)
}


### create gimms-specific envi header file -------------------------------------

createHeader <- function(x, header) {

  ## default content of gimms ndvi3g-related header file
  if (missing(header))
    header <- paste("ENVI",
                    "description = { R-language data }",
                    "samples = 2160",
                    "lines = 4320",
                    "bands = 1",
                    "data type = 2",
                    "header offset = 0",
                    "interleave = bsq",
                    "sensor type = AVHRR",
                    "byte order = 1", sep = "\n")

  ## write .hdr for each input file to disk
  sapply(x, function(i) {
    fls <- paste0(i, ".hdr")
    writeLines(header, fls)
    return(fls)
  })
}


### check desired number of cores ----------------------------------------------

checkCores <- function(cores) {

  ## available cores
  cores_avl <- parallel::detectCores()

  ## resize if 'cores' exceeds number of available cores
  if (cores > cores_avl) {
    cores <- cores_avl - 1
    warning("Desired number of cores is invalid. Resizing parallel cluster to ", cores, " cores.")
  }

  return(cores)
}


### check existence of target folder -------------------------------------------

checkDsn <- function(dsn) {

  ## if 'dsn' doesn't exist, ask user if it should be created
  if (!dir.exists(dsn)) {
    answer <- readline(paste("Target folder", dsn, "doesn't exist.",
                             "Do you wish to create it? (yes/no) \n"))

    if (answer == "yes") {
      dir.create(dsn)
    } else {
      stop(paste("Target folder", dsn, "doesn't exist. Aborting operation...\n"))
    }
  }

  return(invisible())
}


### (re-)set system locale -----

setLocale <- function(reset = FALSE, ...) {

  ## switch current locale to us standard
  if (!reset) {
    if (Sys.info()[["sysname"]] == "Windows") {
      invisible(Sys.setlocale(category = "LC_TIME", locale = "C"))
    } else {
      invisible(Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8"))
    }

    ## reset locale
  } else {
    Sys.setlocale(category = "LC_TIME", ...)
  }

  return(invisible())
}


### get date strings from version-1 files -----

getV1dates <- function(x, pos1 = 15L, pos2 = 23L, suffix = TRUE) {

  fls <- substr(basename(x), pos1, pos2)

  lst <- strsplit(fls, "_")
  yrs <- sapply(lst, "[[", 1)
  yrs <- substr(yrs, 3, 4)
  mts <- sapply(lst, "[[", 2)

  # back-up current locale and subsequently swith to us standard
  locale <- Sys.getlocale(category = "LC_TIME")
  setLocale()

  lst <- lapply(seq(mts), function(i) {
    mts <- if (mts[i] == "0106") {
      rep(tolower(month.abb[1:6]), each = 2)
    } else {
      rep(tolower(month.abb[7:12]), each = 2)
    }

    if (suffix)
      mts <- paste0(mts, rep(c("15a", "15b"), length(mts) / 2))

    dts <- paste0(yrs[i], mts)
    return(dts)
  })

  # revoke locale time adjustment and return date strings
  setLocale(reset = TRUE, locale = locale)
  return(unlist(lst))
}

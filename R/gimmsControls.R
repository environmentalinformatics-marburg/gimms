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

### `getPolesFTPOpts()` ----

## define new `gimms.poles` options
vals = list(
  "gimms.poles.server" = "210.72.14.198"
  , "gimms.poles.username" = "download_292468"
  , "gimms.poles.password" = "08423816"
)

## backup old options
old_opts = Map(
  getOption
  , names(
    vals
  )
)

## set new temporary options
do.call(
  options
  , vals
)

## test if new options are identified correctly
new_opts = gimms:::getPolesFTPOpts()

expect_inherits(
  new_opts
  , class = "list"
  , info = "`*Opts()` output inherits from class 'list'"
)

expect_identical(
  names(
    new_opts
  )
  , c(
    "gimms.poles.server"
    , "gimms.poles.username"
    , "gimms.poles.password"
  )
  , info = "names of `*Opts()` output are set correctly"
)

## restore old -"- options
do.call(
  options
  , old_opts
)


### `getPolesFTPInfo()` ----

## open file connection
con = file(
  system.file(
    "extdata"
    , "poles.html"
    , package = "gimms"
  )
)

## retrieve ftp info
ftp_info = gimms:::getPolesFTPInfo(
  con
)

expect_inherits(
  ftp_info
  , class = "list"
  , info = "`*Info()` output inherits from class 'list'"
)

expect_identical(
  names(
    ftp_info
  )
  , c(
    "gimms.poles.server"
    , "gimms.poles.username"
    , "gimms.poles.password"
  )
  , info = "names of `*Info()` output are set correctly"
)

## close file connection
close(
  con
)

library(gimms)

expect_identical(
  gimms:::serverPath("poles")
  , "ftp://download_403193:72855006@210.72.14.198"
)


### . checkVersion() ----

## ecocast
for (version in c(0, 1)) {
  expect_identical(
    gimms:::checkVersion(
      "ecocast"
      , version
    )
    , version
  )
}

expect_error(
  gimms:::checkVersion(
    "ecocast"
    , 2
  )
)

## nasanex
expect_identical(
  gimms:::checkVersion(
    "nasanex"
    , 0
  )
  , 0
)

expect_error(
  gimms:::checkVersion(
    "nasanex"
    , 1
  )
)

## poles
expect_identical(
  gimms:::checkVersion(
    "poles"
    , 1
  )
  , 1
)

expect_error(
  gimms:::checkVersion(
    "poles"
    , 0
  )
)

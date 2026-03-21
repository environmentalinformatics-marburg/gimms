library("tinytest")
library("checkmate")
using("checkmate")

x = as.numeric(Kendall::PrecipGL)


### `numeric` input ----

## returns `NA` for flat input
expect_scalar_na(
  significantTau(
    rep(1L, 10L)
  )
  , info = "flat input returns NA"
)

## without prewhitening
res = significantTau(
  x
  , p = 0.001
  , prewhitening = FALSE
  , df = TRUE
)

expect_data_frame(
  res
  , types = "numeric"
  , any.missing = FALSE
  , nrows = 1L
  , col.names = "named"
  , info = "output inherits from class `data.frame`"
)

expect_names(
  names(res)
  , identical.to = c("tau", "p")
  , info = 'output contains columns ["tau", "p"]'
)

## with prewhitening
res2 = significantTau(
  x
  , p = 0.001
  , prewhitening = TRUE
  , df = TRUE
)

expect_false(
  identical(
    res
    , res2
  )
  , info = "prewhitening changes the results"
)


### `Raster*` input ----

idx = seq(
  1L
  , raster::nlayers(kili3g.v0)
  , 24L
)

res3 = significantTau(
  kili3g.v0[[idx]]
  , p = 0.01
  , prewhitening = FALSE
)

expect_inherits(
  res3
  , class = "RasterLayer"
  , info = "output inherits from class `RasterLayer`"
)

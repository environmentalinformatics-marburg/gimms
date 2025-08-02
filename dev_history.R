# 2021-07-27 ====

## code related to issue #4, available online:
## https://github.com/environmentalinformatics-marburg/gimms/issues/4


### `updateInventory()` ----

onl1 = updateInventory() # `poles`, fails
onl2 = updateInventory(
  server = "nasanex"
  , version = 0
)
onl3 = updateInventory(
  server = "ecocast"
)

debug(
  gimms:::updatePoles
)
onl1 = updateInventory()
undebug(
  gimms:::updatePoles
)


### `downloadGimms()` ----

(
  files_1985 = downloadGimms(
    x = as.Date("1985-01-01")
    , y = as.Date("1985-12-31")
  )
)


### `rasterizeGimms()` ----

(
  raster_1985 = rasterizeGimms(
    files_1985
  )
)

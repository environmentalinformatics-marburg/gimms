fls = updateInventory(server = "poles")
dts = monthlyIndices(fls, timestamp = TRUE)

dts_plv1 = Map(
  function(x, y) {
    dts[y:(y + 11)]
  }
  , fls
  , seq(1, length(dts), 12)
)

saveRDS(
  dts_plv1
  , "inst/extdata/dates_plv1.rds"
)

## code to create `system.file("extdata/dates_plv1.rds", package = "gimms")`
onl = updateInventory(
  server = "poles"
  , version = 1L
)

lst = mapply(
  monthlyIndices
  , onl
  , MoreArgs = list(
    timestamp = TRUE
  )
  , SIMPLIFY = FALSE
)

saveRDS(
  lst
  , "inst/extdata/dates_plv1.rds"
)

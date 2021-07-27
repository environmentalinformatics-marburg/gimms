# 2021-07-27 ====

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

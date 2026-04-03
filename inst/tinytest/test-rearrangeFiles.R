x = updateInventory(
  server = "nasanex"
  , version = 0L
)

out = rearrangeFiles(x)

decades = unique(
  dirname(out)
) |> 
  basename()

expect_identical(
  decades
  , c("1980s", "1990s", "2000s", "2010s")
  , info = "rearranged files are in order by decade"
)

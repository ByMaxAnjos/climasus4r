test_that(".grid_cache_key is stable, order-insensitive and input-sensitive", {
  k <- climasus4r:::.grid_cache_key
  expect_identical(k(c("1200013", "1100015"), months = 1:3, agg_fun = "mean"),
                   k(c("1100015", "1200013"), months = 3:1, agg_fun = "mean"))
  base <- k(c("1100015", "1200013"), months = 1:3, agg_fun = "mean")
  expect_false(base == k("1100015", months = 1:3, agg_fun = "mean"))
  expect_false(base == k(c("1100015", "1200013"), months = 1:4, agg_fun = "mean"))
  expect_false(base == k(c("1100015", "1200013"), months = 1:3, agg_fun = "sum"))
  expect_false(k(NULL, uf = "AC") == k(NULL, uf = "RO"))
  expect_false(k(NULL, uf = "AC", biome = NULL) == k(NULL, uf = "AC", biome = "Cerrado"))
  expect_false(k(NULL, bbox = c(-75, -35, -28, 6)) == k(NULL, bbox = c(-70, -12, -60, -7)))
})

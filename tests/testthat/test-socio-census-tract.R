make_tracts_sf <- function() {
  codes <- c("110020505000001", "110020505000002", "999999999999999")
  geoms <- lapply(0:2, function(i) {
    sf::st_polygon(list(rbind(c(i, 0), c(i + 1, 0), c(i + 1, 1), c(i, 1), c(i, 0))))
  })
  sf::st_sf(code_tract = codes, geometry = sf::st_sfc(geoms, crs = 4674))
}

test_that("sus_census_join signature keeps muni as default level", {
  f <- formals(sus_census_join)
  expect_equal(eval(f$join_level), c("muni", "tract"))
  expect_null(f$join_tract_col)
})

test_that("join_level = 'tract' requires dataset = 'tracts'", {
  skip_if_not_installed("sf")
  expect_error(
    sus_census_join(make_tracts_sf(), dataset = "population",
                    join_level = "tract", verbose = FALSE),
    "tracts"
  )
})

test_that("tract-level join keeps rows, sf class and geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  fake <- arrow::arrow_table(
    code_tract = c("110020505000001", "110020505000002", "110020505000003"),
    code_muni  = c(1100205, 1100205, 1100205),
    name_muni  = "Porto Velho",
    V0001      = c(10, 20, 30),
    V0007      = c(5, 6, 7)
  )
  testthat::local_mocked_bindings(read_tracts = function(...) fake, .package = "censobr")

  x <- make_tracts_sf()
  out <- sus_census_join(x, dataset = "tracts", tracts_dataset = "Basico",
                         year = 2022, join_level = "tract",
                         census_vars = c("V0001", "V0007"),
                         use_cache = FALSE, verbose = FALSE)
  expect_s3_class(out, "sf")
  expect_s3_class(out, "climasus_df")
  expect_equal(nrow(out), 3L)
  expect_equal(out$V0001, c(10, 20, NA))
  expect_equal(out$V0007, c(5, 6, NA))
  expect_false(any(grepl("_sum$|code_muni", names(out))))
  expect_equal(sf::st_crs(out), sf::st_crs(x))
  expect_equal(sf::st_geometry(out)[[2]], sf::st_geometry(x)[[2]])

  # census_vars = NULL: all non-geographic columns, no code_muni/name_muni
  all <- sus_census_join(x, dataset = "tracts", year = 2022, join_level = "tract",
                         use_cache = FALSE, verbose = FALSE)
  expect_true(all(c("V0001", "V0007") %in% names(all)))
  expect_false(any(c("name_muni", "code_muni") %in% names(all)))
  expect_equal(nrow(all), 3L)
})

test_that("tract-level join aborts when no tract column", {
  skip_if_not_installed("sf")
  x <- make_tracts_sf()
  names(x)[1] <- "foo"
  expect_error(
    sus_census_join(x, dataset = "tracts", join_level = "tract",
                    use_cache = FALSE, verbose = FALSE),
    "join_tract_col"
  )
})

test_that("heat_vulnerability indicators: values, CI and zero denominator", {
  df <- dplyr::tibble(a = c(10, 5, 0), b = c(100, 50, 0))
  df <- ensure_climasus_df(df, stage = "spatial", type = "tracts")
  out <- sus_socio_compute_indicators(
    df, indicators = "improvised_hh_rate",
    col_mapping = list(hh_improvised = "a", total_hh = "b"),
    verbose = FALSE
  )
  expect_equal(out$ind_improvised_hh_rate[1:2], 100 * c(10, 5) / c(100, 50))
  expect_true(is.na(out$ind_improvised_hh_rate[3]))
  expect_true(all(out$ind_improvised_hh_rate_low >= 0 & out$ind_improvised_hh_rate_high <= 100, na.rm = TRUE))
  expect_true(is.na(out$ind_improvised_hh_rate_low[3]))

  cat <- sus_socio_list_indicators(category = "heat_vulnerability")
  expect_equal(nrow(cat), 10L)
})

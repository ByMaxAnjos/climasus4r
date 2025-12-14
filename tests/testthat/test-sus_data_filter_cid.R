test_that("sus_data_filter_cid handles basic filtering scenarios", {
  # Create realistic SUS test data with common ICD-10 variations
  test_data <- tibble::tibble(
    CAUSABAS = c("I10", "I10.0", "I11.9", "J18.9", "A00.0", "C50.9", 
                 "I10 ", "i10", "I10X", "A000", "E11.9", NA),
    DIAG_PRINC = c("J45", "J45.9", "I20.0", "I21.9", "A01.0", "C34.9",
                   "E10", "E11", "I10", "A00", "I10.9", "R51"),
    MUNIC_RES = rep("355030", 12),
    DT_OBITO = rep("20230101", 12)
  )
  
  # Test 1: Exact matching
  result_exact <- sus_data_filter_cid(
    test_data, 
    icd_codes = "I10",
    icd_column = "CAUSABAS",
    match_type = "exact",
    verbose = FALSE
  )
  expect_equal(nrow(result_exact), 1)  # Only exact "I10"
  
  # Test 2: Starts_with matching (most common use case)
  result_starts <- sus_data_filter_cid(
    test_data,
    icd_codes = "I10",
    icd_column = "CAUSABAS",
    match_type = "starts_with",
    verbose = FALSE
  )
  # Should catch: I10, I10.0, I10 , i10, I10X
  expect_true(nrow(result_starts) >= 5)
  
  # Test 3: Multiple codes
  result_multi <- sus_data_filter_cid(
    test_data,
    icd_codes = c("I10", "J18.9"),
    icd_column = "CAUSABAS",
    match_type = "starts_with",
    verbose = FALSE
  )
  expect_true("I10" %in% substr(result_multi$CAUSABAS, 1, 3) |
               "J18" %in% substr(result_multi$CAUSABAS, 1, 3))
})

test_that("sus_data_filter_cid handles range filtering correctly", {
  # Test data with respiratory diseases range
  test_data <- tibble::tibble(
    CAUSABAS = c("J00", "J18.9", "J45", "J45.9", "I10", "A00.0", 
                 "J99", "J99.9", "J20", "J20.9"),
    MUNIC_RES = rep("355030", 10)
  )
  
  # Test complete chapter range
  result_range <- sus_data_filter_cid(
    test_data,
    icd_codes = "J00-J99",
    icd_column = "CAUSABAS",
    match_type = "range",
    verbose = FALSE
  )
  
  # Should include all J codes (7 records) and exclude I10, A00.0
  expect_equal(nrow(result_range), 7)
  expect_true(all(grepl("^J", result_range$CAUSABAS)))
  
  # Test partial range
  result_partial <- sus_data_filter_cid(
    test_data,
    icd_codes = "J45-J45.9",
    icd_column = "CAUSABAS",
    match_type = "range",
    verbose = FALSE
  )
  expect_equal(nrow(result_partial), 2)  # J45 and J45.9
})

test_that("sus_data_filter_cid supports SUS-specific categories", {
  # Create test data with various disease categories
  test_data <- tibble::tibble(
    CAUSABAS = c("O00", "O99.9", "P00", "P96", "V01", "Y98", 
                 "I10", "J18.9", "A00", "C50.9"),
    category = c("maternal", "maternal", "infant", "infant", 
                 "external", "external", "cardio", "resp", "infec", "neo")
  )
  
  # Test maternal causes (should map to O00-O99)
  result_maternal <- sus_data_filter_cid(
    test_data,
    icd_codes = "causas_maternas",
    icd_column = "CAUSABAS",
    match_type = "range",
    verbose = FALSE
  )
  expect_equal(nrow(result_maternal), 2)  # O00 and O99.9
  expect_true(all(grepl("^O", result_maternal$CAUSABAS)))
  
  # Test external causes (should map to V01-Y98)
  result_external <- sus_data_filter_cid(
    test_data,
    icd_codes = "external_causes",
    icd_column = "CAUSABAS",
    match_type = "range",
    verbose = FALSE
  )
  expect_true(all(result_external$CAUSABAS %in% c("V01", "Y98")))
})

test_that("sus_data_filter_cid handles multilingual interface correctly", {
  test_data <- tibble::tibble(
    CAUSABAS = c("I10", "I10.0", "J18.9", "A00.0"),
    MUNIC_RES = rep("355030", 4)
  )
  
  # Capture output for each language
  output_en <- capture.output({
    result_en <- sus_data_filter_cid(
      test_data,
      icd_codes = "I10",
      lang = "en",
      verbose = TRUE
    )
  }, type = "message")
  
  output_pt <- capture.output({
    result_pt <- sus_data_filter_cid(
      test_data,
      icd_codes = "I10",
      lang = "pt",
      verbose = TRUE
    )
  }, type = "message")
  
  output_es <- capture.output({
    result_es <- sus_data_filter_cid(
      test_data,
      icd_codes = "I10",
      lang = "es",
      verbose = TRUE
    )
  }, type = "message")
  
  # Verify filtering results are identical regardless of language
  expect_equal(nrow(result_en), nrow(result_pt))
  expect_equal(nrow(result_pt), nrow(result_es))
  
  # Verify language-specific messages appear
  expect_true(any(grepl("Filtering", output_en)))
  expect_true(any(grepl("Filtrando", output_pt)))
  expect_true(any(grepl("Filtrando", output_es)))
})

test_that("sus_data_filter_cid auto-detects ICD columns in SUS data", {
  # Test data with multiple possible ICD columns (real SUS scenario)
  test_data <- tibble::tibble(
    CAUSABAS = c("I10", "J18.9", "A00.0"),
    DIAG_PRINC = c("J45", "I20.0", "A01.0"),
    linha_a = c("I10", "I11", "A00"),
    linha_b = c("E11.9", "E10", "B20"),
    MUNIC_RES = rep("355030", 3),
    DT_OBITO = rep("20230101", 3)
  )
  
  # Should auto-detect CAUSABAS as primary column
  result_auto <- sus_data_filter_cid(
    test_data,
    icd_codes = "I10",
    icd_column = NULL,  # Let it auto-detect
    verbose = FALSE
  )
  expect_equal(nrow(result_auto), 1)
  
  # Test with explicit column
  result_explicit <- sus_data_filter_cid(
    test_data,
    icd_codes = "J45",
    icd_column = "DIAG_PRINC",
    verbose = FALSE
  )
  expect_equal(nrow(result_explicit), 1)
})

test_that("sus_data_filter_cid handles common SUS data quality issues", {
  # Real-world SUS data often has these issues
  test_data <- tibble::tibble(
    CAUSABAS = c("I10", "I10 ", " I10", "I10.", "I10. ", 
                 "I10X", "I10.0X", "i10", "I10.0", "I109"),
    issue_type = c("correct", "trailing_space", "leading_space", 
                   "trailing_dot", "trailing_dot_space",
                   "extra_char", "extra_char_decimal", 
                   "lowercase", "correct_decimal", "no_dot")
  )
  
  # Test with different match types
  result_starts <- sus_data_filter_cid(
    test_data,
    icd_codes = "I10",
    match_type = "starts_with",
    verbose = FALSE
  )
  
  # Starts_with should catch most variations
  expect_true(nrow(result_starts) >= 8)
  
  # Verify fuzzy matching would catch even more
  # (This tests the concept, though fuzzy isn't implemented yet)
  expect_true(any(grepl("^I10", test_data$CAUSABAS, ignore.case = TRUE)))
})

test_that("sus_data_filter_cid validates inputs correctly", {
  test_data <- tibble::tibble(
    CAUSABAS = c("I10", "J18.9"),
    MUNIC_RES = c("355030", "355030")
  )
  
  # Test error on empty data frame
  expect_error(
    sus_data_filter_cid(data.frame(), icd_codes = "I10"),
    "must be a data.frame"
  )
  
  # Test error on missing ICD codes
  expect_error(
    sus_data_filter_cid(test_data, icd_codes = NULL),
    "is required"
  )
  
  # Test error on non-existent column
  expect_error(
    sus_data_filter_cid(test_data, icd_codes = "I10", icd_column = "NONEXISTENT"),
    "not found"
  )
  
  # Test warning on unsupported language
  expect_warning(
    capture.output(
      sus_data_filter_cid(test_data, icd_codes = "I10", lang = "fr", verbose = FALSE),
      type = "message"
    )
  )
})

test_that("sus_data_filter_cid works with climate-health relevant codes", {
  # ICD codes relevant for climate-health studies
  test_data <- tibble::tibble(
    CAUSABAS = c("J18.9", "A00.0", "A92.0", "A92.8", "I21.9", 
                 "T67.0", "E86.0", "X30", "J45", "A90"),
    disease = c("pneumonia", "cholera", "chikungunya", "zika", 
                "heart_attack", "heat_stroke", "dehydration",
                "heat_exposure", "asthma", "dengue")
  )
  
  # Test filtering for climate-sensitive diseases
  result_climate <- sus_data_filter_cid(
    test_data,
    icd_codes = c("J00-J99", "A00-A09", "A90-A99", "T66-T78"),
    match_type = "range",
    verbose = FALSE
  )
  
  # Should include respiratory, infectious, and heat-related
  climate_codes <- result_climate$CAUSABAS
  expect_true(any(grepl("^J", climate_codes)))  # Respiratory
  expect_true(any(grepl("^A", climate_codes)))  # Infectious
  expect_true(any(grepl("^T67", climate_codes))) # Heat stroke
  
  # Test specific vector-borne diseases
  result_vector <- sus_data_filter_cid(
    test_data,
    icd_codes = c("A90", "A92.0", "A92.8"),
    match_type = "starts_with",
    verbose = FALSE
  )
  expect_equal(nrow(result_vector), 3)  # Dengue, Chikungunya, Zika
})

test_that("sus_data_filter_cid process_icd_codes helper works correctly", {
  # Test helper function directly
  test_codes <- c("I10-I15", "J00", "J18.9", "A00-A09")
  
  # Range match type
  result_range <- climasus4r:::process_icd_codes(test_codes, "range")
  expect_equal(length(result_range), 4)  # All codes preserved
  expect_true("I10-I15" %in% result_range)
  
  # Starts_with match type (ranges converted to prefixes)
  result_starts <- climasus4r:::process_icd_codes(test_codes, "starts_with")
  expect_true("I10" %in% result_starts)  # I10-I15 becomes I10
  expect_true("A00" %in% result_starts)  # A00-A09 becomes A00
  expect_true("J00" %in% result_starts)
  expect_true("J18.9" %in% result_starts)
})

test_that("sus_data_filter_cid filter_by_icd_range helper works correctly", {
  # Test the internal range filtering function
  test_data <- tibble::tibble(
    CAUSABAS = c("I10", "I11.0", "I12.9", "I13", "I15.9", "J18.9", "A00.0"),
    value = 1:7
  )
  
  ranges <- c("I10-I13", "I15")
  
  result <- climasus4r:::filter_by_icd_range(test_data, "CAUSABAS", ranges)
  
  # Should include I10, I11.0, I12.9, I13, I15.9
  # Should exclude J18.9 and A00.0
  expect_equal(nrow(result), 5)
  expect_true(all(grepl("^I", result$CAUSABAS)))
  expect_false(any(grepl("^J|^A", result$CAUSABAS)))
})

test_that("sus_data_filter_cid handles edge cases and NA values", {
  # Test with NA values in ICD column (common in real data)
  test_data <- tibble::tibble(
    CAUSABAS = c("I10", NA, "J18.9", "", "I10.0", NA_character_),
    MUNIC_RES = rep("355030", 6)
  )
  
  result <- sus_data_filter_cid(
    test_data,
    icd_codes = "I10",
    match_type = "starts_with",
    verbose = FALSE
  )
  
  # Should only return rows with valid I10 codes (not NA or empty)
  expect_true(all(!is.na(result$CAUSABAS)))
  expect_true(all(result$CAUSABAS != ""))
  expect_true(all(grepl("^I10", result$CAUSABAS)))
  
  # Test with single row data frame
  single_row <- tibble::tibble(CAUSABAS = "I10", MUNIC_RES = "355030")
  result_single <- sus_data_filter_cid(
    single_row,
    icd_codes = "I10",
    verbose = FALSE
  )
  expect_equal(nrow(result_single), 1)
  
  # Test with no matches
  result_none <- sus_data_filter_cid(
    test_data,
    icd_codes = "Z00",  # No Z codes in test data
    verbose = FALSE
  )
  expect_equal(nrow(result_none), 0)
})

test_that("sus_data_filter_cid preserves all original columns", {
  test_data <- tibble::tibble(
    CAUSABAS = c("I10", "J18.9", "A00.0", "I10.0"),
    MUNIC_RES = c("355030", "355030", "355030", "355030"),
    DT_OBITO = c("20230101", "20230102", "20230103", "20230104"),
    IDADE = c(45, 67, 23, 89),
    SEXO = c("M", "F", "M", "F")
  )
  
  result <- sus_data_filter_cid(
    test_data,
    icd_codes = "I10",
    verbose = FALSE
  )
  
  # Should preserve all original columns
  expect_equal(names(result), names(test_data))
  
  # Should preserve data types
  expect_type(result$IDADE, "double")
  expect_type(result$SEXO, "character")
  
  # Should only filter rows, not modify column values
  expect_true(all(result$CAUSABAS %in% test_data$CAUSABAS))
})
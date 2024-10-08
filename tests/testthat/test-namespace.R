test_that("pac_parse_namespace", {
  expect_true(length(pac_parse_namespace(readLines("files/NAMESPACE_joint.txt"), enc = "UTF-8")) == 10)
  expect_warning(pac_parse_namespace(readLines("files/NAMESPACE_dupfield.txt"), enc = "UTF-8"), "duplicate symbol")
  expect_error(pac_parse_namespace(readLines("files/NAMESPACE_wrongfield.txt"), enc = "UTF-8"), "unknown namespace directive")
  expect_error(pac_parse_namespace(readLines("files/NAMESPACE_wrongparam.txt"), enc = "UTF-8"), "empty name in directive 'exportPattern' in 'NAMESPACE' file")
})


test_that("pacs::pac_namespace", {
  skip_if_offline()
  expect_identical(pac_readnamespace_raw("dplyr", "0.0.0.0.1"), structure(NA, package = "dplyr", version = "0.0.0.0.1"))
  expect_true(length(read_cran_file("dplyr", "0.8.0", repos = "https://cran.rstudio.com/", "NAMESPACE")) == 498)
  expect_true(length(pac_namespace("dplyr", version = "0.8.0")) == 10)
  expect_identical(sort(pac_namespace("memoise", local = TRUE)$exports), sort(base::getNamespaceExports("memoise")))
  expect_identical(pac_namespace("dplyr", "1.1.1.1"), NA)
  expect_identical(pac_namespace("WRONG"), NA)
  expect_identical(pac_namespace("WRONG", local = TRUE), NA)
  expect_silent(pac_namespace("xml2", at = NULL, version = NULL))
  expect_silent(pac_namespace("xml2", version = NULL, at = as.Date("2021-01-01")))
  expect_identical(pac_namespace("dplyr", "0.0.0.1"), NA)
})

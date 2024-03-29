test_that("crandb wrong input", {
  skip_if_offline()
  expect_identical(pacs_lifeduration(c("dplyr"), c("1.0.7", "1.6.0")), NA)
  expect_identical(pacs_lifeduration(c("dplyr", "shiny", "data.table"), c("1.0.7", "1.6.0")), c(NA, NA, NA))
  expect_identical(
    pacs_lifeduration(c("dplyr", "shiny", "3"), c("1.0.7", "1.6.0")),
    c(NA, NA, NA)
  )
})

test_that("crandb timemachine related", {
  skip_if_offline()
  skip_on_cran()
  res <- pac_timemachine("dplyr", source = "crandb")
  expect_true(isNA(res) || is.data.frame(res))
  res <- pac_lifeduration("dplyr", source = "crandb")
  expect_true(isNA(res) || inherits(res, "difftime"))
})

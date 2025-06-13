library("inDAGO")

test_that("Shiny app is generated", {
  expect_s3_class(inDAGO(), "shiny.appobj")
})

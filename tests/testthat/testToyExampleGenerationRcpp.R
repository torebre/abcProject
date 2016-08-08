context("Toy sample generation using Rcpp")

test_that("Samples can be generated", {
  toy.example.setup <- smcToyExampleRcpp()
  SampleFunction <- toy.example.setup[["SampleFunction"]]
  sample.generated <- SampleFunction(c(0.2, 0.3), 2)

  expect_equal(length(sample.generated), 2)
  expect_equal(length(sample.generated[[1]]), 2)
  expect_equal(length(sample.generated[[2]]), 2)
})

context("Toy sample generation")

test_that("Correct number of samples are generated", {
  toy.example.setup <- smcToyExample()
  SampleFunction <- toy.example.setup[["SampleFunction"]]
  sample.generated <- SampleFunction(c(0.2, 0.3), 2)

  expect_equal(length(sample.generated), 2)
  expect_equal(length(sample.generated[[1]]), 2)
  expect_equal(length(sample.generated[[2]]), 2)
})

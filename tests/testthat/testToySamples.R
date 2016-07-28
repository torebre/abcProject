context("Toy sample generation")

test_that("No errors when creating toy samples without replicates", {
  toy.example.configuration <-
    smcToyExample(create.debug.variables = T)

  toy.example.result <-
    Smc(
      toy.example.configuration,
      max.iterations = 5,
      alpha = 0.95,
      number.of.particles = 10,
      number.of.replicates = 1,
      stop.epsilon = 0.01,
      verbose = F
    )
})


test_that("No errors when creating toy samples with replicates", {
  toy.example.configuration <-
    smcToyExample(create.debug.variables = T)

  toy.example.result <-
    Smc(
      toy.example.configuration,
      max.iterations = 5,
      alpha = 0.95,
      number.of.particles = 10,
      number.of.replicates = 2,
      stop.epsilon = 0.01,
      verbose = F
    )
})


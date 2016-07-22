context("MA example generation")

test_that("SMC algorithm can process MA example using raw distance function", {
      ma.configuration <- smcMovingAverageExample(use.raw.distance.function = T,
                                                  create.debug.variables = T)
  ma.example.results <-
    Smc(
      ma.configuration,
      max.iterations = 10,
      alpha = 0.9,
      number.of.particles = 100,
      number.of.replicates = 1,
      resample.ratio = 0.9,
      stop.epsilon = 0.01,
      start.epsilon = 10000,
      verbose = F
    )
})


test_that("SMC algorithm can process MA example using raw distance function with replicates", {
  ma.configuration <- smcMovingAverageExample(use.raw.distance.function = T,
                                              create.debug.variables = T)
  ma.example.results <-
    Smc(
      ma.configuration,
      max.iterations = 10,
      alpha = 0.9,
      number.of.particles = 10,
      number.of.replicates = 5,
      resample.ratio = 0.9,
      stop.epsilon = 0.01,
      start.epsilon = 10000,
      verbose = F
    )
})

test_that("SMC algorithm can process MA example using autocovariance distance function", {
  ma.configuration <- smcMovingAverageExample(use.raw.distance.function = F,
                                              create.debug.variables = T)
  ma.example.results <-
    Smc(
      ma.configuration,
      max.iterations = 10,
      alpha = 0.9,
      number.of.particles = 10,
      number.of.replicates = 1,
      resample.ratio = 0.9,
      stop.epsilon = 0.01,
      start.epsilon = 10000,
      verbose = F
    )
})

test_that("SMC algorithm can process MA example using autocovariance distance function with replicates", {
  ma.configuration <- smcMovingAverageExample(use.raw.distance.function = F,
                                              create.debug.variables = T)
  ma.example.results <-
    Smc(
      ma.configuration,
      max.iterations = 10,
      alpha = 0.9,
      number.of.particles = 10,
      number.of.replicates = 1,
      resample.ratio = 0.9,
      stop.epsilon = 0.01,
      start.epsilon = 10000,
      verbose = F
    )
})




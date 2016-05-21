

RunExample <- function() {

ma.configuration <- smcMovingAverageExample(create.debug.variables = T)

ma.example.results <-
  Smc(
    ma.configuration,
    max.iterations = 100000,
    alpha = 0.9,
    number.of.particles = 1000,
    number.of.replicates = 1,
    stop.epsilon = 0.01,
    start.epsilon = 10000,
    verbose = T
  )

SampleFunction <- maConfiguration[["SampleFunction"]]
ForwardKernelSample <- maConfiguration[["ForwardKernelSample"]]
DistanceFunction <- maConfiguration[["DistanceFunction"]]
GenerateRandomPrior <- maConfiguration[["GenerateRandomPrior"]]


thetas <- GenerateRandomPrior(number.of.particles)
series.sample <- SampleFunction(thetas, number.of.replicates)
weights <- rep(1/number.of.particles, number.of.particles)

}

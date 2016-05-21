

RunExperiment1 <- function(alpha.to.use, number.of.particles.to.use, file.name.prefix) {
toy.example.configuration.1 <- smcToyExample(create.debug.variables = T)
toy.example.results.1 <-
  Smc(
    toy.example.configuration.1,
    max.iterations = 100000,
    alpha = alpha.to.use,
    number.of.particles = number.of.particles.to.use,
    number.of.replicates = 1,
    stop.epsilon = 0.01,
    verbose = T
  )

  state.to.visualise <- length(toy.example.results.1[["effective.sample.sizes"]])

effective.sample.size <-
  toy.example.results.1[["effective.sample.sizes"]][state.to.visualise]
epsilon <- toy.example.results.1[["epsilons"]][state.to.visualise]
thetas <- unlist(toy.example.results.1[["all.thetas"]][state.to.visualise])
weights <- unlist(toy.example.results.1[["all.weights"]][state.to.visualise])
particles <- toy.example.results.1[["all.particles"]][[state.to.visualise]]

use.thetas <- c(-3, 3)


png(paste(file.name.prefix, "_histogram.png", sep = "")) #, width = 480, height = 480 * 3/2)
PlotHistogram(thetas, use.thetas)
dev.off();

# PlotParticles(particles, weights, use.max = distance.max)
# PlotEpsilonTrace(smc.resultoy.example.results.1, state.to.visualise, use.run.length = run.length)
# PlotEssTrace(toy.example.results.1, state.to.visualise, use.run.length = run.length)

CreateGifAnimation(toy.example.results.1, paste(file.name.prefix, "_full.gif"))
}

RunExperiment2 <- function() {
toy.example.configuration.2 <- smcToyExample(create.debug.variables = T)
toy.example.results.2 <-
  Smc(
    toy.example.configuration.2,
    max.iterations = 1000,
    alpha = 0.95,
    number.of.particles = 1000,
    number.of.replicates = 1,
    stop.epsilon = 0.01,
    verbose = T
  )


state.to.visualise <- length(toy.example.results.2[["effective.sample.sizes"]])

effective.sample.size <-
  toy.example.results.2[["effective.sample.sizes"]][state.to.visualise]
epsilon <- toy.example.results.2[["epsilons"]][state.to.visualise]
thetas <- unlist(toy.example.results.2[["all.thetas"]][state.to.visualise])
weights <- unlist(toy.example.results.2[["all.weights"]][state.to.visualise])
particles <- toy.example.results.2[["all.particles"]][[state.to.visualise]]

use.thetas <- c(-3, 3)


png("experiment2_histogram.png") #, width = 480, height = 480 * 3/2)
PlotHistogram(thetas, use.thetas)
dev.off();

# PlotParticles(particles, weights, use.max = distance.max)
PlotEpsilonTrace(smc.resultoy.example.results.2, state.to.visualise, use.run.length = run.length)
PlotEssTrace(toy.example.results.2, state.to.visualise, use.run.length = run.length)

# CreateGifAnimation(toy.example.results.2, "toy_example_results_2.gif")
}

RunExperiment3 <- function() {
toy.example.configuration.3 <- smcToyExample(create.debug.variables = T)
toy.example.results.3 <-
  Smc(
    toy.example.configuration.3,
    max.iterations = 1000,
    alpha = 0.99,
    start.epsilon = 100,
    number.of.particles = 1000,
    number.of.replicates = 1,
    stop.epsilon = 0.01,
    verbose = T
  )

state.to.visualise <- length(toy.example.results.3[["effective.sample.sizes"]])

effective.sample.size <-
  toy.example.results.3[["effective.sample.sizes"]][state.to.visualise]
epsilon <- toy.example.results.3[["epsilons"]][state.to.visualise]
thetas <- unlist(toy.example.results.3[["all.thetas"]][state.to.visualise])
weights <- unlist(toy.example.results.3[["all.weights"]][state.to.visualise])
particles <- toy.example.results.3[["all.particles"]][[state.to.visualise]]

use.thetas <- c(-3, 3)

png("experiment2_histogram.png") #, width = 480, height = 480 * 3/2)
PlotHistogram(thetas, use.thetas)
dev.off();

# PlotParticles(particles, weights, use.max = distance.max)
PlotEpsilonTrace(smc.resultoy.example.results.3, state.to.visualise, use.run.length = run.length)
PlotEssTrace(toy.example.results.3, state.to.visualise, use.run.length = run.length)

# CreateGifAnimation(toy.example.results.3, "toy_example_results_3.gif", skip.frames = 3)
}

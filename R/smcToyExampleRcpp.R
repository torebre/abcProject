#' SMC toy example.
#'
#' @export
smcToyExampleRcpp <- function(create.debug.variables = F) {
  toyExample <- structure(list(), class = "smcConfiguration")
  observation <- 0

  toyExample[["GenerateRandomPrior"]] <- function(number.of.particles) {
    # Uniform [-10, 10]
    as.list(runif(number.of.particles, min = -10, max = 10))
  }

  EvaluateTheta <- function() {
    # The distribution is uniform
    1 / 20
  }

  toyExample[["EvaluateLikelihood"]] <- function(my.x, my.theta) {
    0.5 * dnorm(my.x, mean = my.theta, sd = 1) + 0.5 * dnorm(my.x, mean = my.theta, sd = 1 / 10)
  }

  InternalDistanceFunction <- function(my.sample.replicates) {
    abs(unlist(my.sample.replicates) - observation)
  }

  toyExample[["DistanceFunction"]] <- InternalDistanceFunction

  EvaluateLikelihoodSum <-
    function(my.sample.list, my.current.epsilon) {
      sum(sapply(my.sample.list, function(x) {
        InternalDistanceFunction(x) < my.current.epsilon
      }))
    }

  debug.variables <- new.env(parent = emptyenv())
  debug.variables$avg.acc.rate <- list()
  debug.variables$empirical.variance <- list()
  debug.variables$alive.particles <- list()
  debug.variables$accepted <- list()

  toyExample[["GetDebugVariables"]] <- function() {
    debug.variables
  }


  toyExample[["ForwardKernelSample"]] <- function(samples.old,
                                                  theta.old,
                                                  my.current.epsilon,
                                                  my.weights) {
    temp <- sapply(theta.old, function(x) {
      x
    })
    dim(temp) <- NULL
    empirical.variance <- var(temp) # temp[my.weights > 0])

    my.number.of.particles <- length(samples.old)
    my.number.of.replicates <- length(samples.old[[1]])

    # mean.theta <- mean(theta.old)
    # empirical.variance <- 0
    # for(i in 1:kNumberOfParticles) {
    #   empirical.variance <- empirical.variance + my.weights[i] * (theta.old[i] - mean.theta)^2
    # }

    # A copy of the lists is made here
    samples.new <- samples.old
    theta.new <- theta.old

    accepted <- 0
    alive.particles <- 0

    for (j in 1:my.number.of.particles) {
      # print(paste("my.weights[", j, "] = ", my.weights[j]))

      # if (my.weights[j] <= 0) {
      #   next
      # }

      # for (k in 1:100) {
      theta.candidate <-
        rnorm(1, mean = theta.old[[j]], sqrt(2 * empirical.variance))

      debug.variables$empirical.variance[[length(debug.variables$avg.acc.rate) + 1]] <-
        sqrt(2 * empirical.variance)

      replicates.new <- rep(NA, my.number.of.replicates)

      for (i in 1:my.number.of.replicates) {
        replicates.new[i] <- generateSamples(theta.candidate, 1, -1)
      }

      # The prior is uniform and the random walk is coming from
      # a symmetric distribution so the only term left in the
      # Metropolis-Hastings ratio is the likelihood
      if (my.weights[j] == 0) {

        # Try to see what happens when the particles with no
        # weight are left alone

        # theta.new[j] <- theta.candidate
        # samples.new[[j]] <- replicates.new


        # accepted <- accepted + 1
      }
      else {
        # New as nominator, old as denominator
        old.likelihood <-
          EvaluateLikelihoodSum(samples.new[j], my.current.epsilon)
        if (old.likelihood == 0) {
          next
        }

        metropolis.hastings.ratio <-
          EvaluateLikelihoodSum(replicates.new, my.current.epsilon) / old.likelihood

        if (runif(1) <= min(1, metropolis.hastings.ratio)) {
          theta.new[j] <- theta.candidate
          samples.new[[j]] <- replicates.new

          accepted <- accepted + 1
        }

        alive.particles <- alive.particles + 1

      }
    }

    if(create.debug.variables) {
      debug.variables$accepted[[length(debug.variables$accepted) + 1]] <-
        accepted
      debug.variables$avg.acc.rate[[length(debug.variables$avg.acc.rate) + 1]] <-
        accepted / alive.particles
      debug.variables$alive.particles[[length(debug.variables$alive.particles) + 1]] <-
        alive.particles
    }

    return(list(theta = theta.new, samples = samples.new))
  }

  toyExample[["SampleFunction"]] <-
    function(my.thetas, my.number.of.replicates) {
      generateSamples(my.thetas, my.number.of.replicates, 42)
    }

  toyExample[["ExtractSamples"]] <- function(sample.indices, particles) {
    resampled.particles <- list()
    counter <- 1
    for(i in sample.indices) {
      resampled.particles[[counter]] <- particles[[i]]
      counter <- counter + 1
    }
    resampled.particles
  }

  toyExample[["ExtractThetas"]] <- function(sample.indices, thetas) {
    thetas[sample.indices]
  }

  toyExample
}

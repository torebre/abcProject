#' Performs a run using a Sequential Markov Chain (SMC) method.
#'
#' This is an implementation of a SMC method. It will do a run
#' based on the input parameters.
#'
#' @export
Smc <-
  function(smc.configuration,
           max.iterations = 40,
           alpha = 0.9,
           number.of.particles = 100,
           resample.ratio = 0.5,
           start.epsilon = 10,
           stop.epsilon = 0.1,
           number.of.replicates = 1,
           verbose = F) {
    SampleFunction <- smc.configuration[["SampleFunction"]]
    ForwardKernelSample <-
      smc.configuration[["ForwardKernelSample"]]
    DistanceFunction <- smc.configuration[["DistanceFunction"]]
    GenerateRandomPrior <-
      smc.configuration[["GenerateRandomPrior"]]
    ExtractSamples <- smc.configuration[["ExtractSamples"]]
    ExtractThetas <- smc.configuration[["ExtractThetas"]]

    variable.env <- new.env(parent = emptyenv())

    # Create an initial set of particles
    variable.env$thetas <-GenerateRandomPrior(number.of.particles)
    variable.env$particles <- SampleFunction(variable.env$thetas, number.of.replicates)

    counter <- 0
    resample.limit <- number.of.particles * resample.ratio

    variable.env$weights <-
      rep(1 / number.of.particles, number.of.particles)
    effective.sample.size <- number.of.particles
    current.epsilon <- start.epsilon

    variable.env$effective.sample.sizes <- list()
    variable.env$epsilons <- list()
    variable.env$all.thetas <- list()
    variable.env$all.weights <- list()
    variable.env$all.particles <- list()

    Resample <- function(my.env) {
      resampling.indices <-
        sample(
          1:number.of.particles,
          number.of.particles,
          replace = T,
          prob = my.env$weights
        )
      my.env$particles <- ExtractSamples(resampling.indices, my.env$particles)
      my.env$thetas <- ExtractThetas(resampling.indices, my.env$thetas)
      my.env$weights <-
        rep(1 / number.of.particles, number.of.particles)
    }

    AddValuesToLists <- function(my.counter) {
      variable.env$epsilons[[counter]] <- current.epsilon
      variable.env$effective.sample.sizes[[counter]] <-
        effective.sample.size
      variable.env$all.thetas[[counter]] <- variable.env$thetas
      variable.env$all.weights[[counter]] <- variable.env$weights
      variable.env$all.particles[[counter]] <-variable.env$particles
    }

    counter <- 1

    while (T) {
      # Adaptation
      if (verbose) {
        print(paste("Current epsilon: ", current.epsilon))
      }

      AddValuesToLists(counter)
      counter <- counter + 1

      if (current.epsilon <= stop.epsilon) {
        break
      }

      if(FindNextEpsilon(
        current.epsilon,
        current.epsilon,
        variable.env$particles,
        variable.env$weights,
        alpha,
        DistanceFunction
      ) < 0) {
        if(verbose) {
        print(paste("No change in epsilon:", current.epsilon))
        }
        epsilon.new <- current.epsilon
      }
      else {
      # Find next epsilon
      epsilon.new <- uniroot(function(epsilon.candidate) {
        FindNextEpsilon(
          epsilon.candidate,
          current.epsilon,
          variable.env$particles,
          variable.env$weights,
          alpha,
          DistanceFunction
        )
      }, interval = c(0, current.epsilon), extendInt = "no", check.conv = T)$root
      }

      if(verbose) {
      print(paste("epsilon.new:", epsilon.new))
      }

      # TODO Only compute the weights once, not here and in FindNextEpsilon
      variable.env$weights <-
        NormalizeVector(
          variable.env$weights * CalculateWeightUpdates(
            variable.env$particles,
            current.epsilon,
            epsilon.new,
            DistanceFunction
          )
        )

      if (epsilon.new < current.epsilon) {
        current.epsilon <- epsilon.new
      }

      if(sum(variable.env$weights) == 0) {
       effective.sample.size <- 0
      }
      else {
      effective.sample.size <-
        ComputeEffectiveSampleSize(variable.env$weights)
      }

      if(verbose) {
      print(paste("Effective sample size:", effective.sample.size))
      }

      # Resampling
      if (effective.sample.size < resample.ratio * number.of.particles) {
        if (verbose) {
          print(paste("Resampling at step: ", counter))
        }
        Resample(variable.env)
      }

      # Mutation
      iterated.samples <-
        ForwardKernelSample(
          variable.env$particles,
          variable.env$thetas,
          current.epsilon,
          variable.env$weights
        )

      variable.env$particles <- iterated.samples$samples
      variable.env$thetas <- iterated.samples$theta

      if (verbose) {
        print(paste(
          "Counter: ",
          counter,
          " Effective sample size: ",
          effective.sample.size
        ))
      }

      if (counter == max.iterations) {
        break
      }
    }

    Resample(variable.env)
    AddValuesToLists(counter)

    list(
      effective.sample.sizes = variable.env$effective.sample.sizes,
      epsilons = variable.env$epsilons,
      all.thetas = variable.env$all.thetas,
      all.weights = variable.env$all.weights,
      all.particles = variable.env$all.particles
    )

  }

ComputeEffectiveSampleSize <- function(weights) {
  1 / sum(weights ^ 2)
}

FindNextEpsilon <-
  function(epsilon.candidate,
           my.current.epsilon,
           my.particles,
           my.previous.weights,
           alpha,
           DistanceFunction) {
    ess.old <- ComputeEffectiveSampleSize(my.previous.weights)
    weight.updates <-
      CalculateWeightUpdates(my.particles,
                             my.current.epsilon,
                             epsilon.candidate,
                             DistanceFunction)

    if (sum(weight.updates) == 0) {
      return(-alpha * ess.old)
    }
    weights.new <- NormalizeVector(my.previous.weights * weight.updates)
    ess.new <- ComputeEffectiveSampleSize(weights.new)

    return(ess.new - alpha * ess.old)
  }

NormalizeVector <- function(my.vector) {
  my.vector.sum <- sum(my.vector)
  if(my.vector.sum == 0) {
    return(rep(0, length(my.vector)))
  }
  my.vector / my.vector.sum
}

#'
#' @export
CalculateWeightUpdates <-
  function(my.samples,
           my.old.epsilon,
           my.new.epsilon,
           DistanceFunction) {
    sapply(1:length(my.samples), function(x) {
      CalculateWeightUpdateForParticle(my.samples,
                                       x,
                                       my.old.epsilon,
                                       my.new.epsilon,
                                       DistanceFunction)
    })
  }

CalculateWeightUpdateForParticle <-
  function(my.samples,
           my.particle.number,
           my.old.epsilon,
           my.new.epsilon,
           DistanceFunction) {
    sum1 <-
      CalculateInclusionSum(my.samples[[my.particle.number]], DistanceFunction, my.new.epsilon)
    sum2 <-
      CalculateInclusionSum(my.samples[[my.particle.number]], DistanceFunction, my.old.epsilon)

    # print(paste("sum1:",sum1,"sum2:",sum2))

    if (sum1 == 0) {
      return(0)
    }

    if (sum1 == sum2) {
      return(1)
    }

    sum1 / sum2
  }

# CalculateInclusionSum <-
#   function(my.sample.replicates,
#            DistanceFunction,
#            my.epsilon) {
#
#     inclusion.sum <- 0
#     for(i in 1:length(my.sample.replicates)) {
#       if(DistanceFunction(unlist(my.sample.replicates[[i]])) <= my.epsilon) {
#         inclusion.sum <- 1 + inclusion.sum
#       }
#     }
#     inclusion.sum
#   }


# For MA debugging
CalculateInclusionSum <-
  function(my.sample.replicates,
           DistanceFunction,
           my.epsilon) {
    inclusion.sum <- 0
    number.of.rows <- dim(my.sample.replicates)[1]

    for (i in 1:number.of.rows) {
      if (DistanceFunction(my.sample.replicates[i, ]) < my.epsilon) {
        inclusion.sum <- 1 + inclusion.sum
      }
    }
    inclusion.sum
  }


CalculateWeights <-
  function(old.weight,
           inclusion.sum.nominator,
           inclusion.sum.denominator) {
    old.weight * (inclusion.sum.nominator / inclusion.sum.denominator)
  }

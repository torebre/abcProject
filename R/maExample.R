#' MA(2) example.
#'
#' @export
smcMovingAverageExample <-
  function(use.raw.distance.function = T,
           create.debug.variables = F,
           debug.space = 1) {
    maExample <- structure(list(), class = "smcConfiguration")
    theta1.actual <- 0.6
    theta2.actual <- 0.2
    series.length <- 100

    GenerateSample <- function(my.theta) {
      mu.values <- rnorm(series.length + 2, mean = 0, sd = 1)
      simulated.series <- rep(NA, series.length)

      sapply(1:series.length, function(x) {
        mu.values[x] + my.theta[1] * mu.values[x + 1] + my.theta[2] * mu.values[x + 2]
      })
    }

    ComputeAutoCovariance <- function(my.sample, lag.length) {
      sum(sapply((lag.length + 1):length(my.sample), function(x) {
        my.sample[x] * my.sample[x - lag.length]
      })) / length(my.sample)
    }

    observed.series <- GenerateSample(c(theta1.actual, theta2.actual))
    observed.series.cov1 <- ComputeAutoCovariance(observed.series, 1)
    observed.series.cov2 <- ComputeAutoCovariance(observed.series, 2)

    maExample[["observed.series"]] <- observed.series

    maExample[["GenerateSample"]] <- GenerateSample

    maExample[["GenerateRandomPrior"]] <- function(number.of.priors) {
      # -2 < theta1 < 2, theta1 + theta2 > -1, theta1 - theta2 < 1
      priors <- matrix(NA, nrow = 2, ncol = number.of.priors)

      for (i in 1:number.of.priors) {
        while(T) {
        priors[1, i] <- runif(1, min = -2, max = 2)
        priors[2, i] <- runif(1, min = -2, max = 2)
        if (priors[1, i] + priors[2, i] > -1 &&
            priors[1, i] - priors[2, i] < 1) {
          break
        }
        }
      }
      priors
    }

    EvaluateTheta <- function() {
      # The prior is uniform on a triangle with width 4 and height 2
      1 / 4
    }

    InternalDistanceFunction <- function(my.sample) {
      if (use.raw.distance.function) {
        return(sum((my.sample - observed.series) ^ 2))
      }
      # unlisted.sample <- unlist(my.sample)
      (ComputeAutoCovariance(my.sample, 1) - observed.series.cov1) ^ 2 +
        (ComputeAutoCovariance(my.sample, 2) - observed.series.cov2) ^ 2
    }

    maExample[["DistanceFunction"]] <- InternalDistanceFunction

    EvaluateLikelihoodSum <-
      function(my.sample.vector, my.current.epsilon) {
        number.of.rows <- dim(my.sample.vector)[1]

        likelihood.sum <- 0
        for (i in 1:number.of.rows) {
          if (InternalDistanceFunction(my.sample.vector[i,]) < my.current.epsilon) {
            likelihood.sum <- likelihood.sum + 1
          }
        }
        likelihood.sum
      }

    debug.variables <- new.env(parent = emptyenv())
    debug.variables$avg.acc.rate <- list()
    debug.variables$empirical.variance <- list()
    debug.variables$alive.particles <- list()
    debug.variables$accepted <- list()
    debug.variables$counter <- 0

    maExample[["GetDebugVariables"]] <- function() {
      debug.variables
    }

    maExample[["ForwardKernelSample"]] <- function(samples.old,
                                                   theta.old,
                                                   my.current.epsilon,
                                                   my.weights) {
      temp1 <- theta.old[1, ]
      temp2 <- theta.old[2, ]

      empirical.variance1 <- var(temp1)
      empirical.variance2 <- var(temp2)

      my.number.of.particles <- length(samples.old)
      my.number.of.replicates <- dim(samples.old[[1]])[1]

      samples.new <- samples.old
      theta.new <- theta.old

      accepted <- 0
      alive.particles <- 0

      for (j in 1:my.number.of.particles) {
        theta.candidate <-
          c(rnorm(1, mean = theta.old[1, j], sqrt(2 * empirical.variance1)),
            rnorm(1, mean = theta.old[2, j], sqrt(2 * empirical.variance2)))

        # Move inside rectangle if outside
        if(theta.candidate[1] < -2) {
          theta.candidate[1] <- theta.candidate[1] + 4
        }
        else if(theta.candidate[1] > 2) {
          theta.candidate[1] <- theta.candidate[1] - 4
        }

        if(theta.candidate[2] < -1) {
          theta.candidate[2] <- theta.candidate[2] + 2
        }
        else if(theta.candidate[2] > 1) {
          theta.candidate[2] <- theta.candidate[2] - 2
        }

        if(theta.candidate[1] + theta.candidate[2] < -1) {
            # Outside valid region on the left side
          theta.candidate[2] <- -theta.candidate[2]
          theta.candidate[1] <- theta.candidate[1] + 2

        }
        else if(theta.candidate[1] - theta.candidate[2] > 1) {
          # Outside valid region on right side
          theta.candidate[2] <- -theta.candidate[2]
          theta.candidate[1] <- theta.candidate[1] - 2
        }

        replicates.new <-
          matrix(nrow = my.number.of.replicates, ncol = series.length)

        for (i in 1:my.number.of.replicates) {
          replicates.new[i, ] <- GenerateSample(theta.candidate)
        }

        # The prior is uniform and the random walk is coming from
        # a symmetric distribution so the only term left in the
        # Metropolis-Hastings ratio is the likelihood


        if (my.weights[j] != 0) {
          # New as nominator, old as denominator
          old.likelihood <-
            EvaluateLikelihoodSum(samples.new[[j]], my.current.epsilon)

          if (old.likelihood == 0) {

            print("old.likelihood is 0")

            next
          }

          metropolis.hastings.ratio <-
            EvaluateLikelihoodSum(replicates.new, my.current.epsilon) / old.likelihood

          # print(paste("MH-ratio: ", metropolis.hastings.ratio))

          if (runif(1) <= min(1, metropolis.hastings.ratio)) {
            theta.new[, j] <- theta.candidate
            samples.new[[j]] <- replicates.new

            accepted <- accepted + 1
          }
          alive.particles <- alive.particles + 1
        }
      }

      if (create.debug.variables && debug.variables$counter %% debug.space == 0) {
        debug.variables$accepted[[length(debug.variables$accepted) + 1]] <-
          accepted
        debug.variables$avg.acc.rate[[length(debug.variables$avg.acc.rate) + 1]] <-
          accepted / alive.particles
        debug.variables$alive.particles[[length(debug.variables$alive.particles) + 1]] <-
          alive.particles
        debug.variables$counter <- debug.variables$counter + 1
      }

      return(list(theta = theta.new, samples = samples.new))
    }

    SampleFunctionInternal <-
      function(my.thetas, my.number.of.replicates) {
        my.samples <- list()

        for (i in 1:dim(my.thetas)[2]) {
          my.replicates <-
            matrix(nrow = my.number.of.replicates, ncol = series.length)
          for (j in 1:my.number.of.replicates) {
            my.replicates[j, ] <- GenerateSample(my.thetas[, i])
          }
          my.samples[[i]] <- my.replicates
        }
        return(my.samples)
      }

    maExample[["SampleFunction"]] <- SampleFunctionInternal

    maExample[["ExtractSamples"]] <-
      function(sample.indices, particles) {
        resampled.particles <- list()
        counter <- 1
        for (i in sample.indices) {
          resampled.particles[[counter]] <- particles[[i]]
          counter <- counter + 1
        }
        resampled.particles
      }

    maExample[["ExtractThetas"]] <- function(sample.indices, thetas) {
      thetas[, sample.indices]
    }

    maExample
  }

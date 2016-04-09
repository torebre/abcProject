#' Visualise toy example state
#'
#' @export
VisualiseToyExampleState <-
  function(smc.result, state.to.visualise = -1) {
    if (!requireNamespace("latex2exp", quietly = TRUE)) {
      stop("latex2exp needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if (state.to.visualise == -1) {
      state.to.visualise <- length(smc.result[["all.thetas"]])
    }

    effective.sample.size <-
      smc.result[["effective.sample.sizes"]][state.to.visualise]
    epsilon <- smc.result[["epsilons"]][state.to.visualise]
    thetas <- unlist(smc.result[["all.thetas"]][state.to.visualise])
    weights <- unlist(smc.result[["all.weights"]][state.to.visualise])
    particles <- unlist(smc.result[["all.particles"]][state.to.visualise])

    distance.max <- max(unlist(result$all.particles))

    op <- par("mfrow" = c(2, 2))
    PlotHistogram(thetas)
    PlotParticles(particles, weights, use.max = distance.max)
    PlotEpsilonTrace(smc.result, state.to.visualise)
    PlotEssTrace(smc.result, state.to.visualise)
    par(op)
  }


#' Plot particles.
#'
#' @export
PlotParticles <- function(particles, weights, cex = 0.1, use.max) {
  my.number.of.replicates <- length(particles[[1]])
  my.number.of.particles <- length(particles) * my.number.of.replicates

  # print(paste("My number of particles: ", my.number.of.particles))

  sector.size <- 2 * pi / my.number.of.particles
  if(missing(use.max)) {
    particle.max <- max(unlist(particles))
  }
  else {
    particle.max <- use.max
  }

  cumulative.angle <- 0
  x.coords <- rep(NA, my.number.of.particles)
  y.coords <- rep(NA, my.number.of.particles)
  colors <- rep(NA, my.number.of.particles)

  for(j in 1:length(particles)) {
    for(k in 1:length(particles[[1]])) {
      particle.distance <- particles[[j]][k]
      # if(particle.max < particle.distance) {
      #   particle.max <- particle.distance
      # }
      current.particle.index <- (j - 1) * my.number.of.replicates + k
      x.coords[current.particle.index] <- cos(cumulative.angle) * particle.distance
      y.coords[current.particle.index] <- sin(cumulative.angle) * particle.distance
      cumulative.angle <- cumulative.angle + sector.size

      # print(paste("Weights: ", weights[i]))

      # if(is.na(weights[[j]][k])) {
      #   colors[current.particle.index] <- "black"
      # }
      # else {

      # print(paste("Particle: ", current.particle.index, ". Weight: ", weights[j]))
      #   colors[current.particle.index] <- hsv(h = weights[j])

      # }

    }

  }

  # for (i in 1:my.number.of.particles) {
  #   particle.distance <- particles[i]
  #   x.coords[i] <- cos(cumulative.angle) * particle.distance
  #   y.coords[i] <- sin(cumulative.angle) * particle.distance
  #   cumulative.angle <- cumulative.angle + sector.size
  #
  #   # print(paste("Weights: ", weights[i]))
  #
  #   if(is.na(weights[i])) {
  #     colors[i] <- "black"
  #   }
  #   else {
  #     colors[i] <- hsv(h = weights[i])
  #   }
  #
  #   # if (weights[i] > 0) {
  #   #   colors[i] <- "red"
  #   # }
  #   # else {
  #   #   colors[i] <- "blue"
  #   # }
  # }

  plot(
    x.coords,
    y.coords,
    pch = 16,
    cex = cex,
    xlim = c(-particle.max, particle.max),
    ylim = c(-particle.max, particle.max),
    col = colors,
    ann = F
  )
  title("d(x, 0)")
}


#' Plot histogram.
#'
#' @export
PlotHistogram <- function(thetas) {
  if (!requireNamespace("latex2exp", quietly = TRUE)) {
    stop("latex2exp needed for this function to work. Please install it.",
         call. = FALSE)
  }

  theta.upper <- max(thetas)
  theta.lower <- min(thetas)

  Posterior2 <- function(my.theta) {
    0.5 * dnorm(0, mean = my.theta, sd = 1) + 0.5 * dnorm(0, mean = my.theta, sd = 1 / 10)
  }

  VectorizedPosterior2 <- Vectorize(Posterior2)

  hist(
    unlist(thetas),
    freq = F,
    ann = F
  )
  title(latex2exp::latex2exp("$\\theta$"))

  curve(
    VectorizedPosterior2,
    from = -5,
    to = 5,
    add = T,
    col = "green"
  )
}

#' Plot epsilon trace.
#'
#' @export
PlotEpsilonTrace <- function(smc.result, state.to.visualise) {
  plot(
    unlist(smc.result$epsilons)[1:state.to.visualise],
    type = "l",
    ann = F,
    xlim = c(0, state.to.visualise),
    ylim = c(0, 10)
  )
  title(
    main = latex2exp::latex2exp("$\\epsilon_{n}$"),
    ylab = latex2exp::latex2exp("$\\epsilon_{n}$"),
    xlab = "n"
  )
}

#' Plot ESS trace.
#'
#' @export
PlotEssTrace <- function(smc.result, state.to.visualise) {
  ess.trace <- unlist(smc.result$effective.sample.sizes)[1:state.to.visualise]
  ess.max <- max(ess.trace)
  plot(
    ess.trace,
    type = "l",
    ylim = c(0, ess.max),
    ann = F,
    xlim = c(0, state.to.visualise)
  )
  # abline(h = 5000)
  title(main = "ESS",
        xlab = "n",
        ylab = "EFF")
}


#' @export
CreateGifAnimation <- function(smc.result, movie.name, skip.frames = 1) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop("animation needed for this function to work. Please install it.",
         call. = FALSE)
  }

  run.length <- length(smc.result$epsilons)
  animation::saveGIF(
    for(j in seq(1, run.length, by = skip.frames)) {
      VisualiseToyExampleState(smc.result, state.to.visualise = j)
    }, movie.name = movie.name, interval = 0.3)
}


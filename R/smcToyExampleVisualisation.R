#' Visualise toy example state
#'
#' @export
VisualiseToyExampleState <-
  function(smc.result, state.to.visualise = -1, use.max, use.run.length, use.thetas) {
    if (!requireNamespace("latex2exp", quietly = TRUE)) {
      stop("latex2exp needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if(missing(use.max)) {
      distance.max <- max(unlist(result$all.particles))
    }
    else {
      distance.max <- use.max
    }

    if(missing(use.run.length)) {
      run.length <- state.to.visualise
    }
    else {
      run.length <- use.run.length
    }

    if (state.to.visualise == -1) {
      state.to.visualise <- length(smc.result[["all.thetas"]])
    }

    effective.sample.size <-
      smc.result[["effective.sample.sizes"]][state.to.visualise]
    epsilon <- smc.result[["epsilons"]][state.to.visualise]
    thetas <- unlist(smc.result[["all.thetas"]][state.to.visualise])
    weights <- unlist(smc.result[["all.weights"]][state.to.visualise])
    particles <- smc.result[["all.particles"]][[state.to.visualise]]

    op <- par("mfrow" = c(2, 2))
    PlotHistogram(thetas, use.thetas)
    # if(epsilon > 1) {
    PlotParticles(particles, weights, use.max = distance.max, epsilon = unlist(epsilon[state.to.visualise]))
    # }
    # else {
      # PlotParticles(particles, weights, use.max = distance.max)
    # }
    PlotEpsilonTrace(smc.result, state.to.visualise, use.run.length = run.length)
    PlotEssTrace(smc.result, state.to.visualise, use.run.length = run.length)
    par(op)
  }


#' Plot particles.
#'
#' @export
PlotParticles <- function(particles, weights, cex = 0.1, use.max, epsilon) {
  epsilon.draw <- F
  if (!missing(epsilon) && !requireNamespace("plotrix", quietly = TRUE)) {
    stop("plotrix needed to draw epsilon circle. Please install it.",
         call. = FALSE)
  }
  else if(!missing(epsilon)) {
    epsilon.draw <- T
  }

  my.number.of.replicates <- length(particles[[1]])
  my.number.of.particles <- length(particles) * my.number.of.replicates

  sector.size <- 2 * pi / my.number.of.particles
  if(missing(use.max)) {
    particle.max <- max(abs(unlist(particles)))
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
      current.particle.index <- (j - 1) * my.number.of.replicates + k
      x.coords[current.particle.index] <- cos(cumulative.angle) * particle.distance
      y.coords[current.particle.index] <- sin(cumulative.angle) * particle.distance
      cumulative.angle <- cumulative.angle + sector.size
      colors[current.particle.index] <- hsv(h = 0, s = (1 - weights[j]), v = (1 - weights[j]))
    }
  }

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
  if(epsilon.draw) {
    plotrix::draw.circle(0, 0, epsilon)
  }
  title("d(x, 0)")
}


#' Plot histogram.
#'
#' Note that the thetas are assumed to have been resampled
#' before being used as input here.
#'
#' @export
PlotHistogram <- function(thetas, use.thetas) {
  if (!requireNamespace("latex2exp", quietly = TRUE)) {
    stop("latex2exp needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(missing(use.thetas)) {
    theta.upper <- max(thetas)
    theta.lower <- min(thetas)
  }
  else {
    theta.lower <- use.thetas[1]
    theta.upper <- use.thetas[2]
  }

  Posterior2 <- function(my.theta) {
    0.5 * dnorm(0, mean = my.theta, sd = 1) + 0.5 * dnorm(0, mean = my.theta, sd = 1 / 10)
  }

  VectorizedPosterior2 <- Vectorize(Posterior2)

  MASS::truehist(
    unlist(thetas),
    ann = F,
    xlim = c(theta.lower, theta.upper),
    ylim = c(0, 2.5)
  )
  title(latex2exp::latex2exp("$\\theta$"))

  curve(
    VectorizedPosterior2,
    from = theta.lower,
    to = theta.upper,
    add = T,
    col = "green"
  )
}

#' Plot epsilon trace.
#'
#' @export
PlotEpsilonTrace <- function(smc.result, state.to.visualise, use.run.length, use.eps.max) {
  if(missing(use.run.length)) {
    run.length <- state.to.visualise
  }
  else {
    run.length <- use.run.length
  }

  if(!missing(use.eps.max)) {
    eps.max <- use.eps.max
  }
  else {
    eps.trace <- unlist(smc.result$epsilons)
    eps.max <- max(eps.trace)
  }

  plot(
    unlist(smc.result$epsilons)[1:state.to.visualise],
    type = "l",
    ann = F,
    xlim = c(0, run.length),
    ylim = c(0, eps.max)
  )
  title(
    ylab = latex2exp::latex2exp("$\\epsilon_{n}$"),
    xlab = "n"
  )
}

#' Plot ESS trace.
#'
#' @export
PlotEssTrace <- function(smc.result, state.to.visualise, use.run.length, resample.limit) {
  if(missing(use.run.length)) {
    run.length <- state.to.visualise
  }
  else {
    run.length <- use.run.length
  }

  ess.trace <- unlist(smc.result$effective.sample.sizes)[1:state.to.visualise]
  ess.max <- max(ess.trace)
  plot(
    ess.trace,
    type = "l",
    ylim = c(0, ess.max),
    ann = F,
    xlim = c(0, run.length)
  )
  if(!missing(resample.limit)) {
    abline(h = resample.limit)
  }
  title(xlab = "n",
        ylab = "ESS")
}


#' @export
CreateGifAnimation <- function(smc.result, movie.name, skip.frames = 1) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop("animation needed for this function to work. Please install it.",
         call. = FALSE)
  }

  distance.max <- max(unlist(smc.result$all.particles))
  run.length <- length(smc.result$epsilons)
  all.thetas <- unlist(smc.result$all.thetas)
  # use.thetas <- c(min(all.thetas), max(all.thetas))
  use.thetas <- c(-3, 3)

  animation::saveGIF(
    for(j in seq(1, run.length, by = skip.frames)) {
      VisualiseToyExampleState(smc.result, state.to.visualise = j,
                               use.max = distance.max, use.run.length = run.length, use.thetas = use.thetas)
    }, movie.name = movie.name, interval = 0.3)
}


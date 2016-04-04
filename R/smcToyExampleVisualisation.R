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

    op <- par("mfrow" = c(2, 2))
    PlotHistogram(thetas)
    PlotParticles(particles, weights)
    PlotEpsilonTrace(smc.result, state.to.visualise)
    PlotEssTrace(smc.result, state.to.visualise)
    par(op)
  }


#' Plot particles.
#'
#' @export
PlotParticles <- function(particles, weights) {
  my.number.of.particles <- length(particles)
  sector.size <- 2 * pi / my.number.of.particles
  particle.max <- max(particles)
  cumulative.angle <- 0
  x.coords <- rep(NA, my.number.of.particles)
  y.coords <- rep(NA, my.number.of.particles)
  colors <- rep(NA, my.number.of.particles)
  for (i in 1:my.number.of.particles) {
    particle.distance <- particles[i]
    x.coords[i] <- cos(cumulative.angle) * particle.distance
    y.coords[i] <- sin(cumulative.angle) * particle.distance
    cumulative.angle <- cumulative.angle + sector.size

    if (weights[i] > 0) {
      colors[i] <- "red"
    }
    else {
      colors[i] <- "blue"
    }
  }

  plot(
    x.coords,
    y.coords,
    pch = 16,
    cex = 0.1,
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

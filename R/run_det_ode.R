#' Run ODE
#'
#' Run ordinary differential equation model
#'
#' @param init.inf integer, the number of initial infection
#' @param herd.size integer, the size of the herd
#' @param r0 positive number, the basic reproduction number
#' @param fever.duration a positive number, the days of fever (infectious)
#' @param milk.loss.dur a positive number, the days of milk loss
#' @param cow.prod.lifespan a positive number, the productive lifespan of a cow
#'     in days.
#' @param include.prod.lifespan a logical, should background demographics
#'     be considered with a default of \code{FALSE}
#' @param base.milk.prod a positive number, the baseline milk production
#' @param sympt.milk.prod a positive number, the milk production when the cow
#'     is symptomatic
#' @param recovered.milk.prod a positive number, the production among recovered
#'     cows.
#' @param discard.sick.prod a logical, discard infected milk from production
#' @param disease.induced.mortality a positive number between 0 and 1
#'     representing the mortality rate from disease.
#' @param prop.cull a positive number between 0 and 1 representing the
#'     proportion of cows who recover from infection who are sent to market
#' @param sim.length a positive number, number of days to run the simulation
#' @param gamma_multiplier a number, representing the impact on the recovery
#'     rate where values greater than 0 represent faster removal and values
#'     less than 0 represent longer recovery times.
#' @param beta_multiplier a number, represent the impact on the contact rate
#'     rate where values less than 0 indicate less contact while values
#'     greater than 0 indicate more contact.
#' @param intervention_date  a positive number represent the day number of an
#'     an intervention with a default of \code{NULL} representing no
#'     intervention taking place.
#' @param intervention_stop a positive number representing the day that the
#'     intervention in the beta_multiple stops. Removal rates are assumed to
#'     continue after the intervention and stop dates.
#' @param p_asymptomatic a number between 0 and 1, the proportion of the transmission
#'     that is asymptomatic.
#' @param sigmoid_fun a logical, if true a sigmoid function will be utilized to
#'     phase in the gamma mulipltier based on the intervention date.
#' @param add_milk a logical, if true milk production will be added to the
#'     simulation.
#'
#' @importFrom deSolve ode
#' @export
run_det_ode <- function(init.inf = 1,
                        herd.size = 500,
                        r0 = 1.20,
                        fever.duration = 1.15,
                        milk.loss.dur = 7,
                        cow.prod.lifespan = 365.25 * 2, # 2 years
                        include.prod.lifespan = FALSE,
                        base.milk.prod = 100, # in units of lbs per day
                        sympt.milk.prod = 100 - 25,
                        recovered.milk.prod = 80,
                        discard.sick.prod = FALSE,
                        disease.induced.mortality = 0,
                        prop.cull = 0.0,
                        sim.length = 365,
                        gamma_multiplier = 0,
                        beta_multiplier = 0,
                        intervention_date = NULL,
                        intervention_stop = sim.length,
                        p_asymptomatic = 0,
                        sigmoid_fun = FALSE,
                        add_milk = TRUE) {
  stopifnot(disease.induced.mortality >= 0 || disease.induced.mortality < 1)

  nonzero <- 0.000001
  to_nonzero <- function(x, nz = nonzero) {
    dplyr::if_else(x == 0, nz, x)
  }
  # incubation.duration <- to_nonzero(incubation.duration)
  fever.duration <- to_nonzero(fever.duration)
  milk.loss.dur <- to_nonzero(milk.loss.dur)

  if (milk.loss.dur == fever.duration) milk.loss.dur <- milk.loss.dur + nonzero
  if (milk.loss.dur < fever.duration) stop("Milk loss duration must be longer than fever duration")


  if (discard.sick.prod) {
    sympt.milk.prod <- 0
  }


  state <- c(
    "S" = herd.size - init.inf, # Symptomatic compartment
    "I" = init.inf, # Infectious compartment
    "A" = 0, # Asymptomatic compartment
    "B" = 0, # Symptomatic, but not infectious compartment
    "R" = 0, # Recovered compartment
    "Ra" = 0, # Asymptomatic Recovered compartment
    "D" = 0, # Death from disease
    "C" = 0, # Culled compartment
    "Z" = 0, # Dummy compartment to count cows that were infected but died due to productive lifespan
    "M" = 0,
    "Infected" = 0 # Ever infected
  ) # Milk compartment

  # Set time in units of days
  init.time <- 0
  end.time <- sim.length
  t.step.dur <- 1 / 24 # time step of an hour
  time <- seq(from = init.time, to = end.time, by = t.step.dur)

  # For the time being, let's just look at the fixed values given in the report.
  # Cow parameters
  R0est <- r0 # 1/(1 - HIT)


  parms <- c(
    beta = R0est / (fever.duration) / herd.size,
    gamma = 1 / fever.duration,
    alpha = 1 / (milk.loss.dur - fever.duration),
    mu = (1 / cow.prod.lifespan) *
      dplyr::if_else(include.prod.lifespan, 1, 0),
    phi = disease.induced.mortality,
    m = base.milk.prod,
    p = 1 - sympt.milk.prod / base.milk.prod,
    p_asymptomatic = p_asymptomatic,
    c = prop.cull
  )


  # Write the SIR model in absolute time
  SIRMmod <- function(time_in, State, pars) {
    with(as.list(c(State, pars)), {
      N <- sum(State) - M - C - D - Z - Infected
      if (!sigmoid_fun) {
        gamma0 <- ifelse((!is.null(intervention_date) &&
          time_in > intervention_date),
        gamma * (1 + gamma_multiplier), gamma
        )
        beta0 <- ifelse((!is.null(intervention_date) &&
          time_in > intervention_date) &&
          time_in <= intervention_stop,
        beta * (1 + beta_multiplier), beta
        )
      } else {
        gamma0 <- ifelse(!is.null(intervention_date) &&
          time_in > intervention_date,
        gamma * (1 + sigmoid_fun(time_in, intervention_date) *
          gamma_multiplier),
        gamma
        )
        beta0 <- ifelse(!is.null(intervention_date) &&
          time_in > intervention_date &&
          time_in <= intervention_stop,
        beta * (1 + sigmoid_fun(time_in, intervention_date) *
          beta_multiplier),
        beta
        )
      }

      dS <- -beta0 * S * (I + A) + N * mu - S * mu
      dI <- beta0 * S * (I + A) * (1 - p_asymptomatic) - gamma0 * I - I * mu
      dA <- beta0 * S * (I + A) * p_asymptomatic - gamma0 * A - A * mu
      dB <- (gamma0 * I) * (1 - phi) - alpha * B - mu * B
      dR <- (1 - c) * alpha * B - mu * R
      dRa <- gamma0 * A - mu * A
      dC <- alpha * c * B
      dD <- gamma0 * I * phi
      dZ <- mu * R + mu * I + mu * B
      dM <- m * ((S + R + I) + (B) * (1 - p))
      dInfected <- beta0 * S * I
      return(list(c(dS, dI, dA, dB, dR, dRa, dD, dC, dZ, dM, dInfected)))
    })
  }

  out <- deSolve::ode(state, time, SIRMmod, parms,
    rtol = 1e-15, maxsteps = 500000
  )

  out <- as.data.frame(out)

  if (add_milk) {
    out$milk_production <- with(
      out,
      (S + I + A + Ra) * base.milk.prod + sympt.milk.prod * B + R * recovered.milk.prod
    )
  }

  return(out)
}


#' Format deterministic output
#'
#' @param dat a dataset generate from the function
#' @param states a vector representing the states used in the compartmental model
#' @return a data.frame formatted with the appropriate column names
#'
#' @export
format_det_ode_output <- function(dat, states = compartment_defn$state) {
  x <- data.frame(dat)
  if (ncol(x) == 1L + length(states)) {
    colnames(x) <- c("time", states, "Infected")
  } else {
    colnames(x) <- c("time", states, "Infected", "milk_production")
  }

  class(x) <- c("rRsurveillace_deterministic", "data.frame")
  return(x)
}

#' Ploting function
#'
#' @param x an object
#' @param ... other parameters
#' @importFrom ggplot2 ggplot aes geom_line autoplot
#' @importFrom ggsci scale_color_nejm scale_fill_nejm
#' @export

plot.rRsurveillace_deterministic <- function(x, ...) {
  x |>
    ggplot2::ggplot(ggplot2::aes(time, Infected)) +
    ggplot2::geom_line() +
    ggsci::scale_color_nejm() +
    ggsci::scale_fill_nejm() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      y = "Cumulative Infected (number)",
      x = "Time"
    )
}

#' Sigmoid function
#'
#' A sigmoid function
#'
#' @param x a double, the timepoint at which to evaluate the function
#' @param x0 a double, the starting timepoint of the sigmoid
#' @param k a double, representing the speed of the sigmoid to 1
#'
#' @return a double
#'
#' @export
sigmoid_fun <- function(x, x0 = 4, k = 2) {
  1 / (1 + exp(-k * (x - x0)))
}

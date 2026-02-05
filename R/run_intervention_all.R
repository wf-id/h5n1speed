#' Estimate ODE Intervention outcomes
#'
#' Simulating the outcomes of the ODE system when an intervention of
#' some intensity gamma_post is applied at some time point given the threshold
#' of detection and applied at some delay from identification.
#'
#' @param r0_in R0 input
#' @param gamma_in pre-intervention gamma
#' @param gamma_post post-intervention gamma
#' @param gamma_post_asymptomatic post-intervention gamma for asymptomatic
#' @param sim_duration how long to run simulation
#' @param kappa the symptomatc duration
#' @param delay_time the time to delay an intervention
#' @param p_asymptomatic the proportion of the transmission that is asymptomatic
#' @param s_ini the initial size
#' @param i_ini the initial infected
#' @param r_ini the initial recovered
#' @inheritDotParams detect_infections_ode
#' @export


run_intervention_all_ode <- function(
  r0_in = 1.20,
  gamma_in = 1 / 1.15,
  gamma_post = 1 / (12 / 24),
  gamma_post_asymptomatic = gamma_in,
  sim_duration = 120,
  kappa = 1 / 7,
  delay_time = 7,
  p_asymptomatic = 0,
  s_ini = 500 - 1,
  i_ini  = 1,
  r_ini = 0,
  ...
) {
  sim_base <- run_det_ode(fever.duration = 1 / gamma_in, sim.length = 365) |>
    format_det_ode_output()

  intervention_approach <- detect_infections_ode(sim_base, ...)

  time_intervention <- intervention_approach$detection_day + delay_time

  time_intervention <- ifelse(is.null(time_intervention),
                              Inf, time_intervention)

  state <- c(S = s_ini, I = i_ini, A = 0, B= 0, R = r_ini)
  params <- c(beta = r0_in * gamma_in, gamma1 = gamma_in, gamma2 = gamma_post, kappa = kappa)
  time_in  <- seq(0, sim_duration, .1)

  simple_sir <- function(time_in, state, params) {
    with(as.list(c(state, params)), {
      gamma_use <- ifelse(time_in >= time_intervention, gamma2, gamma1)
      gamma_use_asymptomatic <- ifelse(time_in >= time_intervention, gamma2_asymptomatic, gamma1)
      N <- sum(state)
      dS <- -beta * S * (I + A) / N
      dI <- beta * S * (I + A) / N *(1 - p_asymptomatic) - gamma_use * I
      dA <- beta * S * (I + A) / N * p_asymptomatic - gamma_use_asymptomatic * A
      dB <- gamma_use * I - kappa * B
      dR <- kappa * B 
      dRa <- gamma_use_asymptomatic * A

        return(list(c(dS, dI, dA, dB, dR, dRa)))
    })
  }
  out <- tryCatch(deSolve::ode(state, time_in, simple_sir,
                               parms = params),
                  error = function(e) {
                    data.frame(time = NA_real_,
                               S = NA_real_,
                               I = NA_real_,
                               A = NA_real_,
                               B = NA_real_,
                               R = NA_real_,
                               Ra = NA_real_)
                  })

  out <- as.data.frame(out) |>
    tibble::as_tibble()

    base.milk.prod <- 100 # in units of lbs per day
    sympt.milk.prod <- 100 - 25
    recovered.milk.prod <- 80

    out$total_infect_no_intervention <- intervention_approach$total_infect_no_intervention
    out$milk_production <- with(out,
                                (S + I + A + Ra) * base.milk.prod + sympt.milk.prod * B + R * recovered.milk.prod)

  return(out)

}

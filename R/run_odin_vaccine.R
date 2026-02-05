#' Run odin vaccine
#' 
#' Run the odin model for vaccination representing flows from
#' S -> I -> B -> R.
#'
#' @param intervention_time an interger number representing when
#'     the intervention occurs
#' @param n_sims an integer number of simulation
#' @param duration an integer representing the duration of the simulation
#' @param r0 a number representing the basic reproduction number
#' @param gamma_in a number the recovery rate and period of infectiousness
#' @param kappa_in a number, the rate of transition from I to B
#' @param zeta_in a number, the vaccination rate from S to R
#' @param s_initial a number, the initial population size
#' @param i_initial a number, the initial number infected
#' @param inter_scenario different scenarios
#' @param n_detected an interger, if using the second intervention the threshold
#' @importFrom data.table setDT melt `:=`
#' @export
#'

run_odin_vaccines <- function(intervention_time = 10,
                              n_sims = 1,
                              r0 = 1.20,
                              gamma_in = 1 / 1.15,
                              kappa_in = 0.17,
                              zeta_in = 0.1,
                              duration = 50,
                              s_initial = 500,
                              i_initial = 1,
                              inter_scenario = 1,
                              n_detected = 5) {

  beta_in <- r0 * gamma_in

  use <- sir_vaccine$new(nsim = n_sims,
                         t_intervention = intervention_time,
                         S_ini = s_initial,
                         I_ini = i_initial,
                         beta = beta_in,
                         gamma = gamma_in,
                         kappa = kappa_in,
                         zeta = zeta_in,
                         detect_n = n_detected,
                         intervention_scenario = inter_scenario)

  scenarios_out <- as.data.frame(use$run(0:duration))

  sim_long <- data.table::setDT(scenarios_out)
  sim_long <- data.table::melt(sim_long, id.vars = c("step", "time"))[
    , iter := stringr::str_extract(variable, "\\d+")][
    , compartment := gsub("[^a-zA-Z_]", "", variable)
  ][
    , variable := NULL
  ]
  return(sim_long)
}
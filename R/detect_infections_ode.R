#' Detect Infection from ODE Solution
#'
#' The purpose of this function is to detect an outbreak
#' under different scenarios and uses of detection.
#'
#' @param model a solution from an ODE
#' @param prop_symptomatic a numeric indicating the proportion threshold
#'     for detection considering only those in the "B" class or symptomatic.
#' @param prop_production_drop a numeric indicating the proportion drop in
#'     production to indicate an outbreak
#' @param n_detected a numeric indicating the number of detected infections
#'    which considers all of those invididuals existing in the R, Ra, A, I, and B
#'    classes.
#' @param n_infected a numeric indicating the number of actively infected
#'    individuals before detection occurs.
#'
#' @return a list object with the time points associated with the detection
#'     approach
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom utils head
#' @importFrom dplyr mutate
#'
#' @export

detect_infections_ode <- function(model,
                                  prop_symptomatic = NULL,
                                  prop_production_drop = NULL,
                                  n_detected = NULL,
                                  n_infected = NULL, ...) {
  model_fmt <- format_det_ode_output(model)

  total_infect_no_intervention <- model_fmt |>
    filter(time == max(time)) |>
    mutate(total_infect = I + B + R + A + Ra + D) |>
    pull(total_infect)

  if (!is.null(prop_symptomatic)) {
    first_detection <- head(
      filter(model_fmt, B >= max(S) * prop_symptomatic),
      1
    )
  } else if (!is.null(prop_production_drop)) {
    first_detection <- model_fmt |>
      filter(milk_production <= max(milk_production) *
        (1 - prop_production_drop)) |>
      head(1)
  } else if (!is.null(n_detected)) {
    first_detection <- model_fmt |>
      filter(I + B + R + A + Ra + D >= n_detected) |>
      head(1)
  } else if (!is.null(n_infected)) {
    first_detection <- model_fmt |>
      filter(I >= n_infected) |>
      head(1)
  }

  peak_day <- model_fmt |>
    filter(I == max(I)) |>
    head(1) |>
    pull(time)

  out <- list(
    detection_day = pull(first_detection, time),
    n_active_infected = pull(first_detection, I),
    peak_day = peak_day,
    n_days_to_peak = peak_day - pull(first_detection, time),
    total_infect_no_intervention = total_infect_no_intervention
  )
  return(out)
}

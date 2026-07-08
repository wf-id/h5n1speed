# Setup----
# Cluster version of the time-to-effective-strategy analysis
# (manuscript/time-to-effective-strategy.R), extended so that R0 and
# farm size N are crossed over as sensitivity dimensions in the same
# way as dev/sensitivity-analysis.R. The swept intervention variable is
# tau, the identification-to-quarantine time encoded by gamma_post.

if (Sys.info()[["sysname"]] != "Darwin") {
  .libPaths("/deac/bio/kortessisGrp/dewime23/libs")
}

library(tidyverse)
library(h5n1speed)
library(here)
library(future)
library(furrr)

remotes::install_github("medewitt/nccovid")

Sys.setenv(R_MAX_VSIZE = "100Gb")
message(sprintf("Running R v%s", getRversion()))

message("Setting up parallelism")
plan(multicore)

message("Running process")

# Sensitivity dimensions----
# Same grid as dev/sensitivity-analysis.R: the baseline scenario is
# R0 = 1.2, N = 500 and the other cells show how the tau sweep shifts as
# either knob changes.

R0_values <- c(1.2, 3, 5)
N_values <- c(250, 500, 1000, 10000)

# epi_finalsize depends only on R0, so cache it per R0 rather than
# recomputing inside every simulation.
finalsize_by_r0 <- vapply(R0_values, nccovid::epi_finalsize, numeric(1))
names(finalsize_by_r0) <- as.character(R0_values)

# Number of infected cows----
# Sweeps delay, the detection threshold n_detected, and tau (gamma_post),
# crossed with R0 and N. n_detected scales with N so the threshold is
# per-capita comparable across farm sizes (as in sensitivity-analysis.R);
# s_ini = N - 1 seeds one infection on a herd of size N.

test_grid <- tidyr::crossing(
  R0                 = R0_values,
  N                  = N_values,
  delay              = seq(0.5, 20, 0.05),
  n_detected_per_500 = seq(1, 50, 1),
  gamma_post         = 1 / (seq(.5, 48, length.out = 80) / 24)
) |>
  dplyr::mutate(n_detected = n_detected_per_500 * N / 500)

cat("# Number of infected cows ------------------")

target_out <- here::here(
  "data-raw", "sensitivity-tau-sim_n_detected.rds"
)

if (!file.exists(target_out)) {
  sims_n <- future_pmap(
    test_grid,
    function(R0, N, delay, n_detected_per_500, gamma_post, n_detected) {
      run_intervention_ode(
        r0_in      = R0,
        n_detected = n_detected,
        delay_time = delay,
        gamma_post = gamma_post,
        s_ini      = N - 1,
        i_ini      = 1
      ) |>
        dplyr::filter(time == max(time)) |>
        dplyr::mutate(
          R0_baseline = R0,
          N = N,
          delay = delay,
          n_detected_per_500 = n_detected_per_500,
          gamma_post = gamma_post,
          identification_quarantine = 1 / gamma_post * 24
        )
    },
    .options = furrr_options(seed = 1834L)
  ) |>
    bind_rows() |>
    mutate(
      avoided_infection =
        1 - (R / N) / finalsize_by_r0[as.character(R0_baseline)]
    )

  saveRDS(sims_n, target_out)
}

rm(sims_n)

# Number of symptomatic cows----
# prop_symptomatic is a proportion of the herd, so it stays comparable
# across N and needs no scaling. Delay, prop_symptomatic, and tau are
# crossed with R0 and N.

test_grid <- tidyr::crossing(
  R0               = R0_values,
  N                = N_values,
  delay            = seq(0.5, 20, 0.05),
  prop_symptomatic = seq(1, 50, 1) / 500,
  gamma_post       = 1 / (seq(.5, 48, length.out = 80) / 24)
)

cat("# Number of symptomatic cows ------------------")

target_out <- here::here(
  "data-raw", "sensitivity-tau-sim_prop_sx_detected.rds"
)

if (!file.exists(target_out)) {
  sims_sx_n <- future_pmap(
    test_grid,
    function(R0, N, delay, prop_symptomatic, gamma_post) {
      run_intervention_ode(
        r0_in            = R0,
        prop_symptomatic = prop_symptomatic,
        delay_time       = delay,
        gamma_post       = gamma_post,
        s_ini            = N - 1,
        i_ini            = 1
      ) |>
        dplyr::filter(time == max(time)) |>
        dplyr::mutate(
          R0_baseline = R0,
          N = N,
          delay = delay,
          prop_symptomatic = prop_symptomatic,
          gamma_post = gamma_post,
          identification_quarantine = 1 / gamma_post * 24
        )
    },
    .options = furrr_options(seed = 1834L)
  ) |>
    bind_rows() |>
    mutate(
      avoided_infection =
        1 - (R / N) / finalsize_by_r0[as.character(R0_baseline)]
    )

  saveRDS(sims_sx_n, target_out)
}

rm(sims_sx_n)

# Milk production drop-off----
# prop_production_drop is also a proportion and needs no N-scaling.
# Delay, prop_production_drop, and tau are crossed with R0 and N.

test_grid_milk <- tidyr::crossing(
  R0                   = R0_values,
  N                    = N_values,
  delay                = seq(0.5, 20, 0.05),
  prop_production_drop = seq(0.001, 0.05, 0.001),
  gamma_post           = 1 / (seq(.5, 48, length.out = 80) / 24)
)

cat("# Milk production drop-off ------------------")

target_out <- here::here(
  "data-raw", "sensitivity-tau-sim_production_detected.rds"
)

if (!file.exists(target_out)) {
  sims_milk_pct <- future_pmap(
    test_grid_milk,
    function(R0, N, delay, prop_production_drop, gamma_post) {
      run_intervention_ode(
        r0_in                = R0,
        prop_production_drop = prop_production_drop,
        delay_time           = delay,
        gamma_post           = gamma_post,
        s_ini                = N - 1,
        i_ini                = 1
      ) |>
        dplyr::filter(time == max(time)) |>
        dplyr::mutate(
          R0_baseline = R0,
          N = N,
          delay = delay,
          prop_production_drop = prop_production_drop,
          gamma_post = gamma_post,
          identification_quarantine = 1 / gamma_post * 24
        )
    },
    .options = furrr_options(seed = 1834L)
  ) |>
    bind_rows() |>
    mutate(
      avoided_infection =
        1 - (R / N) / finalsize_by_r0[as.character(R0_baseline)]
    )

  saveRDS(sims_milk_pct, target_out)
}

rm(sims_milk_pct)

plan(sequential)

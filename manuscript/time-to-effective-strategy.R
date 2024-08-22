# Cluster version of analysis


if(Sys.info()[["sysname"]] != "Darwin") {
  .libPaths("/deac/bio/kortessisGrp/dewime23/libs")
}

# Install required libraries
library(tidyverse)
library(h5n1speed)
library(here)
library(future)
library(furrr)

remotes::install_github("medewitt/nccovid")

# Generic information
Sys.setenv(R_MAX_VSIZE="100Gb")
message(sprintf("Running R v%s", getRversion()))

# Settings
message("Setting up parallelism")

# Utilize parallelism to run over grid of parameter values
plan(multicore)

message("Running process")

r0_0 <- 1.2
r0_1 <- 1.52

test_grid <- tidyr::crossing(
    delay = seq(.5, 20, 0.05),
    n_detected = seq(1, 50, 1),
    gamma_post = 1 / (seq(.5, 48,  length.out = 80) / 24)
)

cat("# Number of infected cows ------------------")

target_out <- here::here("dev",
                         "time-to-effective-strategy-sim_n_detected-low.rds")

if(!file.exists(target_out)) {

  sims_n <- future_pmap(test_grid,
     function(delay, n_detected, gamma_post) {
      run_intervention_ode(n_detected = n_detected,
                           delay_time = delay, gamma_post = gamma_post) |>
                           filter(time == max(time)) |>
                          mutate(delay = delay, n_detected = n_detected,
                                 gamma_post = gamma_post,
                                 identifcation_quarantine = 1 / gamma_post * 24)
                      }) |>
    bind_rows() |>
    mutate(avoided_infection = 1 - (R / 500) / nccovid::epi_finalsize(r0_0))|>
mutate(r0 = r0_0)

saveRDS(sims_n, target_out)

}

rm(sims_n)


cat("# Number of symptomatic cows ------------------")

test_grid <- tidyr::crossing(
    delay = seq(0.5, 20, .05),
    prop_symptomatic = seq(1, 50, 1) / 500,
    gamma_post = 1/(seq(.5, 48,  length.out = 80) / 24)
)


target_out <- here::here("dev",
                         "time-to-effective-strategy-sim_prop_sx_detected-low.rds")


if(!file.exists(target_out)) {

sims_sx_n <- future_pmap(test_grid,
     function(delay, prop_symptomatic, gamma_post) {
        cat(prop_symptomatic)
              run_intervention_ode( prop_symptomatic = prop_symptomatic,
                                    delay_time = delay, gamma_post = gamma_post) |>
      mutate(delay = delay,
             prop_symptomatic = prop_symptomatic,
             gamma_post = gamma_post,
             identifcation_quarantine = 1/gamma_post*24) |>
             filter(time == max(time))
      }) |>
bind_rows() |>
mutate(avoided_infection = 1- (R/500) / nccovid::epi_finalsize(r0_0))|>
mutate(r0 = r0_0)

saveRDS(sims_sx_n, target_out)

}

rm(sims_sx_n)


# Milk production drop-off -----------------------------

test_grid_milk <- tidyr::crossing(
    delay = seq(0.5, 20, .05),
    prop_production_drop = seq(0.001, 0.05, .001),
    gamma_post = 1/(seq(.5, 48,  length.out = 80)/24)
)


target_out <- here::here("dev", "time-to-effective-strategy-sim_production_detected-low.rds")


if(!file.exists(target_out)) {

sims_milk_pct <- future_pmap(test_grid_milk,
     function(delay, prop_production_drop, gamma_post) {
              run_intervention_ode( prop_production_drop = prop_production_drop,
                                    delay_time = delay, gamma_post = gamma_post) |>
      mutate(delay = delay,
             prop_production_drop = prop_production_drop,
             gamma_post = gamma_post,
             identifcation_quarantine = 1/gamma_post*24) |>
             filter(time == max(time))
      }) |>
bind_rows() |>
mutate(avoided_infection = 1- (R/500) / nccovid::epi_finalsize(r0_0)) |>
mutate(r0 = r0_0)

saveRDS(sims_milk_pct, target_out)

}

plan(sequential)

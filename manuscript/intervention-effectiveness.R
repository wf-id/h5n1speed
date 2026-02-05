if(Sys.info()[["sysname"]] != "Darwin") {
  .libPaths("/deac/bio/kortessisGrp/dewime23/libs")
}

library(tidyverse)
library(h5n1speed)
library(furrr)

plan(multisession)
Sys.setenv(R_MAX_VSIZE="100Gb")

# Milk based approach
test_grid_milk <- tidyr::crossing(
    delay = seq(0,10,1),
    prop_production_drop = seq(0.01, 0.04, .01)
)

r0_0 <- 1.20
r0_1 <- 1.52
sims_milk_0 <- future_pmap(test_grid_milk,
     function(delay, prop_production_drop) {
      run_intervention_ode(prop_production_drop = prop_production_drop,
          delay_time = delay) |>
      mutate(delay = delay,
             prop_production_drop = prop_production_drop)
      }) |>
bind_rows() |>
filter(time == max(time)) |>
mutate(avoided_infection = 1 - (R/500) / nccovid::epi_finalsize(r0_0)) |>
mutate(r0 = r0_0)


saveRDS(sims_milk_0, here::here("dev", "intervention-effectiveness-milk-low.rds"))

rm(sims_milk_0)

sims_milk_1 <- future_pmap(test_grid_milk,
     function(delay, prop_production_drop) {
      run_intervention_ode(prop_production_drop = prop_production_drop,
          delay_time = delay) |>
      mutate(delay = delay,
             prop_production_drop = prop_production_drop)
      }) |>
bind_rows() |>
filter(time == max(time)) |>
mutate(avoided_infection = 1 - (R/500) / nccovid::epi_finalsize(r0_1))|>
mutate(r0 = r0_1)


saveRDS(sims_milk_1, here::here("dev", "intervention-effectiveness-milk-high.rds"))

rm(sims_milk_1)

# Number -------------------------------------
test_grid <- tidyr::crossing(
    delay = seq(1,10,1),
    n_detected = c(1,5,10, 25, 50)
)

sims_n <- future_pmap(test_grid,
     function(delay, n_detected){
      run_intervention_ode(n_detected = n_detected, delay_time = delay) |>
      mutate(delay = delay, n_detected = n_detected)
      }) |>
bind_rows() |>
filter(time == max(time)) |>
mutate(avoided_infection = 1- (R/500) / nccovid::epi_finalsize(r0_0)) |>
mutate(r0 = r0_0)

saveRDS(sims_n, here::here("dev", "intervention-effectiveness-n-infected-low.rds"))

rm(sims_n)

sims_n <- future_pmap(test_grid,
     function(delay, n_detected){
      run_intervention_ode(n_detected = n_detected, delay_time = delay) |>
      mutate(delay = delay, n_detected = n_detected)
      }) |>
bind_rows() |>
filter(time == max(time)) |>
mutate(avoided_infection = 1- (R/500) / nccovid::epi_finalsize(r0_1)) |>
mutate(r0 = r0_1)

saveRDS(sims_n, here::here("dev", "intervention-effectiveness-n-infected-high.rds"))

rm(sims_n)

# Number symptomatic

test_grid <- tidyr::crossing(
    delay = seq(1,10,1),
    prop_symptomatic = c(1,5,10, 25, 50)
)

sims_n_sx <- future_pmap(test_grid,
     function(delay, prop_symptomatic){
      run_intervention_ode(prop_symptomatic = prop_symptomatic/500, delay_time = delay) |>
      mutate(delay = delay, n_symptomatic = prop_symptomatic)
      }) |>
bind_rows() |>
filter(time == max(time)) |>
mutate(avoided_infection = 1- (R/500) / nccovid::epi_finalsize(r0_0)) |>
mutate(r0 = r0_0)

saveRDS(sims_n_sx, here::here("dev", "intervention-effectiveness-n-symptomatic-low.rds"))

rm(sims_n_sx)

sims_n_sx <- future_pmap(test_grid,
     function(delay, prop_symptomatic){
      run_intervention_ode(prop_symptomatic = prop_symptomatic/500, delay_time = delay) |>
      mutate(delay = delay, n_symptomatic = prop_symptomatic)
      }) |>
bind_rows() |>
filter(time == max(time)) |>
mutate(avoided_infection = 1- (R/500) / nccovid::epi_finalsize(r0_1)) |>
mutate(r0 = r0_1)

saveRDS(sims_n_sx, here::here("dev", "intervention-effectiveness-n-symptomatic-high.rds"))

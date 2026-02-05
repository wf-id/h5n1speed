if(!Sys.info()[["sysname"]]  %in% c("Darwin", "Windows")) {
  .libPaths("/deac/bio/kortessisGrp/dewime23/libs")
} else {
  library(devtools)
  clean_dll()
  load_all()
}

library(tidyverse)
library(h5n1speed)
library(furrr)
library(devtools)
plan(multisession)


test_grid <- tidyr::crossing(
    delay = seq(1,10,1),
    n_detected = c(1,5,10, 25, 50),
    p_asymptomatic = c(0),
    phi = c(0, 0.01, 0.05, 0.1)) |>
    mutate(use_r0 = make_asym_r0(p_asymptomatic, 1 - 1/1.2))

sims_n <- future_pmap(test_grid,
     function(use_r0, delay, n_detected, p_asymptomatic, phi){
      run_intervention_ode(r0 = use_r0, n_detected = n_detected, delay_time = delay, p_asymptomatic = p_asymptomatic, phi = phi) |>
      mutate(delay = delay, n_detected = n_detected, p_asymptomatic = p_asymptomatic, r0 = use_r0, phi = phi)
      }) |>
bind_rows() |>
filter(time == max(time))



out_fig_mortality <- sims_n |>
mutate(avoided_infections = (R + Ra + D)/total_infect_no_intervention) |>
select(R, Ra, D, avoided_infections, n_detected, phi, delay) |>
ggplot(aes(x = delay, y = avoided_infections)) +
geom_line(aes(lty = factor(phi), color = factor(phi))) +
facet_grid(cols = vars(n_detected),
labeller = labeller(n_detected = function(x) paste0("Detected ", x, " infections")))+
theme_bw() +
labs(x = "Delay (days)", fill = "Variable") +
theme(legend.position = "bottom") +
scale_fill_viridis_d(direction = -1) +
scale_y_continuous(expand = c(.01,0),
                   labels = scales::percent,
                   name = "Percent of Final Fraction Infected (without intervention)")+
scale_color_viridis_d(name = "Disease Induced Mortality", direction = -1)+
scale_linetype_discrete(name = "Disease Induced Mortality")



cowplot::ggsave2(out_fig_mortality, filename = here::here("manuscript", "figures", "p_out_fig_mortality.pdf"), width = 8, height = 10)

# Setup----
# Loads the simulator and parallel backend used by the existing dev scripts.

# library(devtools)
# load_all()
library(tidyverse)
library(furrr)
library(h5n1speed)

plan(multisession)
set.seed(1834)

# Test
z0 <- run_intervention_ode(
  r0_in          = 1.2,
  n_detected     = 1,
  delay_time     = 7,
  p_asymptomatic = 0,
  s_ini          = 500 - 1,
  i_ini          = 1
) |>
  dplyr::filter(time == max(time)) |>
  transmute(1 - total_infect_no_intervention / (S + I + A + R + Ra + D), total_infect_no_intervention)

z1 <- run_intervention_ode(
  r0_in          = 1.2,
  n_detected     = 10000 / 500,
  delay_time     = 7,
  p_asymptomatic = 0,
  s_ini          = 10000 - 1,
  i_ini          = 1
) |>
  dplyr::filter(time == max(time)) |>
  transmute(1 - total_infect_no_intervention / (S + I + A + R + Ra + D), total_infect_no_intervention)

# Sanity check that the number of infections is greater all things equal for a larger herd.
stopifnot(pull(z0, total_infect_no_intervention) < pull(z1, total_infect_no_intervention))

base_outbreak <- run_intervention_ode(
  r0_in          = 1.2,
  n_detected     = 1,
  delay_time     = 1e6,
  p_asymptomatic = 0,
  s_ini          = 500 - 1,
  i_ini          = 1
)

observed_i_max <- filter(base_outbreak, I == max(I)) |> pull(time)

fig_base_scenario <- base_outbreak |>
  select(time, I, R) |>
  pivot_longer(-time) |>
  ggplot(aes(time, value, color = name, linetype = name)) +
  geom_line(linewidth = 2) +
  theme_classic(base_size = 22) +
  labs(x = "Time (days)", y = "Number of infected animals", linetype = "Compartment") +
  geom_vline(xintercept = observed_i_max, linetype = "dotted", linewidth = 2) +
  scale_x_continuous(breaks = c(0, round(observed_i_max), 25, 50, 75, 100)) +
  scale_color_viridis_d(name = "Compartment", direction = -1)

cowplot::ggsave2(
  filename = here::here("manuscript", "figures", "figure-supplement-sensitivity-base-scenario.pdf"),
  plot = fig_base_scenario,
  width = 12,
  height = 8
)

# Sensitivity grid----
# Reviewer asked us to vary R0 over {1.2, 3, 5} and farm size N over
# {250, 500, 1000, 10000}; the baseline scenario is R0 = 1.2, N = 500.

R0_values <- c(1.2, 3, 5)
N_values <- c(250, 500, 1000, 10000)

# Heatmap sensitivity grid----
# Mirrors dev/asymptomatic-test.R, but adds R0 and N as crossing
# dimensions; p_asymptomatic is capped at 0.15 so that make_asym_r0
# stays finite at R0 = 5 (which requires p < 1 - 1/R0 = 0.2).

heatmap_grid <- tidyr::crossing(
  R0             = R0_values,
  N              = N_values,
  delay          = seq(0, 10, 0.5),
  p_asymptomatic = seq(0, 0.5, 0.01)
) |>
  dplyr::mutate(
    use_r0     = R0,
    n_detected = 10 * N / 500
  )

# Heatmap simulations----
# n_detected scales with N so that detection sensitivity is per-capita
# comparable across farm sizes (10 head on a 500-cow farm = 200 on
# 10,000); s_ini = N - 1 seeds one infection on a herd of size N.

sims_heatmap <- future_pmap(
  heatmap_grid,
  function(R0, N, delay, p_asymptomatic, use_r0, n_detected) {
    run_intervention_ode(
      r0_in          = use_r0,
      n_detected     = n_detected,
      delay_time     = delay,
      p_asymptomatic = p_asymptomatic,
      s_ini          = N - 1,
      i_ini          = 1
    ) |>
      dplyr::mutate(
        R0_baseline    = R0,
        N              = N,
        delay          = delay,
        p_asymptomatic = p_asymptomatic
      )
  },
  .options = furrr_options(seed = 1834L)
) |>
  dplyr::bind_rows() |>
  dplyr::filter(time == max(time))

saveRDS(
  sims_heatmap,
  here::here("data-raw", "sensitivity-heatmap.rds")
)

# Relative-risk computation----
# RRd and RRp are computed within each (R0, N) cell so each scenario is
# compared to its own delay = 0 / p_asymptomatic = 0 baseline.

rr_out <- sims_heatmap |>
  dplyr::mutate(total_infected = (R + I) / N) |>
  dplyr::group_by(R0_baseline, N, p_asymptomatic) |>
  dplyr::mutate(
    RRd = total_infected / total_infected[delay == 0]
  ) |>
  dplyr::group_by(R0_baseline, N, delay) |>
  dplyr::mutate(
    RRp = total_infected / total_infected[p_asymptomatic == 0]
  ) |>
  dplyr::ungroup()

rr_range <- range(rr_out$RRd, rr_out$RRp, na.rm = TRUE)
l_rr <- log(rr_range)
# Heatmap of delay-attributable relative risk----
# Faceted by R0 (rows) and N (columns) so the reader can see how the
# baseline panel (R0 = 1.2, N = 500) shifts as either knob changes.

p_rrd <- rr_out |>
  ggplot(aes(x = delay, y = p_asymptomatic, fill = log(RRd))) +
  geom_raster(interpolate = TRUE) +
  facet_grid(
    R0_baseline ~ N,
    labeller = label_bquote(
      rows = R[0] == .(R0_baseline),
      cols = N == .(N)
    )
  ) +
  scale_fill_viridis_c(
    name   = "RRd",
    limits = l_rr,
    labels = function(x) round(exp(x), 1)
  ) +
  theme_classic(base_size = 14) +
  scale_x_continuous(breaks = seq(0, 10, 2), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(
    x        = "Delay (days)",
    y        = "Proportion asymptomatic",
    subtitle = "Partial Relative Risk Due to Delay"
  )

cowplot::ggsave2(
  filename = here::here(
    "manuscript", "figures", "figure-supplement-sensitivity-rrd-heatmap.pdf"
  ),
  plot = p_rrd,
  width = 12,
  height = 8
)

# Heatmap of asymptomatic-attributable relative risk----
# Same faceting as RRd; together these two panels are the sensitivity
# counterpart to manuscript/figures/relative-risk-asymptomatic.png.

p_rrp <- rr_out |>
  ggplot(aes(x = delay, y = p_asymptomatic, fill = log(RRp))) +
  geom_raster(interpolate = TRUE) +
  facet_grid(
    R0_baseline ~ N,
    labeller = label_bquote(
      rows = R[0] == .(R0_baseline),
      cols = N == .(N)
    )
  ) +
  scale_fill_viridis_c(
    name   = "RRp",
    limits = l_rr,
    labels = function(x) round(exp(x), 1)
  ) +
  theme_classic(base_size = 14) +
  scale_x_continuous(breaks = seq(0, 10, 2), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(
    x        = "Delay (days)",
    y        = "Proportion asymptomatic",
    subtitle = "Partial Relative Risk Due to Asymptomatic Transmission"
  )

cowplot::ggsave2(
  filename = here::here(
    "manuscript", "figures", "figure-supplement-sensitivity-rrp-heatmap.pdf"
  ),
  plot = p_rrp,
  width = 12,
  height = 8
)

# n_detected sensitivity grid----
# Re-creates the detection-threshold sweep from
# manuscript/intervention-effectiveness.R, scaled to herd size so a
# given n_detected_per_500 represents the same per-capita threshold.

n_detected_grid <- tidyr::crossing(
  R0                 = R0_values,
  N                  = N_values,
  delay              = seq(1, 10, 1),
  n_detected_per_500 = c(1, 5, 10, 25, 50)
) |>
  dplyr::mutate(
    n_detected = n_detected_per_500 * N / 500
  )

# n_detected sensitivity simulations----
# p_asymptomatic is fixed at 0 here so the only moving parts are the
# detection threshold, the delay, R0, and N.

sims_n_detected <- future_pmap(
  n_detected_grid,
  function(R0, N, delay, n_detected_per_500, n_detected) {
    run_intervention_ode(
      r0_in = R0,
      n_detected = n_detected,
      delay_time = delay,
      s_ini = N - 1,
      i_ini = 1,
      sim_duration = 120
    ) |>
      dplyr::mutate(
        R0_baseline        = R0,
        N                  = N,
        delay              = delay,
        n_detected_per_500 = n_detected_per_500
      )
  },
  .options = furrr_options(seed = 1834L)
) |>
  dplyr::bind_rows() |>
  dplyr::filter(time == max(time))

saveRDS(
  sims_n_detected,
  here::here("data-raw", "sensitivity-n-detected.rds")
)

# Avoided-infection curves across (R0, N)----
# Proportion of infections avoided as a function of delay for each
# scaled detection threshold; one panel per (R0, N) cell.

sims_n_detected |>
  filter(N == 1000 & delay == 5 & n_detected_per_500 == 10) |>
  tail(3)

p_n_detected <- sims_n_detected |>
  dplyr::mutate(
    avoided      = pmax(0, total_infect_no_intervention - R - Ra),
    prop_avoided = avoided / total_infect_no_intervention
  ) |>
  ggplot(aes(
    x = delay,
    y = prop_avoided,
    color = as.factor(n_detected_per_500),
    linetype = as.factor(n_detected_per_500)
  )) +
  geom_line(linewidth = 1) +
  facet_grid(
    R0_baseline ~ N,
    labeller = label_bquote(
      rows = R[0] == .(R0_baseline),
      cols = N == .(N)
    )
  ) +
  scale_color_viridis_d(name = "Number of detected infections\n(per 500 head)") +
  scale_linetype_discrete(name = "Number of detected infections\n(per 500 head)") +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 14) +
  theme(legend.position = "top") +
  labs(
    x = "Delay (days)",
    y = "Proportion of avoided infections"
  )

cowplot::ggsave2(
  filename = here::here(
    "manuscript", "figures", "figure-supplement-sensitivity-n-detected-curves.pdf"
  ),
  plot = p_n_detected,
  width = 12,
  height = 8
)

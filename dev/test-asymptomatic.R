# This is the old style which tries to attribute different pieces of the infection
# to different sources of transmission.

if (!Sys.info()[["sysname"]] %in% c("Darwin", "Windows")) {
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
# Sys.setenv(R_MAX_VSIZE="100Gb")

# Milk based approach
test_grid_milk <- tidyr::crossing(
  delay = seq(1, 7, 1),
  prop_production_drop = seq(0.01, 0.03, .01),
  p_asymptomatic = c(0, 0.1, .3)
) |>
  mutate(use_r0 = make_asym_r0(p_asymptomatic, 1 - 1 / 1.2))

r0_0 <- 1.20

sims_milk_0 <- future_pmap(
  test_grid_milk,
  function(use_r0, delay, prop_production_drop, p_asymptomatic) {
    run_intervention_ode(
      r0 = use_r0, prop_production_drop = prop_production_drop,
      delay_time = delay, p_asymptomatic = p_asymptomatic
    ) |>
      mutate(
        delay = delay,
        prop_production_drop = prop_production_drop,
        p_asymptomatic = p_asymptomatic,
        r0 = use_r0
      )
  }
) |>
  bind_rows() |>
  filter(time == max(time))

summary(sims_milk_0)

appender_symptom <- function(x) {
  sprintf("Prop Asymptomatic: %s%%", as.numeric(x) * 100)
}

appender_milk_detection <- function(x) {
  sprintf("Prop Production Drop: %s%%", as.numeric(x) * 100)
}

# Milk simulations are not necessary -------
# p_milk <- sims_milk_0 |>
#   mutate(avoided_infections = total_infect_no_intervention - R - Ra) |>
#   select(R, Ra, avoided_infections, prop_production_drop, p_asymptomatic, delay) |>
#   gather(key = "variable", value = "value", -prop_production_drop, -p_asymptomatic, -delay) |>
#   group_by(prop_production_drop, p_asymptomatic, delay) |>
#   mutate(prop = value / sum(value)) |>
#   ungroup() |>
#   mutate(variable = factor(variable, levels = c("R", "Ra", "avoided_infections"), labels = c("Symptomatic infections", "Asymptomatic infections", "Avoided infections"))) |>
#   ggplot(aes(x = delay, y = prop, fill = as.factor(variable))) +
#   geom_col(position = "stack") +
#   facet_grid(
#     rows = vars(prop_production_drop),
#     cols = vars(p_asymptomatic),
#     labeller = labeller(
#       p_asymptomatic = appender_symptom,
#       prop_production_drop = appender_milk_detection
#     )
#   ) +
#   theme_bw() +
#   labs(x = "Delay (days)", y = "Proportion of infections", fill = "Variable") +
#   theme(legend.position = "bottom") +
#   scale_fill_viridis_d(direction = -1) +
#   scale_y_continuous(expand = c(.01, 0), labels = scales::percent, name = "Percent of Final Fraction Infected (without intervention)")

# cowplot::ggsave2(p_milk, filename = here::here("manuscript", "figures", "p_asymptomatic_milk.pdf"), width = 8, height = 10)


# Number -------------------------------------
test_grid <- tidyr::crossing(
  delay = seq(1, 10, 1),
  n_detected = c(1, 5, 10, 25, 50),
  p_asymptomatic = c(0, 0.1, .3)
) |>
  mutate(use_r0 = make_asym_r0(p_asymptomatic, 1 - 1 / 1.2))

sims_n <- future_pmap(
  test_grid,
  function(use_r0, delay, n_detected, p_asymptomatic) {
    run_intervention_ode(r0 = use_r0, n_detected = n_detected, delay_time = delay, p_asymptomatic = p_asymptomatic) |>
      mutate(delay = delay, n_detected = n_detected, p_asymptomatic = p_asymptomatic, r0 = use_r0)
  }
) |>
  bind_rows() |>
  filter(time == max(time))

# p_sensitive <- sims_n |>
#   mutate(avoided_infections = total_infect_no_intervention - R - Ra) |>
#   select(R, Ra, avoided_infections, n_detected, p_asymptomatic, delay) |>
#   gather(key = "variable", value = "value", -n_detected, -p_asymptomatic, -delay) |>
#   group_by(n_detected, p_asymptomatic, delay) |>
#   mutate(prop = value / sum(value)) |>
#   ungroup() |>
#   mutate(variable = factor(variable, levels = c("R", "Ra", "avoided_infections"), labels = c("Symptomatic infections", "Asymptomatic infections", "Avoided infections"))) |>
#   ggplot(aes(x = delay, y = prop, fill = as.factor(variable))) +
#   geom_col(position = "stack") +
#   facet_grid(
#     rows = vars(n_detected),
#     cols = vars(p_asymptomatic),
#     labeller = labeller(
#       p_asymptomatic = appender_symptom,
#       prop_production_drop = appender_milk_detection
#     )
#   ) +
#   theme_bw() +
#   labs(x = "Delay (days)", y = "Proportion of infections", fill = "Variable") +
#   theme(legend.position = "bottom") +
#   scale_fill_viridis_d(direction = -1) +
#   scale_y_continuous(expand = c(.01, 0), labels = scales::percent, name = "Percent of Final Fraction Infected (without intervention)")


# cowplot::ggsave2(p_sensitive, filename = here::here("manuscript", "figures", "p_asymptomatic_sensitive.pdf"), width = 8, height = 10)


# Number symptomatic -------------------------------------

test_grid <- tidyr::crossing(
  delay = c(0.001, seq(1, 10, 1)),
  prop_symptomatic = c(1, 5, 10, 25),
  p_asymptomatic = c(0, 0.2)
) |>
  mutate(use_r0 = make_asym_r0(p_asymptomatic, 1 - 1 / 1.2))

sims_n_sx <- pmap(
  test_grid,
  function(use_r0, delay, prop_symptomatic, p_asymptomatic) {
    run_intervention_ode(r0 = use_r0, prop_symptomatic = prop_symptomatic / 500, delay_time = delay, p_asymptomatic = p_asymptomatic) |>
      mutate(delay = delay, n_symptomatic = prop_symptomatic, p_asymptomatic = p_asymptomatic, r0 = use_r0)
  }
) |>
  bind_rows() |>
  filter(time == max(time))

d_sims_n_sx <- sims_n_sx |>
  filter(p_asymptomatic %in% c(0, 0.2)) |>
  mutate(avoided_infections = total_infect_no_intervention - R - Ra) |>
  mutate(
    prop_avoided = avoided_infections / total_infect_no_intervention,
    ffi = (I + A + R + Ra) / 500
  ) |>
  select(p_asymptomatic, delay, n_symptomatic, prop_avoided, ffi)

p_n_sx_difference <- d_sims_n_sx |>
  filter(n_symptomatic == 5) |>
  ggplot(aes(x = delay, y = prop_avoided, color = as.factor(p_asymptomatic))) +
  geom_line() +
  theme_classic(base_size = 16) +
  scale_y_continuous(limits = c(.5, NA), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Delay (days)", breaks = 1:10, limits = c(1, 10)) +
  scale_color_manual(
    values = RColorBrewer::brewer.pal(3, "Set1")[1:2],
    labels = function(x) paste0(as.numeric(x) * 100, "%")
  ) +
  theme(legend.position = "top") +
  labs(color = "Proportion Asymptomatic", x = "Delay (days)", y = "Proportion Avoided Infections")

saveRDS(p_n_sx_difference, file = here::here("manuscript", "figures", "p_n_sx_difference.rds"))

d_sims_n_sx_infections <- d_sims_n_sx |>
  filter(n_symptomatic == 5) |>
  mutate(prop_achieved = 1 - prop_avoided)

points_to_use <- d_sims_n_sx_infections |>
  filter(delay %in% c(0.001, 4)) |>
  pull(prop_achieved)

p_n_sx_difference_infections <- d_sims_n_sx_infections |>
  filter(n_symptomatic == 5) |>
  ggplot(aes(x = delay, y = prop_achieved, color = as.factor(p_asymptomatic)), linewidth = 1.5) +
  geom_line() +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Delay (days)", breaks = 0:10, limits = c(0, 10), expand = c(0, 0)) +
  scale_color_manual(
    values = RColorBrewer::brewer.pal(3, "Set1")[1:2],
    labels = function(x) paste0(as.numeric(x) * 100, "%")
  ) +
  theme(legend.position = "top") +
  labs(
    color = "Proportion Asymptomatic",
    x = "Delay (days)",
    y = "Proportion of Final Fraction of Infected (absent intervention)"
  ) +
  coord_cartesian() +
  geom_segment(aes(x = 0, xend = 4, y = points_to_use[4], yend = points_to_use[4]), lty = 2) +
  geom_segment(aes(x = 4, xend = 4, y = points_to_use[3], yend = points_to_use[4]), lty = 2) +
  geom_point(x = 4, y = points_to_use[4], pch = 4, color = "orange", size = 8) +
  annotate(geom = "text", x = 4.04, y = mean(points_to_use[c(1, 3)]), label = "Relative Risk of Delay", hjust = 0, size = 8) +
  annotate(geom = "text", x = 4.04, y = mean(points_to_use[c(3, 4)]), label = "Relative Risk of\nAsymptomatic Transmission", hjust = 0, size = 8)

p_n_sx_difference_infections



saveRDS(p_n_sx_difference_infections, file = here::here("manuscript", "figures", "p_n_sx_difference_infections.rds"))

# ! Final fraction infected y-axis ------------------------------------------

points_to_use <- d_sims_n_sx_infections |>
  filter(delay %in% c(0.001, 4)) |>
  pull(ffi)

p_n_sx_difference_infections_final <- d_sims_n_sx_infections |>
  filter(n_symptomatic == 5) |>
  ggplot(aes(x = delay, y = ffi, color = as.factor(p_asymptomatic)), linewidth = 1.5) +
  geom_line() +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Delay (days)", breaks = 0:10, limits = c(0, 10), expand = c(0, 0)) +
  scale_color_manual(
    values = RColorBrewer::brewer.pal(3, "Set1")[1:2],
    labels = function(x) paste0(as.numeric(x) * 100, "%")
  ) +
  theme(legend.position = "top") +
  labs(
    color = "Proportion Asymptomatic",
    x = "Delay (days)",
    y = "Final Fraction of Infected"
  ) +
  coord_cartesian() +
  geom_segment(aes(x = 0, xend = 4, y = points_to_use[4], yend = points_to_use[4]), lty = 2) +
  geom_segment(aes(x = 4, xend = 4, y = points_to_use[3], yend = points_to_use[4]), lty = 2) +
  geom_point(x = 4, y = points_to_use[4], pch = 4, color = "orange", size = 8) +
  annotate(geom = "text", x = 4.04, y = mean(points_to_use[c(1, 3)]), label = "Relative Risk of Delay", hjust = 0, size = 8) +
  annotate(geom = "text", x = 4.04, y = mean(points_to_use[c(3, 4)]), label = "Relative Risk of\nAsymptomatic Transmission", hjust = 0, size = 8)

p_n_sx_difference_infections_final

cowplot::ggsave2(p_n_sx_difference_infections_final, filename = here::here("manuscript", "figures", "p_n_sx_difference_infections_final.pdf"), width = 8, height = 10)

saveRDS(p_n_sx_difference_infections_final, file = here::here("manuscript", "figures", "p_n_sx_difference_infections_final.rds"))

# p_n_sx_or <- d_sims_n_sx |>
#   pivot_wider(
#     names_from = p_asymptomatic,
#     values_from = prop_avoided,
#     names_prefix = "prop_avoided_"
#   ) |>
#   mutate(rr = (1 - prop_avoided_0) / (1 - prop_avoided_0)) |>
#   mutate(or = (1 - prop_avoided_0.2) / prop_avoided_0.2 /
#     (1 - prop_avoided_0) / prop_avoided_0) |>
#   ggplot(aes(x = delay, y = or, color = as.factor(n_symptomatic))) +
#   geom_line() +
#   theme_classic(base_size = 14) +
#   scale_y_continuous(limits = c(1, NA), breaks = seq(1, 10, 0.5)) +
#   scale_color_viridis_d() +
#   theme(legend.position = "top") +
#   labs(color = "Detection Threshold", x = "Delay (days)", y = "Odds ratio")

# cowplot::plot_grid(p_n_sx_difference, p_n_sx_or, nrow = 2)

# cowplot::ggsave2(cowplot::plot_grid(p_n_sx_difference, p_n_sx_or, nrow = 2), filename = here::here("manuscript", "figures", "p_symptomatic_syndromic_or.pdf"), width = 8, height = 10)
# ggplot(aes(x = delay, y = prop_avoided, color = as.factor(p_asymptomatic))) +
#   geom_line()

# sims_n_sx |>
#   filter(delay == 50)

# appender_symptomatic <- function(x) {
#   sprintf("Symptomatic threshold: %s", x)
# }

# p_symptomatic <- sims_n_sx |>
#   filter(n_symptomatic != 50) |>
#   mutate(avoided_infections = total_infect_no_intervention - R - Ra) |>
#   select(R, Ra, avoided_infections, n_symptomatic, p_asymptomatic, delay) |>
#   gather(key = "variable", value = "value", -n_symptomatic, -p_asymptomatic, -delay) |>
#   group_by(n_symptomatic, p_asymptomatic, delay) |>
#   mutate(prop = value / sum(value)) |>
#   ungroup() |>
#   mutate(variable = factor(variable, levels = c("R", "Ra", "avoided_infections"), labels = c("Symptomatic infections", "Asymptomatic infections", "Avoided infections"))) |>
#   ggplot(aes(x = delay, y = prop, fill = as.factor(variable))) +
#   geom_col(position = "stack") +
#   facet_grid(
#     rows = vars(n_symptomatic),
#     cols = vars(p_asymptomatic),
#     labeller = labeller(
#       p_asymptomatic = appender_symptom,
#       n_symptomatic = appender_symptomatic
#     )
#   ) +
#   theme_bw() +
#   labs(x = "Delay (days)", y = "Proportion of infections", fill = "Variable") +
#   theme(legend.position = "bottom") +
#   scale_fill_viridis_d(direction = -1) +
#   scale_y_continuous(labels = scales::percent) +
#   labs(fill = NULL) +
#   scale_y_continuous(expand = c(.01, 0), labels = scales::percent, name = "Percent of Final Fraction Infected (without intervention)")

# cowplot::ggsave2(p_symptomatic, filename = here::here("manuscript", "figures", "p_symptomatic_syndromic.pdf"), width = 8, height = 10)

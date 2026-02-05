library(future)
library(furrr)
library(h5n1speed)
library(tidyverse)
library(cowplot)
test_grid <- tidyr::crossing(
  delay = seq(0, 10, 0.5),
  n_detected = c(10),
  p_asymptomatic = seq(0, .5, .01)
) |>
  mutate(use_r0 = make_asym_r0(p_asymptomatic, 1 - 1 / 1.2))

run <- FALSE

if (run) {
  plan(multisession, workers = 8)

  sims_n <- future_pmap(
    test_grid,
    function(use_r0, delay, n_detected, p_asymptomatic) {
      run_intervention_ode(r0 = use_r0, n_detected = n_detected, delay_time = delay, p_asymptomatic = p_asymptomatic) |>
        mutate(delay = delay, n_detected = n_detected, p_asymptomatic = p_asymptomatic, r0 = use_r0)
    }
  ) |>
    bind_rows() |>
    filter(time == max(time))


  saveRDS(sims_n, here::here("data-raw", "sims_n-asymptomatic.rds"))
} else {
  sims_n <- readRDS(here::here("data-raw", "sims_n-asymptomatic.rds"))
}

# Extra infectiond to asymptomatic------------------------------------------

sims_n |>
  mutate(infected = R + I) |>
  select(delay, p_asymptomatic, infected) |>
  # group_by(delay) |>
  mutate(infection_from_asymptomatic = infected - rep(infected[p_asymptomatic == 0 & delay == 0], n())) |>
  # ungroup() |>
  # group_by(p_asymptomatic) |>
  mutate(infection_from_delay = infected - rep(infected[delay == 0 & p_asymptomatic == 0], n())) |>
  ungroup() |>
  filter(delay %in% c(0, 5) & p_asymptomatic %in% c(0, 0.5))


# Relative risk analysis ------------------------------------------
out <- sims_n |>
  mutate(total_infected = (R + I) / 500) |>
  select(delay, total_infected, p_asymptomatic) |>
  # Get denominator for RRd (total infected at delay=0 for each p_asymptomatic)
  group_by(p_asymptomatic) |>
  mutate(RRd = total_infected / total_infected[delay == 0], infected_delay_0 = total_infected[delay == 0]) |>
  ungroup() |>
  # Get denominator for RRp (total infected at p_asymptomatic=0 for each delay)
  group_by(delay) |>
  mutate(RRp = total_infected / total_infected[p_asymptomatic == 0]) |>
  ungroup()

library(patchwork)
max(out$RRp, out$RRd)
log(8)

# Relative Risk due to proportion asymptomatic------------------------------------------
p_effect_symptomatic <- out |>
  ggplot(aes(x = delay, y = p_asymptomatic, fill = log(RRp))) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(
    name = "RRp",
    limits = c(0, 2.1),
    oob = scales::squish,
    labels = c(1, 2, 4, 8),
    breaks = log(c(1, 2, 4, 8))
  ) +
  theme_classic(base_size = 20) +
  theme(legend.position = "top") +
  labs(subtitle = "Partial Relative Risk Due to Asymptomatic") +
  scale_x_continuous(
    name = NULL, breaks = seq(0, 10, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = NULL,
    labels = scales::percent,
    breaks = seq(0, .5, .05), expand = c(0, 0)
  ) +
  geom_point(x = 4, y = .20, pch = 4, color = "orange", size = 8)

p_total_infected <- sims_n |>
  mutate(total_infected = 1 - S / 500) |>
  ggplot(aes(x = delay, y = p_asymptomatic, fill = total_infected)) +
  geom_tile() +
  scale_fill_viridis_c(
    name = "Total infected",
    limits = c(0, .6),
    # oob = scales::squish,
    labels = scales::percent(seq(0, .6, 0.1)),
    breaks = seq(0, .6, 0.1)
  ) +
  theme(legend.position = "top") +
  theme_classic(base_size = 20) +
  labs(subtitle = "Total infected") +
  scale_x_continuous(
    name = "Delay (days)", breaks = seq(0, 10, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Proportion asymptomatic",
    labels = scales::percent,
    breaks = seq(0, .5, .05), expand = c(0, 0)
  )

ggdraw(get_legend(p_total_infected))



# ------------------------------------------
# Relative Risk due to delay------------------------------------------
p_effect_delay <- out |>
  ggplot(aes(x = delay, y = p_asymptomatic, fill = log(RRd))) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(
    name = "Relative\nRisk",
    limits = c(0, 2.1),
    # oob = scales::squish,
    labels = c(1, 2, 4, 8),
    breaks = log(c(1, 2, 4, 8))
  ) +
  theme_classic(base_size = 20) +
  # theme(legend.position = "top")+
  labs(subtitle = "Partial Relative Risk Due to Delay") +
  scale_x_continuous(
    name = NULL, breaks = seq(0, 10, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Proportion asymptomatic",
    labels = scales::percent,
    breaks = seq(0, .5, .05), expand = c(0, 0)
  ) +
  geom_point(x = 4, y = .20, pch = 4, color = "orange", size = 8) +
  geom_point(x = 4, y = .2, pch = 4, color = "orange", size = 8)

# p_total_infected / {p_effect_symptomatic + p_effect_delay + plot_layout(guides = "collect")} + plot_annotation(tag_levels = "A")

# str(get_legend(p_effect_symptomatic))
# ggdraw(get_legend(p_effect_delay))

panel_rrs <- plot_grid(
  p_effect_delay +
    theme(legend.position = "none"),
  p_effect_symptomatic +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL),
  get_legend(p_effect_delay),
  labels = c("B", "C", ""),
  ncol = 3, rel_widths = c(1.05, 1, 0.2)
)

xaxis_upper <- ggdraw() +
  draw_label(
    "Delay (days)",
    fontface = "bold",
    x = .5,
    hjust = 1
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

panel_rrs <- plot_grid(panel_rrs, xaxis_upper, rel_heights = c(1, .02), ncol = 1)

# ------------------------------------------
# Resultant Relative Risk Contribution------------------------------------------
p_resultant <- out |>
  mutate(RR_vector = sqrt((RRd - 1)^2 + (RRp - 1)^2)) |>
  ggplot(aes(x = delay, y = p_asymptomatic, fill = RR_vector)) +
  geom_tile() +
  scale_fill_viridis_c() +
  geom_contour(aes(z = RR_vector), color = "white") +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "top") +
  labs(subtitle = "Resultant Relative Risk Contribution") +
  scale_x_continuous(
    name = "Delay (days)", breaks = seq(0, 10, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Proportion asymptomatic",
    labels = scales::percent,
    breaks = seq(0, .5, .05), expand = c(0, 0)
  )




# Create a vector field plot showing the relative effects
p_vector <- out |>
  filter(delay == 5) |>
  ggplot(aes(x = delay, y = p_asymptomatic)) +
  # Create vector field arrows showing relative effects:
  # - RRd-1 represents the relative effect of delay (subtract 1 to center at 0)
  # - RRp-1 represents the relative effect of proportion asymptomatic
  # Divide by 5 to scale down the arrow lengths to fit nicely in the plot
  # Alpha (transparency) is based on the magnitude of the combined effects
  geom_segment(
    aes(
      xend = delay + (RRd - 1) / 5,
      yend = p_asymptomatic + (RRp - 1) / 5,
      alpha = sqrt((RRd - 1)^2 + (RRp - 1)^2)
    ),
    arrow = arrow(length = unit(0.1, "cm"))
  ) +
  scale_alpha_continuous(range = c(0.1, 0.8)) +
  labs(
    x = "Delay (days)",
    y = "Proportion asymptomatic",
    alpha = "Effect magnitude"
  ) +
  theme_minimal()

p_vector

# Display all three plots together
p_effect_symptomatic + p_effect_delay + p_vector +
  plot_layout(guides = "collect", ncol = 3)

fig_general <- readRDS(here::here("manuscript", "figures", "p_n_sx_difference_infections.rds"))

# cowplot::ggsave2("relative-risk-asymptomatic.pdf",
#   cowplot::plot_grid(fig_general, panel_rrs, nrow = 2, labels = "AUTO"),
#   width = 12, height = 16
# )


fig_general <- readRDS(here::here("manuscript", "figures", "p_n_sx_difference_infections_final.rds"))

cowplot::ggsave2(here::here("manuscript", "figures", "relative-risk-asymptomatic-final-preview.pdf"),
  cowplot::plot_grid(fig_general, panel_rrs, nrow = 2, labels = "AUTO"),
  width = 12, height = 16
)

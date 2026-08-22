library(data.table)
library(tidyverse)
library(patchwork)
library(here)

# Read in data from cluster

input_files <- fs::dir_ls(here("data-raw"), regexp = "sensitivity-tau-.*hpc")

sims_n_effective <- setDT(readRDS(
  grep("sim_n_detected", input_files, value = TRUE)
))
sims_sx_effective <- setDT(readRDS(
  grep("sim_prop_sx_detected", input_files, value = TRUE)
))
sims_milk_effective <- setDT(readRDS(
  grep("sim_production_detected", input_files, value = TRUE)
))

# Contour figures
# Same 1-day breaks and viridis fill as the original figure.

mybreaks <- seq(1, 11, 1)
use_length <- length(mybreaks)

mycolors <- function() viridis::viridis_pal()(use_length)

breaklabel <- function() {
  paste0(mybreaks[1:(use_length - 1)], "-", mybreaks[2:use_length])
}

# Facet labels----

facet_labels <- labeller(
  R0_baseline = as_labeller(
    function(x) paste0("R[0] == ", x),
    default = label_parsed
  ),
  N = as_labeller(
    function(x) paste0("N == ", x),
    default = label_parsed
  )
)

# Effective-delay surface----
# For each (R0, N, x, tau) cell, the largest delay that still avoids
# >= 80% of infections
# Cap tau at 20 hours to match the original y range

effective_surface <- function(dt, xvar) {
  dt[
    avoided_infection >= 0.8 & identification_quarantine <= 20,
    .(delay = max(delay)),
    by = c("R0_baseline", "N", xvar, "identification_quarantine")
  ]
}

# Faceted contour functiosn for ggplot

facet_time_to_effective <- function(
  dt,
  xvar,
  xlab,
  xlimits = NULL,
  xbreaks = waiver()
) {
  surface <- effective_surface(dt, xvar)

  ggplot(
    surface,
    aes(.data[[xvar]], identification_quarantine, z = delay)
  ) +
    geom_contour_filled(breaks = mybreaks, show.legend = TRUE) +
    scale_fill_manual(
      values = mycolors(),
      labels = breaklabel(),
      name = expression(tau^"*"),
      drop = FALSE
    ) +
    facet_grid(
      R0_baseline ~ N,
      labeller = facet_labels
    ) +
    scale_y_continuous(
      name = "Time from infection to isolation (hrs)",
      limits = c(4, 24),
      breaks = seq(4, 24, 4),
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      name = xlab,
      limits = xlimits,
      breaks = xbreaks,
      expand = c(0, 0)
    ) +
    theme_classic(base_size = 16) +
    theme(
      panel.background = element_rect(fill = "grey80"),
      panel.spacing = unit(0.6, "lines"),
      legend.position = "bottom",
      legend.title = element_text(size = rel(1.5)),
      axis.title.y = element_text(size = rel(1.1)),
      legend.text = element_text(size = rel(1.1))
    ) +
    guides(fill = guide_legend(nrow = 1))
}

# Facet grid panels
# D: number of **infected** at detection (per 500 head)
# E: number of **symptomatic** at detection (per 500 head)
# F: milk **production** drop

p_d <- facet_time_to_effective(
  sims_n_effective,
  xvar = "n_detected_per_500",
  xlab = "Number of infected at detection (per 500 head)",
  xlimits = c(1, NA),
  xbreaks = c(1, seq(10, 50, 10))
)

p_d

sims_sx_effective[, n_symptomatic_per_500 := prop_symptomatic * 500]

p_e <- facet_time_to_effective(
  sims_sx_effective,
  xvar = "n_symptomatic_per_500",
  xlab = "Number of symptomatic at detection (per 500 head)",
  xlimits = c(1, 25)
)

p_f <- facet_time_to_effective(
  sims_milk_effective,
  xvar = "prop_production_drop",
  xlab = "Milk production drop at detection",
  xlimits = c(0.001, 0.04)
)

# Each panel is a 3 x 4 facet grid, so save individually

fig_dir <- here("manuscript", "figures")

cowplot::ggsave2(
  p_d,
  filename = file.path(fig_dir, "supplement-figure-3-D-facet-r0-farmsize.pdf"),
  width = 9,
  height = 7
)

cowplot::ggsave2(
  p_e,
  filename = file.path(fig_dir, "supplement-figure-3-E-facet-r0-farmsize.pdf"),
  width = 9,
  height = 7
)

cowplot::ggsave2(
  p_f,
  filename = file.path(fig_dir, "supplement-figure-3-F-facet-r0-farmsize.pdf"),
  width = 9,
  height = 7
)

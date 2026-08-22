library(tidyverse)
library(h5n1speed)

set.seed(1834)

# Estimated values ----
gamma.hat <- 1 / 1.15
beta.hat <- 1.2 * gamma.hat
r.hat <- beta.hat - gamma.hat

# Maximum feasible gamma (gamma < gamma_max => duration > 10 days)
# Baker et al among others

gamma_max <- 1 / 10

# Reproduction numbers ---
# R0.hat is directly estimated (beta.hat / gamma.hat = 1.2).
# R0.predict is implied by the growth rate at the fastest feasible
# recovery (gamma_max).

R0.hat <- beta.hat / gamma.hat
R0.predict <- 1 + r.hat / gamma_max

# Vertices of the triangle bounded by the R0.hat line, the blue line,
# and gamma_max. The R0.predict line meets the other two at v2.

v1 <- c(gamma_max * R0.hat, gamma_max) # R0.hat line meets gamma_max
v2 <- c(gamma_max + r.hat, gamma_max) # blue line meets gamma_max
v3 <- c(beta.hat, gamma.hat) # blue line meets R0.hat line (estimate)

sample_slower <- function(n = 1, seed = 1834) {
  set.seed(seed)
  u <- runif(n)
  v <- runif(n)
  flip <- (u + v) > 1
  u[flip] <- 1 - u[flip]
  v[flip] <- 1 - v[flip]
  x <- v1[1] + u * (v2[1] - v1[1]) + v * (v3[1] - v1[1])
  y <- v1[2] + u * (v2[2] - v1[2]) + v * (v3[2] - v1[2])
  cbind(beta = x, gamma = y)
}

# Sample a feasible point for a fixed R0 = beta / gamma. The line
# beta = r0 * gamma intersects the triangle in a segment (or a point)
# points are drawn uniformly along that segment
# Sample over range of bets

sample_r0 <- function(r0, n = 1, seed = 1834) {
  set.seed(seed)
  V <- rbind(v1, v2, v3)
  # f = 0 on the R0 line - sign flips across it
  f <- V[, 1] - r0 * V[, 2]
  edges <- rbind(c(1, 2), c(2, 3), c(3, 1))
  hits <- matrix(numeric(0), ncol = 2)
  for (k in seq_len(nrow(edges))) {
    a <- edges[k, 1]
    b <- edges[k, 2]
    fa <- f[a]
    fb <- f[b]
    if (fa == 0) {
      hits <- rbind(hits, V[a, ])
    }
    if (fb == 0) {
      hits <- rbind(hits, V[b, ])
    }
    if (fa * fb < 0) {
      t <- fa / (fa - fb)
      hits <- rbind(hits, V[a, ] + t * (V[b, ] - V[a, ]))
    }
  }
  hits <- unique(round(hits, 12))
  if (nrow(hits) == 0) {
    r0_range <- range(V[, 1] / V[, 2])
    stop(sprintf(
      "R0 = %.3f is outside the feasible range [%.3f, %.3f].",
      r0,
      r0_range[1],
      r0_range[2]
    ))
  }
  p1 <- hits[1, ]
  p2 <- if (nrow(hits) >= 2) hits[2, ] else hits[1, ]
  s <- runif(n)
  x <- p1[1] + s * (p2[1] - p1[1])
  y <- p1[2] + s * (p2[2] - p1[2])
  cbind(beta = x, gamma = y)
}

pt <- sample_slower(1)

pt_ideal <- sample_r0(2.0, 1)

# Baseline sc
z0 <- run_intervention_ode(
  r0_in = 1.2,
  n_detected = 1,
  delay_time = 1e6,
  p_asymptomatic = 0,
  s_ini = 500 - 1,
  i_ini = 1
)

# Slower scenarion
z1 <- run_intervention_ode(
  r0_in = unname(pt_ideal[1, "beta"] / pt_ideal[1, "gamma"]),
  gamma_in = unname(pt_ideal[1, "gamma"]),
  n_detected = 1,
  delay_time = 1e6,
  p_asymptomatic = 0,
  s_ini = 500 - 1,
  i_ini = 1
)

# Fixed gamma
z2 <- run_intervention_ode(
  r0_in = 2.0,
  n_detected = 1,
  gamma_in = gamma.hat,
  delay_time = 1e6,
  p_asymptomatic = 0,
  s_ini = 500 - 1,
  i_ini = 1
)

# Fixed beta
z3 <- run_intervention_ode(
  r0_in = 2.0,
  n_detected = 1,
  gamma_in = beta.hat / 2.0,
  delay_time = 1e6,
  p_asymptomatic = 0,
  s_ini = 500 - 1,
  i_ini = 1
)
z3

# Find max infx time in scenario
find_max_infection <- function(z) {
  z |>
    filter(I == max(I)) |>
    dplyr::select(time, use_I = I, use_R = R)
}

# Bring scenarios together ---
max_infection_times <- map_df(list(z0, z1, z2, z3), find_max_infection)

max_infection_times$scenario <- c(
  "Base scenario",
  "Slower",
  "Fixed gamma",
  "Fixed beta"
)

combinded_curves <- z1 |>
  select(time, R) |>
  mutate(group = "Slower") |>
  bind_rows(
    z2 |>
      select(time, R) |>
      mutate(group = "Fixed gamma")
  ) |>
  bind_rows(
    z3 |>
      select(time, R) |>
      mutate(group = "Fixed beta")
  ) |>
  bind_rows(
    z0 |>
      select(time, R) |>
      mutate(group = "Base scenario")
  )

combined_curves <- combinded_curves |>
  mutate(
    group = factor(
      group,
      levels = c("Base scenario", "Slower", "Fixed gamma", "Fixed beta")
    )
  )

#cb friendly and match to pts in conceptual
use_cols <- c(
  "Base scenario" = "#2f6f9f",
  "Slower" = "#c1531f",
  "Fixed gamma" = "#3f8f5b",
  "Fixed beta" = "#8a5cb0"
)

p_0 <- combined_curves |>
  filter(time < 60) |>
  ggplot(aes(x = time, y = R, color = group)) +
  geom_line(linewidth = 2) +
  geom_segment(
    data = max_infection_times,
    inherit.aes = FALSE,
    aes(x = time, xend = time, y = 0, yend = use_R, color = scenario),
    linewidth = 1,
    linetype = "dotted",
    alpha = 1
  ) +
  scale_color_manual(values = use_cols) +
  theme_classic(base_size = 14) +
  labs(color = NULL, linetype = NULL) +
  labs(x = "Time (days)", y = "Cumulative infections") +
  scale_y_continuous(
    limits = c(0, 400),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )

p_0

cowplot::ggsave2(
  "dev/conceptual-sensitivity-analysis-curves.pdf",
  p_0,
  width = 6,
  height = 4,
  bg = "white"
)

library(tidyverse)
library(cowplot)
library(deSolve)
library(h5n1speed)

# Population parameters
times <- seq(0, 130, 1)
R0 <- 3.0
y <- c(S = 999, I = 1, R = 0)

# Dynamics
sir_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}
r0 <- .5
# Fast
gamma_0 <- 1 / r_to_gamma(r = r0, r_0 = R0)
beta_0 <- R0 * gamma_0
stopifnot(beta_0 / gamma_0 - R0 < 1e-6)

# Slow
gamma_1 <- 1 / r_to_gamma(r = r0 / 4.0, r_0 = R0)
beta_1 <- R0 * gamma_1
stopifnot(beta_1 / gamma_1 - R0 < 1e-6)


run_conceptual_ode <- function(params, states = y, span = times, mod = sir_model) {
  o <- data.frame(
    ode(y = states, times = span, func = mod, parms = params)
  ) |>
    mutate(cumulative = I + R) |>
    tibble::as_tibble()
  return(o)
}

output <- map(
  list(
    c(beta = beta_0, gamma = gamma_0),
    c(beta = beta_1, gamma = gamma_1)
  ),
  ~ run_conceptual_ode(params = .x)
) |>
  setNames(c("Higher r", "Low r")) |>
  bind_rows(.id = "Scenario")

z <- output |>
  filter(Scenario == "Low r")

z <- output |>
  filter(Scenario != "Low r")

output |>
  group_by(Scenario) |>
  filter(I == max(I)) |>
  mutate(cumulative = cumulative / 1000)

with(z, data.frame(z$time, z$cumulative / 1000)) |>
  write.table(file = here::here("data-raw", "low-r.txt"), row.names = FALSE, col.names = FALSE)

with(z, data.frame(z1$time, z1$cumulative / 1000)) |>
  write.table(file = here::here("data-raw", "high-r.txt"), row.names = FALSE, col.names = FALSE)



output |>
  ggplot((aes(time, cumulative, group = Scenario))) +
  geom_line(linewidth = 1.25) +
  theme_cowplot(font_size = 16)

sir_model_intervention <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    gamma_use <- ifelse(t >= threshold, gamma_post, gamma_pre)
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma_use * I
    dR <- gamma_use * I
    list(c(dS, dI, dR))
  })
}

o_1_no_intervention <- data.frame(ode(
  y = c(S = 999, I = 1, R = 0),
  times = seq(0, 130, 1),
  func = sir_model_intervention, parms = c(beta = beta_0, gamma_pre = gamma_0, gamma_post = gamma_0, threshold = Inf)
)) |>
  mutate(cumulative = I + R) |>
  mutate(cumulative_pct = cumulative / 1000) |>
  filter(cumulative_pct >= .3)

o_1_intervention <- data.frame(ode(
  y = c(S = 999, I = 1, R = 0),
  times = seq(0, 130, 1),
  func = sir_model_intervention, parms = c(beta = beta_0, gamma_pre = gamma_0, gamma_post = 1, threshold = 12 + 7)
)) |>
  mutate(cumulative = I + R) |>
  mutate(cumulative_pct = cumulative / 1000) |>
  filter(cumulative_pct >= .3)

head(o)


plot(o_1_no_intervention$time, o_1_no_intervention$cumulative_pct)
lines(o_1_intervention$time, o_1_intervention$cumulative_pct, add = TRUE, col = "red")

with(o_1_intervention, data.frame(o_1_intervention$time, o_1_intervention$cumulative_pct)) |>
  write.table(file = here::here("data-raw", "high-r-intervention.txt"), row.names = FALSE, col.names = FALSE)


o_2_no_intervention <- data.frame(ode(
  y = c(S = 999, I = 1, R = 0),
  times = seq(0, 130, 1),
  func = sir_model_intervention, parms = c(beta = beta_1, gamma_pre = gamma_1, gamma_post = 1, threshold = Inf)
)) |>
  mutate(cumulative = I + R) |>
  mutate(cumulative_pct = cumulative / 1000) |>
  filter(cumulative_pct >= .3) |>
  head()


o_2_intervention <- data.frame(ode(
  y = c(S = 999, I = 1, R = 0),
  times = seq(0, 130, 1),
  func = sir_model_intervention, parms = c(beta = beta_1, gamma_pre = gamma_1, gamma_post = 1, threshold = 47 + 7)
)) |>
  mutate(cumulative = I + R) |>
  mutate(cumulative_pct = cumulative / 1000) |>
  filter(cumulative_pct >= .3)

with(o_2_intervention, data.frame(o_2_intervention$time, o_2_intervention$cumulative_pct)) |>
  write.table(file = here::here("data-raw", "low-r-intervention.txt"), row.names = FALSE, col.names = FALSE)

scales::show_col((scales::viridis_pal()(10)))

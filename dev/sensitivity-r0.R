# Purpose: Generates supplemental figure 3 showing the
# relationship between farms of two different sizes with
# frequency dependent transmission

library(tidyverse)
library(devtools)

load_all()

set.seed(1834)

# Set some general parameters for subsequent sims

REF_SIZE <- 500
REF_INI <- 1
REF_INI_PCT <- REF_INI / REF_SIZE
REF_DETECT_PCT <- 50 / REF_SIZE

# Base

z0 <- run_intervention_ode(
    r0_in = 1.2,
    n_detected = REF_DETECT_PCT * 500,
    delay_time = 7,
    p_asymptomatic = 0,
    s_ini = 500 - 1,
    i_ini = 1,
    mode = "frequency"
)
z0

# Larger
z1 <- run_intervention_ode(
    r0_in = 1.2,
    n_detected = REF_DETECT_PCT * 10000,
    delay_time = 7,
    p_asymptomatic = 0,
    s_ini = ceiling(10000 - 10000 * REF_INI_PCT),
    i_ini = 10000 * REF_INI_PCT,
    mode = "frequency"
)
z1
combined <- bind_rows(
    z0 |>
        mutate(herd = 500),
    z1 |>
        mutate(herd = 10000)
)

# color-deficiency safe pallete choices:

ok <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#000000"
)

fig <- combined |>
    ggplot(aes(time, R / herd, color = factor(herd), linetype = factor(herd))) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = ok[1:2]) +
    labs(
        color = "Herd size",
        linetype = "Herd size",
        y = "Total cumulative infected (proportion of herd)",
        x = "Days"
    ) +
    theme_classic(base_size = 22) +
    theme(legend.position = "top")

cowplot::ggsave2(
    filename = here::here(
        "manuscript",
        "figures",
        "figure-supplement-sensitivity-herd-frequency.pdf"
    ),
    plot = fig,
    width = 12,
    height = 8
)

# Demonstration of sampling approach

library(tidyverse)

N_herd <- 500

# Utilizing hypergeomtric due to small, closed population.


find_my_prob <- function(infected, herd_size = N_herd, probability = 0.5) {
  uniroot(
    function(x) 1 - phyper(1, infected, herd_size - infected, x) - probability,
    c(1, 500)
  )
}

out <- tibble(N_detect = 1:50)

out$p50 <- c(500, ceiling(map_dbl(2:50, ~find_my_prob(.x)$root)))
out$p95 <- c(500, ceiling(map_dbl(2:50, ~find_my_prob(.x, probability = 0.95)$root)))

out |> 
knitr::kable(format = "latex", booktab = TRUE)

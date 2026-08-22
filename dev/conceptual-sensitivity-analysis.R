# Estimated values
gamma.hat <- 1 / 1.15
beta.hat <- 1.2 * gamma.hat
r.hat <- beta.hat - gamma.hat

# Maximum feasible gamma

gamma_max <- 1 / 10
ax.bnds <- c(0, 1.5)
plot(
  0,
  0,
  typ = "n",
  xlab = expression(beta),
  ylab = expression(gamma),
  xlim = ax.bnds,
  ylim = ax.bnds
)
polygon(
  x = c(ax.bnds[1], ax.bnds[2], ax.bnds[2], ax.bnds[1]),
  y = c(0, 0, gamma_max, gamma_max),
  col = adjustcolor("red", alpha.f = 0.25),
  border = NA
)
abline(a = 0, b = 1, col = "black")
abline(h = gamma_max, lty = 2, col = "red")
lines(10 * c(-1, 1), 10 * c(-1, 1) - r.hat, col = "blue", typ = "l")
points(beta.hat, gamma.hat, col = "blue", pch = 19)

R0 <- seq(from = 1.2, to = 10, by = 0.2)
# for (i  in 1:length(R0)){
#  abline(a = 0, b = 1/R0[i], col = 'forestgreen', lty = 2)
# }

(R0.hat <- beta.hat / gamma.hat)
(R0.predict <- 1 + r.hat * 1 / gamma_max)
abline(a = 0, b = 1 / R0.predict, col = "black", lwd = 4)
abline(a = 0, b = 1 / R0.hat, col = "black", lwd = 4)

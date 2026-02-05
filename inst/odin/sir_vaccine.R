steps_per_day <- user(1)
dt <- 1 / steps_per_day
initial(time) <- 0
update(time) <- (step + 1) * dt

## Compartments
update(S[]) <- S[i] - n_SI[i] - n_SR[i]
update(I[]) <- I[i] + n_SI[i] - n_IB[i]
update(B[]) <- B[i] + n_IB[i] - n_BR[i]
update(R[]) <- R[i] + n_BR[i] + n_SR[i]

## Transition probs
p_SI[] <- 1 - exp(-beta * I[i] / N[i]) # S to I
p_IB <- 1 - exp(-gamma) # I to B1
p_BR <- 1 - exp(-kappa)
p_SR_int1 <- if(as.integer(time) > as.integer(t_intervention)) 1 - exp(-zeta) else 0
p_SR_int2 <- if(sum(S_inc[as.integer(1):as.integer(time)]) > detect_n) 1 - exp(-zeta) else 0

## Intervention 1 is time based. Intervention 2 is count based
p_SR <- if(intervention_scenario > 1) p_SR_int2 else p_SR_int1

n_SI[] <- rbinom(S[i], p_SI[i] * dt)
n_IB[] <- rbinom(I[i], p_IB * dt)
n_BR[] <- rbinom(B[i], p_BR * dt)
n_SR[] <- rbinom(S[i], p_SR * dt)
  

## Total population size
N[] <- S[i] + I[i] + B[i] + R[i]

initial(S[]) <- S_ini
initial(I[]) <- I_ini
initial(B[]) <- 0
initial(R[]) <- 0

## Convert some values into incidence
initial(S_inc[]) <- 0
update(S_inc[]) <- if (step %% steps_per_day == 0) n_SI[i] else S_inc[i] + n_SI[i]

initial(V_inc[]) <- 0
update(V_inc[]) <- if (step %% steps_per_day == 0) n_SR[i] else V_inc[i] + n_SR[i]

initial(InfectedCNT[]) <- 0
update(InfectedCNT[]) <- if (step %% steps_per_day == 0) InfectedCNT[i] + n_SI[i] else InfectedCNT[i] + n_SI[i]

initial(InterventionFLG[]) <- 0
update(InterventionFLG[]) <- p_SR

## User inputs
S_ini <- user(499)
I_ini <- user(1)
beta <- user(1.42)
gamma <- user(0.57)
kappa <- user(0.143)
zeta <- user(.1)
detect_n <- user(1)
intervention_scenario <- user(1)
t_intervention <- user(10)


nsim <- user(500)
dim(N) <- nsim
dim(S) <- nsim
dim(I) <- nsim
dim(B) <- nsim
dim(R) <- nsim
dim(S_inc) <- nsim
dim(V_inc) <- nsim
dim(InfectedCNT) <- nsim
dim(p_SI) <- nsim
dim(n_SI) <- nsim
dim(n_IB) <- nsim
dim(n_BR) <- nsim
dim(n_SR) <- nsim
dim(InterventionFLG) <- nsim
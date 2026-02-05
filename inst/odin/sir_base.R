steps_per_day <- user(1)
dt <- 1 / steps_per_day
initial(time) <- 0
update(time) <- (step + 1) * dt

## Compartments 
update(S[]) <- S[i] - n_SI[i]
update(I[]) <- I[i] + n_SI[i] - n_IR[i]
update(R[]) <- R[i] + n_IR[i]

## Transition probs
p_SI[] <- 1 - exp(-beta * I[i] / N[i]) # S to I
p_IR <- 1 - exp(-gamma) # I to B1


n_SI[] <- rbinom(S[i], p_SI[i] * dt)
n_IR[] <- rbinom(I[i], p_IR * dt)
  

## Total population size
N[] <- S[i] + I[i] + R[i]

initial(S[]) <- S_ini
initial(I[]) <- I_ini
initial(R[]) <- 0

## Convert some ßvalues into incidence
initial(S_inc[]) <- 0
update(S_inc[]) <- if (step %% steps_per_day == 0) n_SI[i] else S_inc[i] + n_SI[i]
  
## User inputs
S_ini <- user(499)
I_ini <- user(1)
beta <- user(0.83)
gamma <- user(0.5)

nsim <- user(500)
dim(N) <- nsim
dim(S) <- nsim
dim(I) <- nsim
dim(R) <- nsim
dim(S_inc) <- nsim
dim(p_SI) <- nsim
dim(n_SI) <- nsim
dim(n_IR) <- nsim

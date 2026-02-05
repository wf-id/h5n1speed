# Estimate ODE Intervention outcomes

Simulating the outcomes of the ODE system when an intervention of some
intensity gamma_post is applied at some time point given the threshold
of detection and applied at some delay from identification.

## Usage

``` r
run_intervention_ode(
  r0_in = 1.2,
  gamma_in = 1/1.15,
  gamma_post = 1/(12/24),
  gamma_post_asymptomatic = gamma_in,
  sim_duration = 120,
  kappa = 1/7,
  delay_time = 7,
  p_asymptomatic = 0,
  s_ini = 500 - 1,
  i_ini = 1,
  r_ini = 0,
  phi = 0,
  ...
)
```

## Arguments

- r0_in:

  R0 input

- gamma_in:

  pre-intervention gamma

- gamma_post:

  post-intervention gamma

- sim_duration:

  how long to run simulation

- kappa:

  the symptomatc duration

- delay_time:

  the time to delay an intervention

- s_ini:

  the initial size

- i_ini:

  the initial infected

- r_ini:

  the initial recovered

- phi:

  the proportion of the population that dies from the disease

- ...:

  Arguments passed on to
  [`detect_infections_ode`](https://wf-id.github.io/h5n1speed/reference/detect_infections_ode.md)

  `model`

  :   a solution from an ODE

  `prop_symptomatic`

  :   a numeric indicating the proportion threshold for detection
      considering only those in the "B" class or symptomatic.

  `prop_production_drop`

  :   a numeric indicating the proportion drop in production to indicate
      an outbreak

  `n_detected`

  :   a numeric indicating the number of detected infections which
      considers all of those invididuals existing in the R, Ra, A, I,
      and B classes.

  `n_infected`

  :   a numeric indicating the number of actively infected individuals
      before detection occurs.

## Value

a tibble containing the outputs from the deSolve package of the ode
include the intervention.

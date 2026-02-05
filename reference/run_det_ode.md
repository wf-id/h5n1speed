# Run ODE

Run ordinary differential equation model

## Usage

``` r
run_det_ode(
  init.inf = 1,
  herd.size = 500,
  r0 = 1.2,
  fever.duration = 1.15,
  milk.loss.dur = 7,
  cow.prod.lifespan = 365.25 * 2,
  include.prod.lifespan = FALSE,
  base.milk.prod = 100,
  sympt.milk.prod = 100 - 25,
  recovered.milk.prod = 80,
  discard.sick.prod = FALSE,
  disease.induced.mortality = 0,
  prop.cull = 0,
  sim.length = 365,
  gamma_multiplier = 0,
  beta_multiplier = 0,
  intervention_date = NULL,
  intervention_stop = sim.length,
  p_asymptomatic = 0,
  sigmoid_fun = FALSE,
  add_milk = TRUE
)
```

## Arguments

- init.inf:

  integer, the number of initial infection

- herd.size:

  integer, the size of the herd

- r0:

  positive number, the basic reproduction number

- fever.duration:

  a positive number, the days of fever (infectious)

- milk.loss.dur:

  a positive number, the days of milk loss

- cow.prod.lifespan:

  a positive number, the productive lifespan of a cow in days.

- include.prod.lifespan:

  a logical, should background demographics be considered with a default
  of `FALSE`

- base.milk.prod:

  a positive number, the baseline milk production

- sympt.milk.prod:

  a positive number, the milk production when the cow is symptomatic

- recovered.milk.prod:

  a positive number, the production among recovered cows.

- discard.sick.prod:

  a logical, discard infected milk from production

- disease.induced.mortality:

  a positive number between 0 and 1 representing the mortality rate from
  disease.

- prop.cull:

  a positive number between 0 and 1 representing the proportion of cows
  who recover from infection who are sent to market

- sim.length:

  a positive number, number of days to run the simulation

- gamma_multiplier:

  a number, representing the impact on the recovery rate where values
  greater than 0 represent faster removal and values less than 0
  represent longer recovery times.

- beta_multiplier:

  a number, represent the impact on the contact rate rate where values
  less than 0 indicate less contact while values greater than 0 indicate
  more contact.

- intervention_date:

  a positive number represent the day number of an an intervention with
  a default of `NULL` representing no intervention taking place.

- intervention_stop:

  a positive number representing the day that the intervention in the
  beta_multiple stops. Removal rates are assumed to continue after the
  intervention and stop dates.

- p_asymptomatic:

  a number between 0 and 1, the proportion of the transmission that is
  asymptomatic.

- sigmoid_fun:

  a logical, if true a sigmoid function will be utilized to phase in the
  gamma mulipltier based on the intervention date.

- add_milk:

  a logical, if true milk production will be added to the simulation.

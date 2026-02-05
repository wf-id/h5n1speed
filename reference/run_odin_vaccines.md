# Run odin vaccine

Run the odin model for vaccination representing flows from S -\> I -\> B
-\> R.

## Usage

``` r
run_odin_vaccines(
  intervention_time = 10,
  n_sims = 1,
  r0 = 1.2,
  gamma_in = 1/1.15,
  kappa_in = 0.17,
  zeta_in = 0.1,
  duration = 50,
  s_initial = 500,
  i_initial = 1,
  inter_scenario = 1,
  n_detected = 5
)
```

## Arguments

- intervention_time:

  an interger number representing when the intervention occurs

- n_sims:

  an integer number of simulation

- r0:

  a number representing the basic reproduction number

- gamma_in:

  a number the recovery rate and period of infectiousness

- kappa_in:

  a number, the rate of transition from I to B

- zeta_in:

  a number, the vaccination rate from S to R

- duration:

  an integer representing the duration of the simulation

- s_initial:

  a number, the initial population size

- i_initial:

  a number, the initial number infected

- inter_scenario:

  different scenarios

- n_detected:

  an interger, if using the second intervention the threshold

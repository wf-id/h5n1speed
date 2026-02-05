# Detect Infection from ODE Solution

The purpose of this function is to detect an outbreak under different
scenarios and uses of detection.

## Usage

``` r
detect_infections_ode(
  model,
  prop_symptomatic = NULL,
  prop_production_drop = NULL,
  n_detected = NULL,
  n_infected = NULL,
  ...
)
```

## Arguments

- model:

  a solution from an ODE

- prop_symptomatic:

  a numeric indicating the proportion threshold for detection
  considering only those in the "B" class or symptomatic.

- prop_production_drop:

  a numeric indicating the proportion drop in production to indicate an
  outbreak

- n_detected:

  a numeric indicating the number of detected infections which considers
  all of those invididuals existing in the R, Ra, A, I, and B classes.

- n_infected:

  a numeric indicating the number of actively infected individuals
  before detection occurs.

- ...:

  additional arguments (not used)

## Value

a list object with the time points associated with the detection
approach

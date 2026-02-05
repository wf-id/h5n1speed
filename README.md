
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Time is of the essence: effectiveness of dairy farm control strategies for H5N1 are limited by fast spread

<!-- badges: start -->
<!-- badges: end -->

This repository hosts the code, data and supporting analysis for “Time
is of the essence: effectiveness of dairy farm control for H5N1 is
limited by fast spread.” The analysis was structured in the form of an R
package to facilitate reproducibility.

## Installation

You can install the development version of rRsurveillance from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("wf-id/h5n1speed")
```

## Running the analysis

The primary analysis scripts are available as:

- **R/generate_outbreak_chart.R** contains the code used to generate
  Figure 1, a figure of the ongoing H5N1 outbreak (available in
  `manuscript/figures/outbreak.pdf`)
- **manuscript/delay_strategy.qmd** contains the code to generate Figure
  2, panel A and assembles the figures to make Figure 2.
- **manuscript/intervention-effectiveness.R** contains the code to
  generate the figures shown in Figure 3 panels A-C which shows the
  effectiveness of each surveillance strategy and detection threshold at
  prevent infections.
- **manuscript/time-to-effective-strategy.R** contains the code to
  generate the time required to realize an effective strategy. These
  values are used to generate Figure 3 panels D-F.
- **dev/test-asymptomatic.R** contains the code to explore the role of
  asymptomatic transmission on detection and prevention of an outbreak
  and generates data for Figure 4A
- **dev/asymptomatic-test.R** contains the code to explore the
  contribution of asymptomatic transmission and delay in terms of
  relative risk and also generated the complted Figure 4 with panels
  B-C.
- **dev/conceptual.R** contains the code to generate the outputs used to
  make the conceptual figures shown in Figure 5.

Supplmental analysis:

- **dev/test-mortality.R** contains the code to explore the role of
  disease induced mortality on detection and prevention of an outbreak.

Please note that the R scripts were run across 38 threads on a single
node of the Wake Forest University HPC which took ~5-6hrs. Running these
scripts on a single machine may take a significantly long time.

## Underlying functions

We have parameterized our analysis into an R package to faciliate ease
of use and allow us to explore scenarios of speed on surveillance and
intervention strategies.

- `run_det_ode` contains the code used to run a deterministic
  compartmental model as described in our manuscript. This code use the
  deSolve package to solve the differential equations. Additionally,
  this code adds in the milk production.
- `detect_infections_ode` processes the outputs from the `run_det_ode`
  and find when some particular detection threshold has been met (e.g.,
  number of cows infected cumulatively, number of symptomatic cows with
  clinical signs, some proportion drop in milk production). This
  function returns a list object with the timepoint at which the
  detection threshold is met.
- `run_intervention_ode` allows a given intervention to be applied given
  some detection and surveillance approach (e.g., the number symptomatic
  detected) at that time with some delay with varying intensity.

## ODE System

As a reminder, the ODE is shown below:

$$
\begin{align}
  \frac{dS}{dt} &= - \beta S I/N \\
  \frac{dI}{dt} &= \beta S I/N - \gamma I \\
  \frac{dB}{dt} &= \gamma I - \kappa B \\
  \frac{dR}{dt} &= \kappa B \\
\end{align}
$$

Milk production is calculated as follows:

$$
\text{Milk} = 100 * (S + I) + 75 * B + 80 * R 
$$

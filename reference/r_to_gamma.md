# Simple conversion of R0 to r

Given that r = 1/ phi (\$R_0\$ - 1) then we can solve for phi given some
value of \$R_0\$ by phi = (R_0 -1) / r.

## Usage

``` r
r_to_gamma(r, r_0 = 2.73)
```

## Arguments

- r:

  a number representing the instrinic growth rate

- r_0:

  a number, the target basic reproduction number

## Examples

``` r
r_to_gamma(1, 2)
#> [1] 1
r_to_gamma(1, 4)
#> [1] 3

# Estimate COVID-19
r_to_gamma(c(.22, 0.2, 0.24), 2.5)
#> [1] 6.818182 7.500000 6.250000

# H1N1 2009
r_to_gamma(0.37, 2.3)
#> [1] 3.513514
```

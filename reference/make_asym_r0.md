# Create R0 for asymptomatic infections

Create R0 for asymptomatic infections

## Usage

``` r
make_asym_r0(p, ffi)
```

## Arguments

- p:

  a number representing the proprortion of asymptomatic infections

- ffi:

  a number, final fraction infected

## Examples

``` r
make_asym_r0(0.1, .4)
#> [1] 1.8
stopifnot(make_asym_r0(0.0, .4) - 1 / (1 - .4) < 1e-10)

plot(seq(0, .6, 0.01), 
  sapply(seq(0, .6, 0.01), 
  function(x) make_asym_r0(x, 0.4)), 
  ylim = c(1, 10), 
  type = "l", xlab = "Proportional asymptomatic", ylab = expression(R[0]))

```

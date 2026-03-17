# Hyperband Schedule

Returns hyperband schedule.

## Usage

``` r
hyperband_schedule(r_min, r_max, eta, integer_budget = FALSE)
```

## Arguments

- r_min:

  (`numeric(1)`)  
  Lower bound of budget parameter.

- r_max:

  (`numeric(1)`)  
  Upper bound of budget parameter.

- eta:

  (`numeric(1)`)  
  Fraction parameter of the successive halving algorithm: With every
  stage the configuration budget is increased by a factor of `eta` and
  only the best `1/eta` points are used for the next stage. Non-integer
  values are supported, but `eta` is not allowed to be less or equal 1.

- integer_budget:

  (`logical(1)`)  
  Determines if budget is an integer.

## Value

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)

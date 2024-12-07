
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Keng(庚) <img src="man/figures/logo.png" align="right" height="139" alt="Keng" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/Keng)](https://CRAN.R-project.org/package=Keng)
<!-- badges: end -->

The `Keng` package is named after Loo-Keng Hua, who made great
achievements in mathematics mainly through self-study. Loo-Keng Hua
encouraged novices to show their axe skills at the gate of Ban’s house,
so the `Keng` package comes. In addition, `Keng` is the abbreviation of
“Knock Errors off Nice Guesses.”

The `Keng` package aims to automate the computations Qingyao repeat in
his psychological research and teaching. Hope the functions and data
gathered in this package help to ease your life.

## Installation

You can install the development version of `Keng` from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("qyaozh/Keng", dependencies = TRUE, build_vignettes = TRUE)
```

## Load

Before using the `Keng` package, load it using the `library()` function.

``` r
library(Keng)
```

## List of contents

Here is a list of the functions and data gathered in the `Keng` package.
Their usages are detailed in the documentation.

### Data

`depress` is a subset of data from a research about depression and
coping.

### Variable transformation

`Scale()` could change the origin of a numeric vector `x` (including
mean-centering it), or standardize the mean and standard deviation of
`x` (including transforming it to its z-score).

### Pearson’s *r*

`cut_r()` gives you the cut-off values of Pearson’s *r* at the
significance levels of *p* = 0.1, 0.05, 0.01, 0.001 when the sample size
*n* is known.

`test_r()` tests the significance of *r* using *t*-test and Fisher’s z
when *r* and *n* is known.

### The linear model

`compare_lm()` compares `lm()`’s fitted outputs using *PRE*,
*R*<sup>2</sup>, *f*<sup>2</sup>, and post-hoc power.

`calc_pre()` calculates PRE from partial correlation, Cohen’s f, or
f_squared.

`power_lm()` computes the post-hoc power and/or plans the sample size
for one or a set of predictors in regression analysis.

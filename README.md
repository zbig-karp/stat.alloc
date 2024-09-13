
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stat.alloc

<!-- badges: start -->

<!-- badges: end -->

stat.alloc is an R package with a set of functions which allow for
calculating reference allocations for a given (observed) status
allocation and use these reference allocations to estimate models in a
family of status allocation models proposed in a series of publications
by Zbigniew Karpiński and John Skvoretz (see Karpiński and Skvoretz 2023
for example). Consider the original publications for details concerning
the models.

## Installation

You can install the development version of stat.alloc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zbig-karp/stat.alloc")
```

## Example

This example walks through a calculation of **meritocratic** and
**lottery** allocations from a given (observed) status allocation
Karpiński and Skvoretz (2023) and uses the result to estimate a formal
model of status allocation.

We first set up a status allocation table. It shows counts of
individuals from a given educational category (row of the matrix) who
end up in a given occupational status \[column of the matrix; see Krauze
and Słomczyński (1985) for the source of the data\]:

``` r
ks1985 <- matrix(
  c(135, 52, 58, 12, 6, 
    27, 56, 126, 26, 7, 
    13, 51, 183, 98, 65, 
    3, 8, 30, 23, 21), 
  ncol = 4, dimnames = list(rownames = paste0("E", 1:5), 
                            colnames = paste0("O", 1:4)))
```

Function `mar()` is used to calculate meritocratic and lottery
allocations from a given one:

``` r
library(stat.alloc)

mar(ks1985)
#> $Actual
#>         colnames
#> rownames  O1  O2  O3 O4
#>       E1 135  27  13  3
#>       E2  52  56  51  8
#>       E3  58 126 183 30
#>       E4  12  26  98 23
#>       E5   6   7  65 21
#> 
#> $Meritocratic
#>         colnames
#> rownames  O1  O2  O3 O4
#>       E1 178   0   0  0
#>       E2  85  82   0  0
#>       E3   0 160 237  0
#>       E4   0   0 159  0
#>       E5   0   0  14 85
#> 
#> $Lottery
#>         colnames
#> rownames  O1 O2  O3   O4
#>       E1  47 43  73 15.1
#>       E2  44 40  68 14.2
#>       E3 104 96 163 33.7
#>       E4  42 38  65 13.5
#>       E5  26 24  41  8.4
```

## Constant mixing models

Function `cmm_mde()` estimates a model which treats the observed status
allocation as a mixture of two “pure” status allocation. It returns a
list of three elements:

- mixing coefficient, or the proportion of meritocracy in the mix,
- model-predicted status allocation,
- the index of dissimilarity between the observed and model-predicted
  allocations

``` r
cmm_mde(dat = mar(ks1985))
#> $`Mixing coefficient`
#>   Estimate   S.E. z test p value
#> 1     0.45 0.0026    173 < 0.001
#> 
#> $`Model-predicted allocation`
#>         colnames
#> rownames  O1  O2  O3   O4
#>       E1 106  24  40  8.4
#>       E2  62  59  38  7.8
#>       E3  58 125 196 18.6
#>       E4  23  21 107  7.5
#>       E5  14  13  29 42.7
#> 
#> $`Dissimilarity index`
#> [1] 0.12
```

`cmm_mde()` estimates the mixing coefficient by searching for a value of
the coefficient which minimises the distance (or the Frobenius norm of
the difference) between the observed and model-predicted status
allocations. An alternative, a function called `cmm_mle`, estimates the
coefficient by means of maximum likelihood.

``` r
cmm_mle(dat = mar(ks1985))
#> $`Mixing coefficient`
#>   Estimate  S.E. z test p value
#> 1     0.42 0.027     15 < 0.001
#> 
#> $`Model-predicted allocation`
#>         colnames
#> rownames  O1  O2  O3   O4
#>       E1 102  25  42  8.8
#>       E2  61  58  40  8.3
#>       E3  61 123 194 19.6
#>       E4  24  22 104  7.9
#>       E5  15  14  29 40.5
#> 
#> $`Dissimilarity index`
#> [1] 0.11
```

As we can see, the maximum-likelihood estimate is slightly lower than
the minimum-distance one and it has a considerably larger standard
error. In terms of fit, as measured with the dissimilarity index, the
two estimations are nearly identical, but their predicted counts differ
slightly. The index of dissimilarity between the two model-predicted
allocations is:

``` r
pred_mde <- prop.table(cmm_mde(dat = mar(ks1985))[[2]])
pred_mle <- prop.table(cmm_mle(dat = mar(ks1985))[[2]])
sum(abs(pred_mde - pred_mle))/2
#> [1] 0.015
```

Thus, 15 cases (individuals) out of 1,000 have to be re-allocated to
equalise the two matrices. It’s not much in absolute terms, but it still
shows that the two predictions differ.

## Differential mixing models

So far, the mixing coefficient was assumed to be constant across all
origin categories. It is possible to relax this assumption and allow
each origin category to have its own mixing coefficient. Function
`dmm_mde()` fits a **differential mixing model** using the minimum
distance estimation routine:

``` r
dmm_mde(dat = mar(ks1985))
#> $`Mixing coefficient`
#>   Estimate   S.E. z test p value
#> 1     0.71 0.0039    183 < 0.001
#> 2     0.33 0.0070     47 < 0.001
#> 3     0.15 0.0132     12 < 0.001
#> 4     0.34 0.0071     48 < 0.001
#> 5     0.14 0.0083     16 < 0.001
#> 
#> $`Adjustment proportions`
#>   O1   O2   O3   O4 
#> 0.16 0.28 0.46 0.11 
#> 
#> $`Model-predicted allocation`
#>         colnames
#> rownames  O1  O2  O3   O4
#>       E1 134  14  24  5.5
#>       E2  46  58  52 11.9
#>       E3  53 117 191 35.8
#>       E4  17  29 102 11.1
#>       E5  13  24  41 20.6
#> 
#> $`Dissimilarity index`
#> [1] 0.069
```

The object returned by `dmm_mde` has a structure similar to those
returned by `cmm_mde` or `cmm_mle`. A primary difference is that
`dmm_mde` returns a list of four, rather than three, elements, the
additional element being a named numeric vector of so-called adjustment
proportions, or a special type of parameter introduced into the model to
fit the column marginals (see Karpiński and Skvoretz 2023 for more
details concerning the adjustment proportions).

There is also a function which fits the differential model by means of
maximum likelihood estimation, `dmm_mle()`.

``` r
dmm_mle(dat = mar(ks1985))
#> $`Mixing coefficient`
#>   Estimate  S.E. z test p value
#> 1    0.723 0.035  20.94 < 0.001
#> 2    0.413 0.063   6.55 < 0.001
#> 3    0.094 0.132   0.71    n.s.
#> 4    0.251 0.083   3.03  < 0.01
#> 5    0.171 0.058   2.97  < 0.01
#> 
#> $`Adjustment proportions`
#>    O1    O2    O3    O4 
#> 0.140 0.273 0.488 0.099 
#> 
#> $`Model-predicted allocation`
#>         colnames
#> rownames  O1  O2  O3   O4
#>       E1 136  13  24  4.9
#>       E2  49  61  48  9.8
#>       E3  50 113 198 35.8
#>       E4  17  32  98 11.9
#>       E5  11  22  42 22.7
#> 
#> $`Dissimilarity index`
#> [1] 0.074
```

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-karpinski2023" class="csl-entry">

Karpiński, Zbigniew, and John Skvoretz. 2023. “Status Allocation from
Elementary Allocation Principles.” *Research in Social Stratification
and Mobility* 83 (February): 100769.
<https://doi.org/10.1016/j.rssm.2023.100769>.

</div>

<div id="ref-krauze1985" class="csl-entry">

Krauze, Tadeusz, and Kazimierz M. Słomczyński. 1985. “How Far to
Meritocracy? Empirical Tests of a Controversial Thesis.” *Social Forces*
63: 623–42.

</div>

</div>

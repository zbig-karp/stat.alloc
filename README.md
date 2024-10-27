
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stat.alloc

<!-- badges: start -->

<!-- badges: end -->

stat.alloc is an R package with a set of functions which allow for an
analysis of status allocation, or a process whereby individuals from
different **origin categories** (e.g., educational levels) are allocated
slots in a set of ranked positions (e.g., occupations ranked by the
degree of prestige). Krauze and Słomczyński ([1985](#ref-krauze1985))
proposed elementary status allocation principles: **meritocracy** and
**lottery**, while Karpiński and Skvoretz ([2023](#ref-karpinski2023)),
building on their work, proposed a set of formal status allocation
models which define the outcome of the status allocation process as a
mixture of the elementary principles. A crucial feature of the
Karpiński-Skvoretz status allocation model is a parameter called the
**mixing coefficient** which specifies the contribution of the principle
of meritocracy to the mix. In the simples case, if the mixing
coefficient equal 0.4, it means that 40 percent of the time status
allocation is driven by meritocracy, while 60 percent of the time it is
driven by lottery. Consult the original publications for further details
about the models and their substantive justification.

Functions in the package stat.alloc estimate a family of status
allocation models, namely:

- models which assume that the mixing coefficient is **constant** across
  the origin categories. The relevant functions have names which begin
  with `cmm`, which stands for “constant mixing model”;
- models which assume that the mixing coefficients vary across the
  origin categories. The relevant functions have names which begin with
  `dmm`, which stands for “differential mixing model”;
- models which assume that there is a single basis for status allocation
  (i.e., a merit characteristic) and ones which assume that there are
  two such bases. Functions to estimate models of the latter type
  include `2d` in their names.

In addition, some functions in the package use an approach to estimating
the parameters which consists in minimising the distance (defined in
terms of the Frobenius norm of the difference) between the observed and
model-predicted status allocations; these functions have names which end
with `_mde`, which stands for **minimum distance estimation**. Other
functions in the package estimate the model parameters by maximising the
value of the relevant likelihood function; these functions have names
which end with `_mle`, which stands for `maximum likelihood estimation`.
In most cases, the two approaches to estimation yield estimates which
are very similar, but not identical.

## Installation

You can install the development version of stat.alloc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zbig-karp/stat.alloc")
```

## Example

This example walks through estimation of the simplest status allocation
model, namely, the **constant mixing model**. The outcome of the status
allocation process is represented in the form of a matrix whose rows
correspond to **origin categories**, while its columns correspond to
**destination statuses**. The code below builds a status allocation
table with 5 origin categories (i.e., education levels) and 4
destination statuses (i.e., occupational prestige categories). The rows
are ordered top to bottom from the highest to the lowest education
level, while the columns are ordered left to right from the highest
prestige to the lowest prestige category. Each element of the matrix
represents the number of individuals with a given level of education who
end up in a given occupation. The matrix is reproduced from Krauze and
Słomczyński ([1985](#ref-krauze1985)).

``` r
data(ks1985)
t1 <- xtabs(freq ~ degree + status, data = ks1985)
t1
#>       status
#> degree    S1    S2    S3    S4
#>     E1 11883  2385  1144   277
#>     E2  4567  4960  4474   709
#>     E3  5155 11143 16131  2599
#>     E4  1081  2299  8686  2031
#>     E5   550   577  5731  1859
```

Function `refall()` is used to calculate meritocratic and lottery
allocations from a given one:

``` r
library(stat.alloc)

refall(t1)
#> $Actual
#>       status
#> degree    S1    S2    S3    S4
#>     E1 11883  2385  1144   277
#>     E2  4567  4960  4474   709
#>     E3  5155 11143 16131  2599
#>     E4  1081  2299  8686  2031
#>     E5   550   577  5731  1859
#> 
#> $Meritocratic
#>       status
#> degree    S1    S2    S3   S4
#>     E1 15689     0     0    0
#>     E2  7547  7163     0    0
#>     E3     0 14201 20827    0
#>     E4     0     0 14097    0
#>     E5     0     0  1242 7475
#> 
#> $Lottery
#>       status
#> degree   S1   S2    S3   S4
#>     E1 4131 3798  6430 1329
#>     E2 3874 3561  6029 1246
#>     E3 9224 8481 14356 2967
#>     E4 3712 3413  5778 1194
#>     E5 2295 2110  3573  738
```

Function `cmm_mde()` estimates a model which treats the observed status
allocation as a mixture of two “pure” status allocation. It returns a
list of three elements:

- mixing coefficient, or the proportion of meritocracy in the mix,
- model-predicted status allocation,
- the index of dissimilarity between the observed and model-predicted
  allocations

``` r
cmm_mde(dat = refall(t1))
#> $`Mixing coefficient`
#>   Estimate     S.E. z test p value
#> 1    0.448 2.93e-05  15251 < 0.001
#> 
#> $`Model-predicted allocation`
#>       status
#> degree   S1    S2    S3   S4
#>     E1 9304  2098  3552  734
#>     E2 5518  5173  3331  688
#>     E3 5096 11041 17252 1639
#>     E4 2051  1885  9501  660
#>     E5 1268  1166  2530 3753
#> 
#> $`Dissimilarity index`
#> [1] 0.115
```

Other functions in the package are used in a similar way.

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

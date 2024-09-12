
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
#> rownames      O1     O2     O3     O4
#>       E1  46.814 43.076  72.98 15.130
#>       E2  43.921 40.414  68.47 14.195
#>       E3 104.411 96.074 162.77 33.745
#>       E4  41.817 38.478  65.19 13.515
#>       E5  26.037 23.958  40.59  8.415
```

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
#>    Estimate        S.E.   z test p value
#> 1 0.4476211 0.002588898 172.9002 < 0.001
#> 
#> $`Model-predicted allocation`
#>         colnames
#> rownames        O1        O2        O3        O4
#>       E1 105.53562  23.79427  40.31261  8.357493
#>       E2  62.30883  59.02877  37.82138  7.841019
#>       E3  57.67443 124.68863 195.99691 18.640026
#>       E4  23.09883  21.25444 107.18133  7.465401
#>       E5  14.38229  13.23389  28.68776 42.696061
#> 
#> $`Dissimilarity index`
#> [1] 0.115597
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

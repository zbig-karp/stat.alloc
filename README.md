
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

This is a basic example which shows you how to calculate
**meritocratic** and **lottery** allocations from a given (observed)
status allocation Karpiński and Skvoretz (2023).

The code below defines an example of a status allocation table. The
table is reproduced from a seminal paper by Krauze and Słomczyński
(1985). It shows counts of individuals from a given educational category
(row of the matrix) who end up in a given occupational status (column of
the matrix):

``` r
ks1985 <- matrix(
  c(135, 52, 58, 12, 6, 
    27, 56, 126, 26, 7, 
    13, 51, 183, 98, 65, 
    3, 8, 30, 23, 21), 
  ncol = 4, dimnames = list(rownames = paste0("$E_", 1:5, "$"), 
                            colnames = paste0("$O_", 1:4, "$")))
```

Two **reference allocations** can be proposed for a given status
allocation: a **meritocratic** allocation and a **lottery allocation**.
Under the former, individuals are assigned to destination statuses based
on their merits, so that those with the highest merits are the first to
be assigned to the highest status. As a result, under perfectly
meritocratic allocation, it is never the case that a person with lower
merits ends up in higher destination status than a person with higher
merits. Formally, the result of the fully meritocratic status allocation
is defined as follows: $$
m_{gh} = \min{\left(a_{g+} - \sum_{i=0}^{h-1}m_{gi},\,a_{+h} - \sum_{j=0}^{i-1} m_{jh}\right)},
$$ where $m_{gh}$ is the number of individuals from education category
$g$ who end up in occupational status $h$, $a_{g+}$ is the
$g$<sup>th</sup> row marginal and $a_{+h}$ is the $h$th column marginal.
Further, we set $m_{g0} = m_{0h} = 0$ for simplicity of expression.
Allocation by this procedure insures that it is never the case that a
person with lower credentials is found in a higher destination status
than a person with higher credentials.

``` r
library(stat.alloc)

mar(ks1985)
#> $Actual
#>         colnames
#> rownames $O_1$ $O_2$ $O_3$ $O_4$
#>    $E_1$   135    27    13     3
#>    $E_2$    52    56    51     8
#>    $E_3$    58   126   183    30
#>    $E_4$    12    26    98    23
#>    $E_5$     6     7    65    21
#> 
#> $Meritocratic
#>         colnames
#> rownames $O_1$ $O_2$ $O_3$ $O_4$
#>    $E_1$   178     0     0     0
#>    $E_2$    85    82     0     0
#>    $E_3$     0   160   237     0
#>    $E_4$     0     0   159     0
#>    $E_5$     0     0    14    85
#> 
#> $Lottery
#>         colnames
#> rownames   $O_1$  $O_2$  $O_3$  $O_4$
#>    $E_1$  46.814 43.076  72.98 15.130
#>    $E_2$  43.921 40.414  68.47 14.195
#>    $E_3$ 104.411 96.074 162.77 33.745
#>    $E_4$  41.817 38.478  65.19 13.515
#>    $E_5$  26.037 23.958  40.59  8.415
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

#' 1977 labour force participation by education, gender, and status
#'
#' A subset of the 1977 report by the U.S. Department of Labour, Bureau of Labour Statistics, based on data in Table K in the Appendix of the original report (see the link below)
#'
#' @format ## `ks1985`
#' A data frame with 40 rows and 4 columns:
#' \describe{
#'   \item{edu}{Educational attainment classified into 5 categories:
#'     **E1** -- College, 4 years or more; **E2** -- College, 1-3 years;
#'     **E3** -- High school, 4 years; **E4** -- High school, 1-3 years;
#'     **E5** -- Elementary school, 8 years or less}
#'   \item{sex}{Gender, coded **M** for male and **F** for female}
#'   \item{status}{Occupational status classified into 4 categories:
#'     **S1** -- Professional, technical and kindred workers; Managers and administrators,
#'     except farm; **S2** -- Sales workers; Clerical and kindred workers; **S3** -- Craft and
#'      kindred workers; Operatives, except transport; Transport equipment operatives; Service
#'      workers, except household; **S4** -- laborers, except farm; Farm workers; Private household
#'       workers}
#'   \item{freq}{Frequencies (counts) of individuals in each joint category of `edu`, `sex`, and
#'    `status`}
#' }
#' @source U.S. Department of Labor, Bureau of Labor Statistics. 1977. "Educational Attainment of
#'         Workers", [Special Labor Force Report 209, March 1977](https://www.google.pl/books/edition/Special_Labor_Force_Reports/t8VpM89u6XMC?hl=en&gbpv=1&pg=RA4-PP3&printsec=frontcover)
"ks1985"

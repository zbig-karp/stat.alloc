% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refall.R
\name{refall2d}
\alias{refall2d}
\title{Two-dimensional model of status allocation}
\usage{
refall2d(dat, n1 = NULL, n2 = NULL)
}
\arguments{
\item{dat}{a matrix representing the outcome of a process of status allocation with two merit dimensions (or two bases for status allocation). The rows of the matrix correspond to combined categories of the merit characteristics ordered lexicographically. The rows should have names of the form 'Xi-Yj', where the symbol left of the dash indicates category of the primary characteristic and the symbol right of the dash indicates the category of the secondary characteristic}

\item{n1}{the number of categories of the primary characteristic. Ignored if \code{!is.null(rownames(dat))}. Necessary if \code{is.null(rownames(dat))}}

\item{n2}{the number of categories of the secondary characteristic. Ignored if \code{!is.null(rownames(dat))}. Necessary if \code{is.null(rownames(dat))}}
}
\value{
a list with five status allocation matrices:
\itemize{
\item actual (or observed);
\item meritocratic with respect to both dimensions;
\item meritocratic with respect to the primary and lottery with respect to the secondary dimension;
\item lottery with respect to the primary and meritocratic with respect to the secondary dimension; and
\item lottery with respect to both dimensions
}
}
\description{
Two-dimensional model of status allocation
}
\details{
The function requires that \code{dat} has row names, so either provide the names explicitly or specify the numbers of categories of the two merit characteristics, using the arguments \code{n1} and \code{n2}. In the latter case, some generic labels will be generated. For example, in the case of two merit characteristics, X and Y, a vector of row names will be generated of the following form: \code{Xi-Yj}, where $i = 1,\ldots,n1$ and $j = 1,\ldots,n2$. If the row names are provided explicitly, n1 and n2 are ignored.
}
\examples{
data(ks1985)
refall2d(dat = xtabs(freq ~ origin + status, data = ks1985))
}

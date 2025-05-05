#' Single-dimensional model of status allocation
#'
#' @param x A numeric matrix
#'
#' @return A list of three matrices, each of the same order as `x`:
#' - The first element of the list is the original status allocation matrix, `x`.
#' - The second element of the list corresponds to a fully meritocratic status allocation, i.e., a status allocation which would be observed the allocation to destination statuses were driven solely by the principle of meritocracy, as defined by Krauze and Słomczyński (1985).
#' - The third element of the list corresponds to a status allocation matrix which would be observed if the allocation to destination statuses were driven solely by lottery.
#'
#' @export
#'
#' @examples
#' data(ks1985)
#' t1 <- xtabs(freq ~ degree + status, data = ks1985)
#' refall(x = t1)
refall <- function(x) {
  stopifnot("x must be a numeric matrix" = is.matrix(x),
            "x must be a numeric matrix" = is.numeric(x))
  #x <- x[rowSums(x) > 0, colSums(x) > 0]
  l <- rowSums(x) %o% colSums(x)/sum(x)
  dimnames(l) <- dimnames(x)
  m <- matrix(0, nrow = nrow(x), ncol = ncol(x), dimnames = dimnames(x))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) m[i,j] <- min(rowSums(x)[i] - sum(m[i,1:j]), colSums(x)[j] - sum(m[1:i,j]))
  }
  list(Actual = x, Meritocratic = m, Lottery = l)
}

#' Two-dimensional model of status allocation
#'
#' @param dat a matrix representing the outcome of a process of status allocation with two merit dimensions (or two bases for status allocation). The rows of the matrix correspond to combined categories of the merit characteristics ordered lexicographically. The rows should have names of the form 'Xi-Yj', where the symbol left of the dash indicates category of the primary characteristic and the symbol right of the dash indicates the category of the secondary characteristic
#' @param n1 the number of categories of the primary characteristic. Ignored if `!is.null(rownames(dat))`. Necessary if `is.null(rownames(dat))`
#' @param n2 the number of categories of the secondary characteristic. Ignored if `!is.null(rownames(dat))`. Necessary if `is.null(rownames(dat))`
#' @details
#' The function requires that `dat` has row names, so either provide the names explicitly or specify the numbers of categories of the two merit characteristics, using the arguments `n1` and `n2`. In the latter case, some generic labels will be generated. For example, in the case of two merit characteristics, X and Y, a vector of row names will be generated of the following form: `Xi-Yj`, where $i = 1,\ldots,n1$ and $j = 1,\ldots,n2$. If the row names are provided explicitly, n1 and n2 are ignored.
#'
#'
#' @return a list with five status allocation matrices:
#' - actual (or observed);
#' - meritocratic with respect to both dimensions;
#' - meritocratic with respect to the primary and lottery with respect to the secondary dimension;
#' - lottery with respect to the primary and meritocratic with respect to the secondary dimension; and
#' - lottery with respect to both dimensions
#' @export
#'
#' @examples
#' data(ks1985)
#' refall2d(dat = xtabs(freq ~ origin + status, data = ks1985))

refall2d <- function(dat, n1 = NULL, n2 = NULL) {
  if (is.null(rownames(dat)) & (is.null(n1) | is.null(n2)))
    stop("Row names for `dat` are required for the function to be executed")
  else if (is.null(rownames(dat)) & !(is.null(n1) | is.null(n2))) {
    if (n1 * n2 != nrow(dat)) stop("The numbers `n1` and `n2` are inconsistent with `nrow(dat)`!")
    x_cat <- paste0("X", 1:n1)
    y_cat <- paste0("Y", 1:n2)
    rownames(dat) <- paste(rep(x_cat, each = n2), rep(y_cat, times = n1), sep = "-")
  }

  # The labels for the original categories (taken from the rownames of dat)
  r_margs <- str_split(string = rownames(dat), pattern = "-", simplify = TRUE)

  # Categories of the primary dimension
  d1_levels <- unique(r_margs[, 1])
  n1 <- length(d1_levels)

  # Categories of the secondary dimension
  d2_levels <- unique(r_margs[, 2])
  n2 <- length(d2_levels)

  # Collapsing dat over the categories of the secondary dimension to obtain status allocation
  # along the primary dimension only
  dat1d <- matrix(NA, nrow = n1, ncol = ncol(dat),
                  dimnames = list(
                    origin = d1_levels,
                    destination = colnames(dat)
                  ))
  for (i in 1:n1) {
    .f <- n2 * i - (n2 - 1)
    .t <- n2 * i
    dat1d[i, ] <- colSums(dat[.f:.t, ])
  }

  # Merit and lottery allocations along the primary dimension
  stall <- refall(dat1d)

  # Calculating the reference distributions resulting from meritocracy along the primary dimension
  aux <- vector(mode = "list", length = n1)
  for (i in 1:n1) {
    .f <- n2 * i - (n2 - 1)
    .t <- n2 * i
    aux[[i]] <- outer(rowSums(dat)[.f:.t], stall[[2]][i, ])/sum(stall[[2]][i, ])
  }
  aux <- aux |>
    map(refall)
  mm <- aux |>
    map(~.[[2]]) %>%
    do.call("rbind", .)
  ml <- aux |>
    map(~.[[3]]) %>%
    do.call("rbind", .)
  rm(aux)

  # Calculating the reference distributions resulting from lottery along the primary dimension
  aux <- vector(mode = "list", length = n1)
  for (i in 1:n1) {
    .f <- n2 * i - (n2 - 1)
    .t <- n2 * i
    aux[[i]] <- outer(rowSums(dat)[.f:.t], stall[[3]][i, ])/sum(stall[[3]][i, ])
  }
  aux <- aux |>
    map(refall)
  lm <- aux |>
    map(~.[[2]]) %>%
    do.call("rbind", .)
  ll <- aux |>
    map(~.[[3]]) %>%
    do.call("rbind", .)

  out <- list(Actual = dat, MM = mm, ML = ml, LM = lm, LL = ll)
  rm(list = ls()[ls() != "out"])
  out
}

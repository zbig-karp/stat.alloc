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
#' @param d the data frame containing the data
#' @param dim1 the name of the variable in `d` corresponding to the primary merit characteristic
#' @param dim2 the name of the variable in `d` corresponding to the secondary merit characteristic
#' @param status the name of the variable in `d` corresponding to the destination status
#' @param f the name of the variable in `d` corresponding to the counts of unique combinations of the values of dim1, dim2, and status
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
#' refall2d(d = ks1985, dim1 = "degree", dim2 = "sex", status = "status", f = "freq")

refall2d <- function(d, dim1, dim2, status, f) {

  # Changing the names of the original variables

  d <- d[, c(f, dim1, dim2, status)]
  names(d) <- c("f", "dim1", "dim2", "status")

  # Reordering the rows of d by levels of the merit dimensions
  d <- d |>
    dplyr::arrange(dim1, dim2)

  # Cross-classification of the merit dimensions
  t1 <- xtabs(f ~ dim1 + dim2, data = d)

  # Status allocation table collapsed over the categories of dim2
  t2 <- xtabs(f ~ dim1 + status, data = d)

  # Lottery and meritocratic allocations based on t2
  t2 <- refall(t2)

  # Joint origin category
  d <- d |>
    tidyr::unite(col = "origin", dim1, dim2, sep = "-", remove = FALSE) |>
    dplyr::mutate(origin = forcats::fct_inorder(origin))

  # Observed status allocation with joint origin categories
  t3 <- xtabs(f ~ origin + status, data = d)

  ll <- vector(mode = "list", length = nrow(t1))
  for (i in 1:length(ll)) ll[[i]] <- outer(t1[i, ], t2[[3]][i, ], "*")/sum(t1[i, ])

  lm <- purrr::map(ll, ~refall(.)[[2]])

  ml <- vector(mode = "list", length = nrow(t1))
  for (i in 1:length(ml)) ml[[i]] <- outer(t1[i, ], t2[[2]][i, ], "*")/sum(t1[i, ])

  mm <- purrr::map(ml, ~refall(.)[[2]])

  out <- list(mm, ml, lm, ll) |>
    purrr::map(~do.call("rbind", .))

  for (i in 1:length(out)) rownames(out[[i]]) <- rownames(t3)
  names(out) <- c("Meritocracy on both merit dimensions", "Meritocracy on the primary, lottery on the secondary dimension", "Lottery on the primary, meritocracy on the secondary dimension", "Lottery on both merit dimensions")

  c("Actual" = list(t3), out)
}

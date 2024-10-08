# Single-dimensional model of status allocation: a function returning a list with three status allocation matrices: actual (or observed), meritocratic, and lottery-based

#' Title
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
#' ks1985 <- matrix(c(135, 52, 58, 12, 6, 27, 56, 126, 26, 7, 13, 51, 183, 98, 65, 3, 8, 30, 23, 2), ncol = 4, dimnames = list(rownames = paste0("E", 1:5), colnames = paste0("O", 1:4)))
#' mar(x = ks1985)
mar <- function(x) {
  stopifnot("x must be a numeric matrix" = is.matrix(x),
            "x must be a numeric matrix" = is.numeric(x))
  x <- x[rowSums(x) > 0, colSums(x) > 0]
  l <- rowSums(x) %o% colSums(x)/sum(x)
  dimnames(l) <- dimnames(x)
  m <- matrix(0, nrow = nrow(x), ncol = ncol(x), dimnames = dimnames(x))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) m[i,j] <- min(rowSums(x)[i] - sum(m[i,1:j]), colSums(x)[j] - sum(m[1:i,j]))
  }
  list(Actual = x, Meritocratic = m, Lottery = l)
}

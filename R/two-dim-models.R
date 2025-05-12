#' Two-dimensional constant mixing model with the minimum-distance estimation routine
#'
#' @param dat An object returned by the function `refall2d`
#'
#' @return a list with four elements:
#'   - a tibble with the estimated coefficients and relevant statistical tests;
#'   - a matrix representing the model-predicted status allocation;
#'   - the index of dissimilarity between the observed and model-predicted status allocations;
#'   - the minimised distance (i.e., the Frobenius norm of the difference) between the observed and model-predicted status allocations
#' @export
#'
#' @examples
#' t1 <- refall2d(xtabs(freq ~ origin + status, data = ks1985))
#' cmm2d_mde(t1)


cmm2d_mde <- function(dat) {
  mixcoef <- function(dat, mc) {
    a <- mc[1]
    b <- mc[2]
    pred <- (1 - a) * (1 - b) * dat[[5]] + (1 - a) * b * dat[[4]] + a * (1 - b) * dat[[3]] +
      a * b * dat[[2]]
    norm(dat[[1]] - pred, type = "F")
  }

  # Minimising the distance between the observed and the model-predicted allocations
  out <- optim(par = c(0.1, 0.1),
               fn = mixcoef,
               dat = dat,
               method = "L-BFGS-B",
               lower = c(0, 0), upper = c(1, 1),
               hessian = TRUE)

  est_table <- tibble::tibble(
    term = c("alpha", "beta"),
    Estimate = out$par,
    `S.E.` = sqrt(diag(1/out$hessian)),
    `z test` = Estimate/`S.E.`,
    `p value` = pnorm(`z test`, lower.tail = F)
  )

  a <- out$par[1]
  b <- out$par[2]
  pred <- (1 - a) * (1 - b) * dat[[5]] + (1 - a) * b * dat[[4]] + a * (1 - b) * dat[[3]] +
    a * b * dat[[2]]

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Model-predicted allocation` = pred,
       `Dissimilarity index` = sum(abs(dat[[1]] - pred))/(2 * sum(dat[[1]])),
       `Distance (Frobenius)` = norm(dat[[1]] - pred, type = "F")/sum(dat[[1]]))
}

#' Two-dimensional constant mixing model with the maximum-likelihood estimation routine
#'
#' @param dat An object returned by the function `refall2d`
#'
#' @return a list with four elements:
#'   - a tibble with the estimated coefficients and relevant statistical tests;
#'   - a matrix representing the model-predicted status allocation;
#'   - the index of dissimilarity between the observed and model-predicted status allocations;
#'   - the minimised distance (i.e., the Frobenius norm of the difference) between the observed and model-predicted status allocations
#' @export
#'
#' @examples
#' t1 <- refall2d(xtabs(freq ~ origin + status, data = ks1985))
#' cmm2d_mde(t1)

cmm2d_mle <- function(dat) {
  mixcoef <- function(dat, mc) {
    a <- mc[1]
    b <- mc[2]
    pred <- (1 - a) * (1 - b) * dat[[5]] + (1 - a) * b * dat[[4]] + a * (1 - b) * dat[[3]] +
      a * b * dat[[2]]

    fake <- 9.999 * 10^9
    ll <- -sum(dat$Actual * log(pred))

    if (is.finite(ll)) {
      fake <- ll
    } else {
      ll <- fake
    }
    ll
  }

  # Maximising the likelihood with respect to the model parameters
  out <- optim(par = c(0.1, 0.1),
               fn = mixcoef,
               dat = dat,
               method = "L-BFGS-B",
               lower = c(0, 0), upper = c(1, 1),
               hessian = TRUE)

  est_table <- tibble::tibble(
    term = c("alpha", "beta"),
    Estimate = out$par,
    `S.E.` = sqrt(diag(1/out$hessian)),
    `z test` = Estimate/`S.E.`,
    `p value` = pnorm(`z test`, lower.tail = F)
  )

  a <- out$par[1]
  b <- out$par[2]
  pred <- (1 - a) * (1 - b) * dat[[5]] + (1 - a) * b * dat[[4]] + a * (1 - b) * dat[[3]] +
    a * b * dat[[2]]

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Model-predicted allocation` = pred,
       `Dissimilarity index` = sum(abs(dat[[1]] - pred))/(2 * sum(dat[[1]])),
       `Distance (Frobenius)` = norm(dat[[1]] - pred, type = "F")/sum(dat[[1]]))
}

#' Two-dimensional constant mixing model with the minimum-distance estimation routine: an extended variant
#'
#' @param dat An object returned by the function `refall2d`
#'
#' @return a list with four elements:
#'   - a tibble with the estimated coefficients and relevant statistical tests;
#'   - a matrix representing the model-predicted status allocation;
#'   - the index of dissimilarity between the observed and model-predicted status allocations;
#'   - the minimised distance (i.e., the Frobenius norm of the difference) between the observed and model-predicted status allocations
#'
#' `cmm2d_mde_ext` implements an extension of `cmm2d_mde` in which the mixing coefficient on the secondary merit dimension is allowed to vary depending on whether status allocation with respect to the primary dimension is driven by lottery or meritocracy.
#'
#' @export
#'
#' @examples
#' t1 <- refall2d(xtabs(freq ~ origin + status, data = ks1985))
#' cmm2d_mde_ext(t1)


cmm2d_mde_ext <- function(dat) {
  mixcoef <- function(dat, mc) {
    a <- mc[1]
    bL <- mc[2]
    bM <- mc[3]
    pred <- (1 - a) * (1 - bL) * dat[[5]] + (1 - a) * bL * dat[[4]] + a * (1 - bM) * dat[[3]] +
      a * bM * dat[[2]]
    norm(dat[[1]] - pred, type = "F")
  }

  # Minimising the distance between the observed and the model-predicted allocations
  out <- optim(par = c(0.1, 0.1, 0.1),
               fn = mixcoef,
               dat = dat,
               method = "L-BFGS-B",
               lower = c(0, 0, 0), upper = c(1, 1, 1),
               hessian = TRUE)

  est_table <- tibble::tibble(
    term = c("alpha", "beta (L)", "beta (M)"),
    Estimate = out$par,
    `S.E.` = sqrt(diag(1/out$hessian)),
    `z test` = Estimate/`S.E.`,
    `p value` = pnorm(`z test`, lower.tail = F)
  )

  a <- out$par[1]
  bL <- out$par[2]
  bM <- out$par[3]
  pred <- (1 - a) * (1 - bL) * dat[[5]] + (1 - a) * bL * dat[[4]] + a * (1 - bM) * dat[[3]] +
    a * bM * dat[[2]]

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Model-predicted allocation` = pred,
       `Dissimilarity index` = sum(abs(dat[[1]] - pred))/(2 * sum(dat[[1]])),
       `Distance (Frobenius)` = norm(dat[[1]] - pred, type = "F")/sum(dat[[1]]))
}

#' Two-dimensional constant mixing model with the maximum-likelihood estimation routine: an extended version
#'
#' @param dat An object returned by the function `refall2d`
#'
#' @return a list with four elements:
#'   - a tibble with the estimated coefficients and relevant statistical tests;
#'   - a matrix representing the model-predicted status allocation;
#'   - the index of dissimilarity between the observed and model-predicted status allocations;
#'   - the minimised distance (i.e., the Frobenius norm of the difference) between the observed and model-predicted status allocations
#'
#' `cmm2d_mle_ext` implements an extension of `cmm2d_mle` in which the mixing coefficient on the secondary merit dimension is allowed to vary depending on whether status allocation with respect to the primary dimension is driven by lottery or meritocracy.
#'
#' @export
#'
#' @examples
#' t1 <- refall2d(xtabs(freq ~ origin + status, data = ks1985))
#' cmm2d_mde(t1)

cmm2d_mle_ext <- function(dat) {
  mixcoef <- function(dat, mc) {
    a <- mc[1]
    bL <- mc[2]
    bM <- mc[3]
    pred <- (1 - a) * (1 - bL) * dat[[5]] + (1 - a) * bL * dat[[4]] + a * (1 - bM) * dat[[3]] +
      a * bM * dat[[2]]

    fake <- 9.999 * 10^9
    ll <- -sum(dat$Actual * log(pred))

    if (is.finite(ll)) {
      fake <- ll
    } else {
      ll <- fake
    }
    ll
  }

  # Maximising the likelihood with respect to the model parameters
  out <- optim(par = c(0.1, 0.1, 0.1),
               fn = mixcoef,
               dat = dat,
               method = "L-BFGS-B",
               lower = c(0, 0, 0), upper = c(1, 1, 1),
               hessian = TRUE)

  est_table <- tibble::tibble(
    term = c("alpha", "beta (L)", "beta (M)"),
    Estimate = out$par,
    `S.E.` = sqrt(diag(1/out$hessian)),
    `z test` = Estimate/`S.E.`,
    `p value` = pnorm(`z test`, lower.tail = F)
  )

  a <- out$par[1]
  bL <- out$par[2]
  bM <- out$par[3]
  pred <- (1 - a) * (1 - bL) * dat[[5]] + (1 - a) * bL * dat[[4]] + a * (1 - bM) * dat[[3]] +
    a * bM * dat[[2]]

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Model-predicted allocation` = pred,
       `Dissimilarity index` = sum(abs(dat[[1]] - pred))/(2 * sum(dat[[1]])),
       `Distance (Frobenius)` = norm(dat[[1]] - pred, type = "F")/sum(dat[[1]]))
}

#' Two-dimensional differential mixing model with the minimum-distance estimation routine
#'
#' @param dat An object returned by the function `refall2d`
#'
#' @return a list with five elements:
#'   - a tibble with the estimated coefficients and relevant statistical tests;
#'   - a named vector of adjustment proportions
#'   - the index of dissimilarity between the observed and model-predicted status allocations;
#'   - the minimised distance (i.e., the Frobenius norm of the difference) between the observed and model-predicted status allocations
#'   - a matrix representing the model-predicted status allocation;
#' @export
#'
#' @examples
#' t1 <- refall2d(xtabs(freq ~ origin + status, data = ks1985))
#' dmm2d_mde(t1)

dmm2d_mde <- function(dat) {

  # Auxiliary objects ----
  # Joint categories of the merit characteristics
  mcats <- rownames(dat[[1]])
  mcats <- str_split(string = mcats, pattern = "-", simplify = TRUE)
  # Categories of the primary characteristic
  mcat1 <- unique(mcats[, 1])
  # Categories of the secondary characteristic
  mcat2 <- unique(mcats[, 2])
  # The number of categories of the primary characteristic
  k1 <- length(mcat1)
  # The number of categories of the secondary characteristic
  k2 <- length(mcat2)

  # The function to be optimised over ----
  mixcoef <- function(dat, mc) {
    a <- mc[1:k1]
    b <- mc[(k1 + 1):(k1 + k2)]

    # Adjustment proportions
    target <- rep(0, times = ncol(dat[[1]]))
    target <- (colSums(dat[[1]]) -
                 colSums(rep(a, each = k2) * rep(b, times = k1) * dat[[2]]) -
                 colSums(rep(a, each = k2) * rep(1 - b, times = k1) * dat[[3]]) -
                 colSums(rep(1 - a, each = k2) * rep(b, times = k1) * dat[[4]]))/
      sum(as.double(outer(1 - b, 1 - a)) * rowSums(dat[[5]]))

    # Predicted counts
    pred <- rep(a, each = k2) * rep(b, times = k1) * dat[[2]] +
      rep(a, each = k2) * rep(1 - b, times = k1) * dat[[3]] +
      rep(1 - a, each = k2) * rep(b, times = k1) * dat[[4]] +
      rep(1 - a, each = k2) * rep(1 - b, times = k1) * outer(rowSums(dat[[5]]), target)

    # The distance from prediction to observation
    ll <- norm(pred - dat[[1]], type = "F")
    fake <- 99999999
    if (is.finite(ll)) {
      fake <- ll
    } else {
      ll <- fake
    }
    ll
  }

  out <- optim(par = rep(0.5, k1 + k2),
               fn = mixcoef,
               dat = dat,
               method = "L-BFGS-B",
               lower = rep(0, k1 + k2),
               upper = rep(1, k1 + k2),
               hessian = TRUE)

  a <- out$par[1:k1]
  b <- out$par[(k1 + 1):(k1 + k2)]

  # A table with mixing coefficients
  est_table <- tibble(
    term = paste(c(rep("Dim 1", k1), rep("Dim 2", k2)), c(mcat1, mcat2), sep = ": "),
    Estimate = out$par,
    `S.E.` = sqrt(diag(1/out$hessian)),
    `z-statistic` = Estimate/`S.E.`,
    `p-value` = 2 * pnorm(abs(`z-statistic`), lower.tail = FALSE)
  )

  # Adjustment proportions
  target <- rep(0, times = ncol(dat[[1]]))
  target <- (colSums(dat[[1]]) -
               colSums(rep(a, each = k2) * rep(b, times = k1) * dat[[2]]) -
               colSums(rep(a, each = k2) * rep(1 - b, times = k1) * dat[[3]]) -
               colSums(rep(1 - a, each = k2) * rep(b, times = k1) * dat[[4]]))/
    sum(as.double(outer(1 - b, 1 - a)) * rowSums(dat[[5]]))


  # A table with adjustment proportions
  adj_prop <- tibble(
    status = colnames(dat[[1]]),
    adj_prop = target
  )

  # Predicted counts
  pred <- rep(a, each = k2) * rep(b, times = k1) * dat[[2]] +
    rep(a, each = k2) * rep(1 - b, times = k1) * dat[[3]] +
    rep(1 - a, each = k2) * rep(b, times = k1) * dat[[4]] +
    rep(1 - a, each = k2) * rep(1 - b, times = k1) * outer(rowSums(dat[[5]]), target)

  # Goodness of fit
  delta <- sum(abs(pred - dat[[1]]))/(2 * sum(dat[[1]]))

  # Distance
  distance <- out$value

  list("Mixing coefficient" = est_table, "Predicted counts" = pred,
       "Dissimilarity index" = delta,
       "Distance (Frobenius)" = norm(dat[[1]] - pred, type = "F")/sum(dat[[1]]),
       "Adjustment proportions" = target)
}

#' Two-dimensional differential mixing model with the maximum-likelihood estimation routine
#'
#' @param dat An object returned by the function `refall2d`
#'
#' @return a list with five elements:
#'   - a tibble with the estimated coefficients and relevant statistical tests;
#'   - a named vector of adjustment proportions
#'   - the index of dissimilarity between the observed and model-predicted status allocations;
#'   - the minimised distance (i.e., the Frobenius norm of the difference) between the observed and model-predicted status allocations
#'   - a matrix representing the model-predicted status allocation;
#' @export
#'
#' @examples
#' t1 <- refall2d(xtabs(freq ~ origin + status, data = ks1985))
#' dmm2d_mde(t1)

dmm2d_mle <- function(dat) {

  # Auxiliary objects ----
  # Joint categories of the merit characteristics
  mcats <- rownames(dat[[1]])
  mcats <- str_split(string = mcats, pattern = "-", simplify = TRUE)
  # Categories of the primary characteristic
  mcat1 <- unique(mcats[, 1])
  # Categories of the secondary characteristic
  mcat2 <- unique(mcats[, 2])
  # The number of categories of the primary characteristic
  k1 <- length(mcat1)
  # The number of categories of the secondary characteristic
  k2 <- length(mcat2)

  # The function to be optimised over ----
  mixcoef <- function(dat, mc) {
    a <- mc[1:k1]
    b <- mc[(k1 + 1):(k1 + k2)]

    # Adjustment proportions
    target <- rep(0, times = ncol(dat[[1]]))
    target <- (colSums(dat[[1]]) -
                 colSums(rep(a, each = k2) * rep(b, times = k1) * dat[[2]]) -
                 colSums(rep(a, each = k2) * rep(1 - b, times = k1) * dat[[3]]) -
                 colSums(rep(1 - a, each = k2) * rep(b, times = k1) * dat[[4]]))/
      sum(as.double(outer(1 - b, 1 - a)) * rowSums(dat[[5]]))

    # Predicted counts
    pred <- rep(a, each = k2) * rep(b, times = k1) * dat[[2]] +
      rep(a, each = k2) * rep(1 - b, times = k1) * dat[[3]] +
      rep(1 - a, each = k2) * rep(b, times = k1) * dat[[4]] +
      rep(1 - a, each = k2) * rep(1 - b, times = k1) * outer(rowSums(dat[[5]]), target)

    fake <- 9.999 * 10^9
    ll <- -sum(dat$Actual * log(pred))

    if (is.finite(ll)) {
      fake <- ll
    } else {
      ll <- fake
    }
    ll
  }

  out <- optim(par = rep(0.5, k1 + k2),
               fn = mixcoef,
               dat = dat,
               method = "L-BFGS-B",
               lower = rep(0, k1 + k2),
               upper = rep(1, k1 + k2),
               hessian = TRUE)

  # A table with mixing coefficients
  est_table <- tibble(
    term = paste(c(rep("Dim 1", k1), rep("Dim 2", k2)), c(mcat1, mcat2), sep = ": "),
    Estimate = out$par,
    `S.E.` = sqrt(diag(1/out$hessian)),
    `z-statistic` = Estimate/`S.E.`,
    `p-value` = 2 * pnorm(abs(`z-statistic`), lower.tail = FALSE)
  )

  # Adjustment proportions
  a <- out$par[1:k1]
  b <- out$par[(k1 + 1):(k1 + k2)]

  # Adjustment proportions
  target <- rep(0, times = ncol(dat[[1]]))
  target <- (colSums(dat[[1]]) -
               colSums(rep(a, each = k2) * rep(b, times = k1) * dat[[2]]) -
               colSums(rep(a, each = k2) * rep(1 - b, times = k1) * dat[[3]]) -
               colSums(rep(1 - a, each = k2) * rep(b, times = k1) * dat[[4]]))/
    sum(as.double(outer(1 - b, 1 - a)) * rowSums(dat[[5]]))

  # A table with adjustment proportions
  adj_prop <- tibble(
    status = colnames(dat[[1]]),
    adj_prop = target
  )

  # Predicted counts
  pred <- rep(a, each = k2) * rep(b, times = k1) * dat[[2]] +
    rep(a, each = k2) * rep(1 - b, times = k1) * dat[[3]] +
    rep(1 - a, each = k2) * rep(b, times = k1) * dat[[4]] +
    rep(1 - a, each = k2) * rep(1 - b, times = k1) * outer(rowSums(dat[[5]]), target)

  # Goodness of fit
  delta <- sum(abs(pred - dat[[1]]))/(2 * sum(dat[[1]]))

  # Distance
  distance <- out$value

  list("Mixing coefficient" = est_table, "Predicted counts" = pred,
       "Dissimilarity index" = delta,
       "Distance (Frobenius)" = norm(dat[[1]] - pred, type = "F")/sum(dat[[1]]),
       "Adjustment proportions" = target)
}

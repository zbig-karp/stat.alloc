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
#' t1 <- refall2d(d = ks1985, dim1 = "degree", dim2 = "sex", status = "status", f = "freq")
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
#' t1 <- refall2d(d = ks1985, dim1 = "degree", dim2 = "sex", status = "status", f = "freq")
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
#' t1 <- refall2d(d = ks1985, dim1 = "degree", dim2 = "sex", status = "status", f = "freq")
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
#' t1 <- refall2d(d = ks1985, dim1 = "degree", dim2 = "sex", status = "status", f = "freq")
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

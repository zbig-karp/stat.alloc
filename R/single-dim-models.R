#' Constant mixing model with the minimum-distance estimation routine
#'
#' The function estimates a mixing coefficient in the constant mixing model by minimising the distance between observed and model-predicted status allocations. `cmm` stands for "constant mixing model", while `mde` stands for "minimum distance estimation".
#'
#' @param dat an object returned by `stat.alloc::mar()`, or a list with the following three elements: the observed status allocation, the meritocratic status allocation, and the lottery allocation.
#'
#' @return a list of three elements:
#' - a data frame with the estimated mixing coefficient, its standard error and test statistics
#' - a matrix representing the model-predicted status allocation table
#' - the index of dissimilarity between observed and model-predicted status allocation tables. It is used as the model's goodness of fit measure.
#' @export
#'
#' @examples
#' ks1985 <- matrix(c(135, 52, 58, 12, 6, 27, 56, 126, 26, 7, 13, 51, 183, 98, 65, 3, 8, 30, 23, 2), ncol = 4, dimnames = list(rownames = paste0("E", 1:5), colnames = paste0("O", 1:4)))
#' ks1985_mar <- mar(x = ks1985)
#' cmm_mde(ks1985_mar)

cmm_mde <- function(dat) {

  # Function which returns the distance between the observed and the model-predicted allocations
  mc <- function(dat, alpha) {
    aprime <- alpha * dat$Meritocratic + (1 - alpha) * dat$Lottery
    norm(x = dat$Actual - aprime, type = "F")^2
  }

  # Minimising the distance between the observed and the model-predicted allocations
  out <- optim(par = 0.5,
               fn = mc,
               dat = mar(ks1985),
               method = "L-BFGS-B", lower = 0 + 1e-5, upper = 1 - 1e-5,
               hessian = TRUE)

  est_table <- tibble::tibble(
    Estimate = out$par,
    `S.E.` = sqrt(1/out$hessian[1, 1]),
    `z test` = Estimate/`S.E.`,
    `p value` = cut(pnorm(`z test`, lower.tail = FALSE),
                    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "< 0.1", "n.s.")))

  aprime <- out$par * dat$Meritocratic + (1 - out$par) * dat$Lottery

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Model-predicted allocation` = aprime,
       `Dissimilarity index` = sum(abs(prop.table(dat$Actual) - prop.table(aprime)))/2)
}

#' Constant mixing model with maximum-likelihood estimation routine
#'
#' The function estimates a mixing coefficient in the constant mixing model by means of maximum likelihood. `cmm` stands for "constant mixing model", while `mde` stands for "minimum distance estimation".
#'
#' @param dat an object returned by `stat.alloc::mar()`, or a list with the following three elements: the observed status allocation, the meritocratic status allocation, and the lottery allocation.
#'
#' @return a list of three elements:
#' - a data frame with the estimated mixing coefficient, its standard error and test statistics
#' - a matrix representing the model-predicted status allocation table
#' - the index of dissimilarity between observed and model-predicted status allocation tables. It is used as the model's goodness of fit measure.
#' @export
#'
#' @examples
#' ks1985 <- matrix(c(135, 52, 58, 12, 6, 27, 56, 126, 26, 7, 13, 51, 183, 98, 65, 3, 8, 30, 23, 2), ncol = 4, dimnames = list(rownames = paste0("E", 1:5), colnames = paste0("O", 1:4)))
#' ks1985_mar <- mar(x = ks1985)
#' cmm_mle(ks1985_mar)

cmm_mle <- function(dat) {

  # Function which returns the log-likelihood for the compound matrix
  mc <- function(dat, alpha) {
    aprime <- (alpha * dat$Meritocratic + (1 - alpha) * dat$Lottery)/sum(dat$Actual)

    fake <- 99999
    ll <- -sum(dat$Actual * log(aprime))

    if (is.finite(ll)) {
      fake <- ll
    } else {
      ll <- fake
    }
    ll
  }

  # Maximising the likelihood with respect to the mixing coefficient
  out <- optim(par = 0.5,
               fn = mc,
               dat = dat,
               method = "L-BFGS-B", lower = 0, upper = 1,
               hessian = TRUE)

  est_table <- tibble::tibble(
    Estimate = out$par,
    `S.E.` = sqrt(1/out$hessian[1, 1]),
    `z test` = Estimate/`S.E.`,
    `p value` = cut(pnorm(`z test`, lower.tail = F),
                    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "< 0.1", "n.s."))
  )

  aprime <- out$par * dat$Meritocratic + (1 - out$par) * dat$Lottery

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Model-predicted allocation` = aprime,
       `Dissimilarity index` = sum(abs(prop.table(dat$Actual) - prop.table(aprime)))/2)
}

#' Differential mixing model with the minimum-distance estimation routine
#'
#' The function estimates mixing coefficients in the differential mixing model by minimising the distance between observed and model-predicted status allocations. `dmm` stands for "differential mixing model", while `mde` stands for "minimum distance estimation".
#'
#' @param dat an object returned by `stat.alloc::mar()`, or a list with the following three elements: the observed status allocation, the meritocratic status allocation, and the lottery allocation.
#'
#' @return a list of four elements:
#' - a data frame with the estimated mixing coefficients, one coefficient per origin category, their standard error and associated test statistics
#' - a named vector of **adjustment proportions**, or conditional probabilities that an individual ends up in destination status \eqn{k} **given that** the allocation is driven by lottery. Adjustment proportions are necessary to fit column marginals of the status allocation matrix
#' - a matrix representing the model-predicted status allocation table
#' - the index of dissimilarity between observed and model-predicted status allocation tables. It is used as the model's goodness of fit measure.
#' @export
#'
#' @examples
#' ks1985 <- matrix(c(135, 52, 58, 12, 6, 27, 56, 126, 26, 7, 13, 51, 183, 98, 65, 3, 8, 30, 23, 2), ncol = 4, dimnames = list(rownames = paste0("E", 1:5), colnames = paste0("O", 1:4)))
#' ks1985_mar <- mar(x = ks1985)
#' dmm_mde(ks1985_mar)

dmm_mde <- function(dat) {

  # Function which returns the distance between the observed and the model-predicted allocations
  mc <- function(dat, alpha) {
    # Row and column marginals
    row_mrg <- rowSums(dat$Actual)
    col_mrg <- colSums(dat$Actual)

    # "Target" proportions
    target <- vector(mode = "double", length = length(col_mrg))
    for (i in 1:length(target)) {
      target[i] <- (col_mrg[i] - sum(alpha * dat$Meritocratic[, i]))/sum((1 - alpha) * row_mrg)
    }

    # "Compound" allocation
    aprime <- dat$Actual
    for (i in 1:nrow(aprime)) {
      for (j in 1:ncol(aprime))
        aprime[i, j] <- (alpha[i] * dat$Meritocratic[i, j] + (1 - alpha[i]) * target[j] * row_mrg[i])
    }

    fake <- 99999
    ll <- norm(x = dat$Actual - aprime, type = "F")^2

    if (is.finite(ll)) {
      fake <- ll
    } else {
      ll <- fake
    }
    ll
  }

  # Minimising the the distance between the observed and the model-predicted allocations
  out <- optim(par = rep(0.9, times = nrow(dat$Actual)),
               fn = mc,
               dat = dat,
               method = "L-BFGS-B",
               lower = rep(0, times = nrow(dat$Actual)),
               upper = rep(1, times = nrow(dat$Actual)),
               hessian = TRUE)

  est_table <- tibble::tibble(
    Estimate = out$par,
    `S.E.` =  ifelse(diag(out$hessian) <= 0, NA, sqrt(diag(solve(out$hessian)))),
    `z test` = Estimate/`S.E.`,
    `p value` = cut(pnorm(`z test`, lower.tail = F),
                    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "< 0.1", "n.s.")))

  # Calculating expected counts
  row_mrg <- rowSums(dat$Actual)
  col_mrg <- colSums(dat$Actual)
  alpha <- est_table$Estimate

  target <- vector(mode = "double", length = length(col_mrg))
  for (i in 1:length(target)) {
    target[i] <- (col_mrg[i] - sum(alpha * dat$Meritocratic[, i]))/sum((1 - alpha) * row_mrg)
  }
  names(target) <- colnames(dat[[1]])

  aprime <- dat$Actual
  for (i in 1:nrow(aprime)) {
    for (j in 1:ncol(aprime))
      aprime[i, j] <- alpha[i] * dat$Meritocratic[i, j] + (1 - alpha[i]) * target[j] * row_mrg[i]
  }

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Adjustment proportions` = target,
       `Model-predicted allocation` = aprime,
       `Dissimilarity index` = sum(abs(prop.table(dat$Actual) - prop.table(aprime)))/2)
}

#' Differential mixing model with the maximum-likelihood estimation routine
#'
#' The function estimates mixing coefficients in the differential mixing model by means of maximum likelihood. `dmm` stands for "differential mixing model", while `mle` stands for "maximum likelihood estimation".
#'
#' @param dat an object returned by `stat.alloc::mar()`, or a list with the following three elements: the observed status allocation, the meritocratic status allocation, and the lottery allocation.
#'
#' @return a list of four elements:
#' - a data frame with the estimated mixing coefficients, one coefficient per origin category, their standard error and associated test statistics
#' - a named vector of **adjustment proportions**, or conditional probabilities that an individual ends up in destination status \eqn{k} **given that** the allocation is driven by lottery. Adjustment proportions are necessary to fit column marginals of the status allocation matrix
#' - a matrix representing the model-predicted status allocation table
#' - the index of dissimilarity between observed and model-predicted status allocation tables. It is used as the model's goodness of fit measure.
#' @export
#'
#' @examples
#' ks1985 <- matrix(c(135, 52, 58, 12, 6, 27, 56, 126, 26, 7, 13, 51, 183, 98, 65, 3, 8, 30, 23, 2), ncol = 4, dimnames = list(rownames = paste0("E", 1:5), colnames = paste0("O", 1:4)))
#' ks1985_mar <- mar(x = ks1985)
#' dmm_mle(ks1985_mar)

dmm_mle <- function(dat) {

  # Function which returns the log-likelihood for the compound matrix
  mc <- function(dat, alpha) {
    # Row and column marginals
    row_mrg <- rowSums(dat$Actual)
    col_mrg <- colSums(dat$Actual)

    # "Target" proportions
    target <- vector(mode = "double", length = length(col_mrg))
    for (i in 1:length(target)) {
      target[i] <- (col_mrg[i] - sum(alpha * dat$Meritocratic[, i]))/sum((1 - alpha) * row_mrg)
    }

    # "Compound" allocation
    aprime <- dat$Actual
    for (i in 1:nrow(aprime)) {
      for (j in 1:ncol(aprime))
        aprime[i, j] <- (alpha[i] * dat$Meritocratic[i, j] + (1 - alpha[i]) * target[j] * row_mrg[i])/sum(dat$Actual)
    }

    fake <- 99999
    ll <- -sum(dat$Actual * log(aprime))

    if (is.finite(ll)) {
      fake <- ll
    } else {
      ll <- fake
    }
    ll
  }

  # Maximising the likelihood with respect to the mixing coefficient
  out <- optim(par = rep(0.5, times = nrow(dat$Actual)),
               fn = mc,
               dat = dat,
               method = "L-BFGS-B",
               lower = rep(0, times = nrow(dat$Actual)),
               upper = rep(1, times = nrow(dat$Actual)),
               hessian = TRUE)

  est_table <- tibble::tibble(
    Estimate = out$par,
    `S.E.` =  ifelse(diag(out$hessian) <= 0, NA, sqrt(diag(solve(out$hessian)))),
    `z test` = Estimate/`S.E.`,
    `p value` = cut(pnorm(`z test`, lower.tail = FALSE),
                    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "< 0.1", "n.s."))
  )

  # Calculating expected counts
  row_mrg <- rowSums(dat$Actual)
  col_mrg <- colSums(dat$Actual)
  alpha <- est_table$Estimate

  target <- vector(mode = "double", length = length(col_mrg))
  for (i in 1:length(target)) {
    target[i] <- (col_mrg[i] - sum(alpha * dat$Meritocratic[, i]))/sum((1 - alpha) * row_mrg)
  }
  names(target) <- colnames(dat[[1]])

  aprime <- dat$Actual
  for (i in 1:nrow(aprime)) {
    for (j in 1:ncol(aprime))
      aprime[i, j] <- alpha[i] * dat$Meritocratic[i, j] + (1 - alpha[i]) * target[j] * row_mrg[i]
  }

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Adjustment proportions` = target,
       `Model-predicted allocation` = aprime,
       `Dissimilarity index` = sum(abs(prop.table(dat$Actual) - prop.table(aprime)))/2)
}

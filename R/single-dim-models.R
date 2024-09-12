# Constant mixing model with the estimation routine which minimises the distance between observed and model-predicted status allocations. `cmm` stands for "constant mixing model", while `mde` stands for "minimum distance estimation".

#' Title
#'
#' @param dat an object returned by stat.alloc::mar(), or a list with the following three elements: the observed status allocation, the meritocratic status allocation, and the lottery allocation.
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

  est_table <- data.frame(
    Estimate = out$par,
    `S.E.` = sqrt(1/out$hessian[1, 1])
  )
  est_table$`z test` <- with(est_table, Estimate/`S.E.`)
  est_table$`p value` <- with(est_table, pnorm(`z test`, lower.tail = FALSE))
  est_table$`p value` <- cut(est_table$`p value`,
                             breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                             labels = c("< 0.001", "< 0.0$", "< 0.05", "< 0.1", "n.s."))

  aprime <- out$par * dat$Meritocratic + (1 - out$par) * dat$Lottery

  list(`Mixing coefficient` = as.data.frame(est_table),
       `Model-predicted allocation` = aprime,
       `Dissimilarity index` = sum(abs(prop.table(dat$Actual) - prop.table(aprime)))/2)
}

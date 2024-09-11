test_that("mar() returns a list of 3 matrices", {
  a <- matrix(c(135, 52, 58, 12, 6, 27, 56, 126, 26, 7, 13, 51, 183, 98, 65, 3, 8, 30, 23, 21),
              ncol = 4, dimnames = list(rownames = paste0("$E_", 1:5, "$"),
                                        colnames = paste0("$O_", 1:4, "$")))
  b <- rowSums(a) %o% colSums(a)/sum(a)
  expect_equal(mar(a)[[2]],
               matrix(c(178, 85, 0, 0, 0, 0, 82, 160, 0, 0, 0, 0, 237, 159, 14,
                        0, 0, 0, 0, 85), ncol = 4,
                      dimnames = list(rownames = paste0("$E_", 1:5, "$"),
                                      colnames = paste0("$O_", 1:4, "$"))))
})

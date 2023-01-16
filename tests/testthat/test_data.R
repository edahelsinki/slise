context("Tests for the data functions")
source("setup.R")

test_that("Check data preprocessing", {
    for (i in c(rep(c(4, 8), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        for (i in 0:7) {
            expect_equal(
                c(data$X),
                c(remove_intercept_column(add_intercept_column(data$X)))
            )
            X <- data$X
            X[, 3] <- 0
            X2 <- remove_constant_columns(X)
            expect_equal(c(X[, -3]), c(X2))
            expect_equal(attr(X2, "constant_columns"), 3)
            X3 <- scale_robust(X2)
            expect_equal(X2, unscale(X3))
            expect_equal(X3, scale_same(data$X, X3))
            expect_equal(c(scale_same(X[4, ], X3)), c(X3[4, ]))
            expect_equal(c(scale_same(X[1:3, ], X3)), c(X3[1:3, ]))
            Y2 <- scale_robust(data$Y)
            expect_equal(data$Y, unscale(Y2))
            ols1 <- .lm.fit(add_intercept_column(X3), Y2)$coefficients
            ols2 <- .lm.fit(add_intercept_column(X2), data$Y)$coefficients
            ols3 <- unscale_alpha(ols1, X3, Y2)
            expect_equal(ols2, ols3)
        }
    }
})

test_that("Check simple_pca", {
    for (i in c(rep(c(4, 8), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        pca1 <- simple_pca(data$X, ceiling(i * 0.3))
        pca2 <- prcomp(data$X, center = FALSE, scale = FALSE, rank = ceiling(i * 0.3))$rotation
        expect_equal(c(pca1), c(pca2))
        pca3 <- simple_pca(data$X, i)
        X2 <- (data$X %*% pca3) %*% t(pca3)
        expect_equal(X2, data$X)
    }
})
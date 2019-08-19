# tests for the slise algorithm

test_that("Check SLISE", {
    for (i in c(rep(c(4, 8, 16), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        slise1 <- slise.fit(data$X, data$Y, epsilon=0.1, lambda=0)
        slise2 <- slise.fit(data$X, data$Y, epsilon=0.1, lambda=0.1)
        data2 <- slise1$scaled
        ols <- .lm.fit(data2$X, data2$Y)$coefficients
        slise1_loss <- slise1$value
        slise2_loss <- slise2$value
        ols1_loss <- loss_sharp(ols, data2$X, data2$Y, epsilon=0.1, lambda=0)
        ols2_loss <- loss_sharp(ols, data2$X, data2$Y, epsilon=0.1, lambda=0.1)
        expect_lt(slise1_loss, ols1_loss)
        expect_lt(slise2_loss, ols2_loss)
        x <- rnorm(length(data$alpha)-1)
        y <- sum(x * data$alpha[-1]) + data$alpha[[1]]
        expl1_alpha <- slise.explain(data$X, data$Y, x, y, 0.03, 0)$coefficients
        expl2_alpha <- slise.explain(data$X, data$Y, x, y, 0.03, 0.1)$coefficients
        expect_equal(sum(expl1_alpha[-1] * x) + expl1_alpha[[1]], y)
        expect_equal(sum(expl2_alpha[-1] * x) + expl2_alpha[[1]], y)
    }
})

test_that("Check SLISE find", {
    for (i in c(rep(c(4, 8), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        x <- data$X[1,]
        y <- data$clean[1]
        expl1_alpha <- slise.explain_find(data$X, data$Y, x, y, 0.03, lambda=0, variables=i/2)$coefficients
        expl2_alpha <- slise.explain_find(data$X, data$Y, x, y, 0.03, lambda=0.1, variables=i/2)$coefficients
        expect_equal(sum(expl1_alpha[-1] * x) + expl1_alpha[[1]], y)
        expect_equal(sum(expl2_alpha[-1] * x) + expl2_alpha[[1]], y)
    }
})

test_that("Check SLISE comb", {
    for (i in c(rep(5, 3))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        x <- data$X[1,]
        y <- data$clean[1]
        expl1 <- slise.explain_comb(data$X, data$Y, x, y, 0.03, variables=3)
        expl2 <- slise.explain_comb(data$X, data$Y, x, y, 0.10, variables=3)
        expect_equal(predict(expl1, x), y)
        expect_equal(predict(expl2, x), y)
        expect_gte(3, sparsity(expl1$alpha[-1]))
        expect_gte(3, sparsity(expl2$alpha[-1]))
    }
})

test_that("Check SLISE predict", {
    for (i in c(rep(c(4, 8), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        x <- data$X[1,]
        y <- data$clean[1]
        expl1 <- slise.explain(data$X, data$Y, x, y, scale=TRUE)
        expl2 <- slise.explain(data$X, sigmoid(data$Y), x, sigmoid(y), logit=TRUE)
        expl3 <- slise.fit(data$X, data$Y, lambda=0.1)
        Y1 <- predict(expl1, data$X)
        Y2 <- predict(expl2, data$X)
        Y3 <- predict(expl3, data$X)
        expect_equal(Y1[1], y)
        expect_equal(Y2[1], sigmoid(y))
    }
})

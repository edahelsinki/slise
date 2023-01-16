context("Tests for the slise algorithm")
source("setup.R")

test_that("Check SLISE", {
    for (i in c(rep(c(4, 8, 16), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        slise1 <- slise.fit(data$X, data$Y, epsilon = 0.1, lambda1 = 0)
        slise2 <- slise.fit(data$X, data$Y, epsilon = 0.1, lambda1 = 0.1)
        ols <- .lm.fit(data$X, data$Y)$coefficients
        slise1_loss <- slise1$value
        slise2_loss <- slise2$value
        ols1_loss <- loss_sharp(ols, data$X, data$Y, epsilon = 0.1, lambda1 = 0)
        ols2_loss <- loss_sharp(ols, data$X, data$Y, epsilon = 0.1, lambda1 = 0.1)
        expect_lt(slise1_loss, ols1_loss)
        expect_lt(slise2_loss, ols2_loss)
        x <- rnorm(length(data$alpha) - 1)
        y <- sum(x * data$alpha[-1]) + data$alpha[[1]]
        expl1_alpha <- slise.explain(data$X, data$Y, 0.03, x, y, 0)$coefficients
        expl2_alpha <- slise.explain(data$X, data$Y, 0.03, x, y, 0.1)$coefficients
        expect_equal(sum(expl1_alpha[-1] * x) + expl1_alpha[[1]], y)
        expect_equal(sum(expl2_alpha[-1] * x) + expl2_alpha[[1]], y)
    }
})

test_that("Check SLISE formula", {
    data <- data.frame(y = rnorm(8), a = rnorm(8), b = rnorm(8))
    model <- slise.formula(y ~ a * b + abs(a), data, 0.1, normalise = TRUE)
    expect_equal(length(model$coefficients), 5)
    expect_equal(length(model$normalised_coefficients), 5)
    expect_true(model$intercept)
    model <- slise.formula(y ~ a * b + abs(a) + 0, data, 0.1, normalise = TRUE)
    expect_equal(length(model$coefficients), 5)
    expect_equal(length(model$normalised_coefficients), 4)
    expect_true(model$intercept)
    model <- slise.formula(y ~ a * b + abs(a) + 0, data, 0.1, normalise = FALSE, lambda1 = 0.1)
    expect_equal(length(model$coefficients), 4)
    expect_false(model$intercept)
    model <- slise.formula(y ~ poly(a, b, degree = 2), data, 0.1, normalise = FALSE, lambda1 = 0.1)
    expect_equal(length(model$coefficients), 6)
    expect_true(model$intercept)
})

test_that("Check SLISE comb", {
    for (i in c(rep(5, 3))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        x <- data$X[1, ]
        y <- data$clean[1]
        expl1 <- slise.explain_comb(data$X, data$Y, 0.03, x, y, variables = 3)
        expl2 <- slise.explain_comb(data$X, data$Y, 0.10, x, y, variables = 3)
        expect_equal(predict(expl1, x)[[1]], y)
        expect_equal(predict(expl2, x)[[1]], y)
        expect_gte(3, sparsity(expl1$alpha[-1]))
        expect_gte(3, sparsity(expl2$alpha[-1]))
    }
})

test_that("Check SLISE predict", {
    for (i in c(rep(c(4, 8), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        Y2 <- data$Y - min(data$Y)
        Y2 <- Y2 / max(Y2)
        expl1 <- slise.explain(data$X, data$Y, 0.2, data$X[1, ], data$clean[1])
        expl2 <- slise.explain(data$X, Y2, 0.3, data$X[1, ], Y2[1], logit = TRUE)
        expl3 <- slise.explain(data$X, Y2, 0.3, 2, logit = TRUE, normalise = TRUE)
        fit1 <- slise.fit(data$X, data$Y, 0.1, 0.1)
        Y1 <- predict(expl1, data$X)
        Y2b <- predict(expl2, data$X)
        Y2c <- predict(expl3, data$X[2, ])
        Y3 <- predict(fit1, data$X)
        expect_equal(Y1[1], data$clean[1])
        expect_equal(Y2[2], c(Y2c))
        expect_equal(expl1$subset, c(abs(predict(expl1, data$X) - data$Y) < expl1$epsilon))
        expect_equal(expl2$subset, c(abs(predict(expl2, data$X, logit = TRUE) - expl2$Y) < expl2$epsilon))
        expect_equal(expl3$subset, c(abs(predict(expl3, data$X, logit = TRUE) - expl3$Y) < expl3$epsilon))
        expect_equal(fit1$subset, c(abs(predict(fit1, data$X) - data$Y) < fit1$epsilon))
    }
})

test_that("Check SLISE unscale", {
    for (i in c(rep(c(4, 8, 16), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.3), 0.03, 0.3, 0.3)
        slise1 <- slise.fit(data$X, data$Y, epsilon = 0.1, normalise = TRUE)
        data2 <- slise.preprocess(data$X, data$Y, 0.1, intercept = TRUE, normalise = TRUE)
        subset1 <- mean(abs(predict(slise1, data$X) - data$Y) <= slise1$epsilon)
        subset2 <- mean(abs(data2$X %*% slise1$normalised_alpha - data2$Y) < slise1$normalised_epsilon)
        expect_equal(subset1, mean(slise1$subset))
        expect_equal(subset2, mean(slise1$subset))
    }
})
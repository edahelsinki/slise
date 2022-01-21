context("Tests for checking that the weights are implemented correctly")
source("setup.R")


test_that("Check simple losses", {
    for (j in 1:8) {
        mask <- runif(100) > 0.5
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        data2 <- list(X = rbind(data$X, data$X), Y = c(data$Y, data$Y))
        data3 <- list(X = rbind(data$X, data$X[mask, ]), Y = c(data$Y, data$Y[mask]))
        alpha <- rnorm(ncol(data$X))
        w1 <- rep(1, 100)
        w2 <- rep(2, 100)
        w3 <- mask + 1
        # Simple losses
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = w1),
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = w2),
            loss_smooth(alpha, X = data2$X, Y = data2$Y, epsilon = 0.1, beta = 3),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = w3),
            loss_smooth(alpha, X = data3$X, Y = data3$Y, epsilon = 0.1, beta = 3),
            tolerance = 1e-3
        )
    }
})

test_that("Check derivations numerically", {
    if (require(numDeriv)) {
        for (j in 1:8) {
            mask <- runif(100) > 0.5
            data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
            data2 <- list(X = rbind(data$X, data$X), Y = c(data$Y, data$Y))
            data3 <- list(X = rbind(data$X, data$X[mask, ]), Y = c(data$Y, data$Y[mask]))
            alpha <- rnorm(ncol(data$X))
            w1 <- rep(1, 100)
            w2 <- rep(2, 100)
            wr <- runif(100)
            w3 <- mask + 1
            # Numeric Derivation
            expect_equivalent(
                loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3),
                grad(loss_smooth, alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = w1),
                tolerance = 1e-3
            )
            expect_equivalent(
                loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = w2),
                grad(loss_smooth, alpha, X = data2$X, Y = data2$Y, epsilon = 0.1, beta = 3),
                tolerance = 1e-3
            )
            expect_equivalent(
                loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = wr),
                grad(loss_smooth, alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = wr),
                tolerance = 1e-3
            )
            expect_equivalent(
                loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = w3),
                grad(loss_smooth, alpha, X = data3$X, Y = data3$Y, epsilon = 0.1, beta = 3),
                tolerance = 1e-3
            )
        }
    }
})

test_that("Check that the R and C++ versions are identical", {
    for (j in 1:8) {
        mask <- runif(100) > 0.5
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        data2 <- list(X = rbind(data$X, data$X), Y = c(data$Y, data$Y))
        data3 <- list(X = rbind(data$X, data$X[mask, ]), Y = c(data$Y, data$Y[mask]))
        alpha <- rnorm(ncol(data$X))
        w1 <- rep(1, 100)
        w2 <- rep(2, 100)
        wr <- runif(100)
        w3 <- mask + 1
        # R to C++
        expect_equal(
            c(loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3)),
            c(loss_smooth_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = w1)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth(alpha, X = data2$X, Y = data2$Y, epsilon = 0.1, beta = 3)),
            c(loss_smooth_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = w2)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = wr)),
            c(loss_smooth_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = wr)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = wr)),
            c(loss_smooth_grad_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = wr)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth(alpha, X = data3$X, Y = data3$Y, epsilon = 0.1, beta = 3)),
            c(loss_smooth_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = w3)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X = data3$X, Y = data3$Y, epsilon = 0.1, beta = 3)),
            c(loss_smooth_grad_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = w3)),
            tolerance = 1e-4
        )
        # Combined
        dc <- data_container(X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = w1)
        expect_equal(
            c(loss_smooth_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight =  numeric(0))),
            c(lg_combined_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = numeric(0))),
            c(lg_getgrad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        dc$setWeight(w2)
        expect_equal(
            c(loss_smooth_c(alpha, data = data2$X, response = data2$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight =  numeric(0))),
            c(lg_combined_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad_c(alpha, data = data2$X, response = data2$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = numeric(0))),
            c(lg_getgrad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        dc$setWeight(wr)
        expect_equal(
            c(loss_smooth_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = wr)),
            c(lg_combined_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = wr)),
            c(lg_getgrad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        dc$setWeight(w3)
        expect_equal(
            c(loss_smooth_c(alpha, data = data3$X, response = data3$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight =  numeric(0))),
            c(lg_combined_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad_c(alpha, data = data3$X, response = data3$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight = numeric(0))),
            c(lg_getgrad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
    }
})

test_that("Check that OWL-QN still works", {
    for (j in 1:8) {
        mask <- runif(100) > 0.5
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        data2 <- list(X = rbind(data$X, data$X), Y = c(data$Y, data$Y))
        data3 <- list(X = rbind(data$X, data$X[mask, ]), Y = c(data$Y, data$Y[mask]))
        alpha <- rnorm(ncol(data$X))
        w1 <- rep(1, 100)
        w2 <- rep(2, 100)
        wr <- runif(100)
        w3 <- mask + 1
        # OWL-QN
        dc <- data_container(X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, weight = wr)
        expect_equal(
            owlqn_c(alpha, dc)$value,
            owlqn_r(alpha, data$X, data$Y, 0.1, 3, weight = wr)$value,
            tolerance = 1e-4
        )
        dc$setWeight(w1)
        expect_equal(
            owlqn_c(alpha, dc)$value,
            owlqn_r(alpha, data$X, data$Y, 0.1, 3)$value,
            tolerance = 1e-4
        )
        dc$setWeight(w2)
        expect_equal(
            owlqn_c(alpha, dc)$value,
            owlqn_r(alpha, data2$X, data2$Y, 0.1, 3)$value,
            tolerance = 1e-4
        )
        dc$setWeight(w3)
        expect_equal(
            owlqn_c(alpha, dc)$value,
            owlqn_r(alpha, data3$X, data3$Y, 0.1, 3)$value,
            tolerance = 1e-4
        )
    }
})

test_that("Check log_approx_ratio", {
    for (j in 1:8) {
        mask <- runif(100) > 0.5
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        data2 <- list(X = rbind(data$X, data$X), Y = c(data$Y, data$Y))
        data3 <- list(X = rbind(data$X, data$X[mask, ]), Y = c(data$Y, data$Y[mask]))
        alpha <- rnorm(ncol(data$X))
        w1 <- rep(1, 100)
        w2 <- rep(2, 100)
        w3 <- mask + 1
        # log_approximation ratio
        expect_equivalent(
            log_approximation_ratio((data$X %*% alpha - data$Y)^2, 0.01, 1, 4, w1),
            log_approximation_ratio((data$X %*% alpha - data$Y)^2, 0.01, 1, 4),
            tolerance = 1e-4
        )
        expect_equivalent(
            log_approximation_ratio((data$X %*% alpha - data$Y)^2, 0.01, 1, 4, w2),
            log_approximation_ratio((data2$X %*% alpha - data2$Y)^2, 0.01, 1, 4),
            tolerance = 1e-4
        )
        expect_equivalent(
            log_approximation_ratio((data$X %*% alpha - data$Y)^2, 0.01, 1, 4, w3),
            log_approximation_ratio((data3$X %*% alpha - data3$Y)^2, 0.01, 1, 4),
            tolerance = 1e-4
        )
    }
})

test_that("Check matching_epsilon", {
    for (j in 1:8) {
        mask <- runif(100) > 0.5
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        data2 <- list(X = rbind(data$X, data$X), Y = c(data$Y, data$Y))
        data3 <- list(X = rbind(data$X, data$X[mask, ]), Y = c(data$Y, data$Y[mask]))
        alpha <- rnorm(ncol(data$X))
        w1 <- rep(1, 100)
        w2 <- rep(2, 100)
        w3 <- mask + 1
        # log_approximation ratio
        expect_equivalent(
            matching_epsilon((data$X %*% alpha - data$Y)^2, 0.04, 4, w1),
            matching_epsilon((data$X %*% alpha - data$Y)^2, 0.04, 4),
            tolerance = 1e-4
        )
        expect_equivalent(
            matching_epsilon((data$X %*% alpha - data$Y)^2, 0.03, 2, w2),
            matching_epsilon((data2$X %*% alpha - data2$Y)^2, 0.03, 2),
            tolerance = 1e-4
        )
        expect_equivalent(
            matching_epsilon((data$X %*% alpha - data$Y)^2, 0.05, 3, w3),
            matching_epsilon((data3$X %*% alpha - data3$Y)^2, 0.05, 3),
            tolerance = 1e-4
        )
    }
})

test_that("Check the next beta", {
    for (j in 1:8) {
        mask <- runif(100) > 0.5
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        data2 <- list(X = rbind(data$X, data$X), Y = c(data$Y, data$Y))
        data3 <- list(X = rbind(data$X, data$X[mask, ]), Y = c(data$Y, data$Y[mask]))
        alpha <- rnorm(ncol(data$X))
        w1 <- rep(1, 100)
        w2 <- rep(2, 100)
        w3 <- mask + 1
        # log_approximation ratio
        expect_equivalent(
            next_beta((data$X %*% alpha - data$Y)^2, 0.04, 4, w1),
            next_beta((data$X %*% alpha - data$Y)^2, 0.04, 4),
            tolerance = 1e-4
        )
        expect_equivalent(
            next_beta((data$X %*% alpha - data$Y)^2, 0.03, 2, w2),
            next_beta((data2$X %*% alpha - data2$Y)^2, 0.03, 2),
            tolerance = 1e-4
        )
        expect_equivalent(
            next_beta((data$X %*% alpha - data$Y)^2, 0.05, 3, w3),
            next_beta((data3$X %*% alpha - data3$Y)^2, 0.05, 3),
            tolerance = 1e-4
        )
    }
})

test_that("Check that the full SLISE algorithm works", {
    for (j in 1:8) {
        mask <- runif(200) > 0.5
        data <- data_create(200, 8, 2, 0.1, 0.3, 0.3)
        data2 <- list(X = rbind(data$X, data$X), Y = c(data$Y, data$Y))
        data3 <- list(X = rbind(data$X, data$X[mask, ]), Y = c(data$Y, data$Y[mask]))
        alpha <- rnorm(ncol(data$X))
        w1 <- rep(1, 200)
        w2 <- rep(2, 200)
        w3 <- mask + 1
        init1 <- list(rep(0, ncol(data$X) + 1), 0)
        init2 <- list(rep(0, ncol(data$X)), 0)
        # test SLISE with weights
        expect_equivalent(
            slise.fit(data$X, data$Y, 0.1, initialisation = init1)$loss,
            slise.fit(data$X, data$Y, 0.1, weight = w1, initialisation = init1)$loss,
            tolerance = 1e-4
        )
        expect_equivalent(
            slise.fit(data2$X, data2$Y, 0.1, initialisation = init1)$loss,
            slise.fit(data$X, data$Y, 0.1, weight = w2, initialisation = init1)$loss,
            tolerance = 1e-4
        )
        expect_equivalent(
            slise.fit(data3$X, data3$Y, 0.1, initialisation = init1)$loss,
            slise.fit(data$X, data$Y, 0.1, weight = w3, initialisation = init1)$loss,
            tolerance = 1e-4
        )
        expect_equivalent(
            slise.explain(data$X, data$Y, 0.1, data$X[1,], data$Y[1], initialisation = init2)$loss,
            slise.explain(data$X, data$Y, 0.1, data$X[1,], data$Y[1], weight = w1, initialisation = init2)$loss,
            tolerance = 1e-4
        )
        expect_equivalent(
            slise.explain(data2$X, data2$Y, 0.1, data$X[2,], data$Y[2], initialisation = init2)$loss,
            slise.explain(data$X, data$Y, 0.1, data$X[2,], data$Y[2], weight = w2, initialisation = init2)$loss,
            tolerance = 1e-4
        )
        expect_equivalent(
            slise.explain(data3$X, data3$Y, 0.1, data$X[4,], data$Y[4], initialisation = init2)$loss,
            slise.explain(data$X, data$Y, 0.1, data$X[4,], data$Y[4], weight = w3, initialisation = init2)$loss,
            tolerance = 1e-4
        )
    }
})
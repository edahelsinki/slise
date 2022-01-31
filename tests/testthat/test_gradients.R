context("Tests for comparing R/C++ calculations")
source("setup.R")

test_that("Compare loss_smooth_grad to numerical gradient approximation", {
    if (require(numDeriv)) {
        for (j in 1:8) {
            data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
            alpha <- rnorm(ncol(data$X))
            expect_equal(
                c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0)),
                grad(loss_smooth, alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0),
                tolerance = 1e-3
            )
            expect_equal(
                c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0.1)),
                grad(loss_smooth, alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0.1),
                tolerance = 1e-3
            )
            alpha <- rnorm(ncol(data$X))
            expect_equal(
                c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0)),
                grad(loss_smooth, alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0),
                tolerance = 1e-3
            )
            expect_equal(
                c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0.1)),
                grad(loss_smooth, alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0.1),
                tolerance = 1e-3
            )
        }
    }
})


test_that("Compare loss_smooth and loss_smooth_grad to cpp", {
    for (j in 1:8) {
        # global
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        alpha <- rnorm(ncol(data$X))
        # fn
        expect_equal(
            c(loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0)),
            c(loss_smooth_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight =  numeric(0))),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0)),
            c(loss_smooth_grad_c(alpha, data = data$X, response = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0, lambda2 = 0, weight =  numeric(0))),
            tolerance = 1e-4
        )
        # dc
        dc <- data_container(X = data$X, Y = data$Y, epsilon = 0.1, beta = 3)
        expect_equal(
            c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0)),
            c(loss_smooth_grad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0)),
            c(loss_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        # combined
        expect_equal(
            c(loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0)),
            c(lg_combined_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 3, lambda1 = 0)),
            c(lg_getgrad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        # local
        alpha <- rnorm(ncol(data$X))
        # fn
        expect_equal(
            c(loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0)),
            c(loss_smooth_c(alpha, data = data$X, response = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0, lambda2 = 0, weight =  numeric(0))),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0)),
            c(loss_smooth_grad_c(alpha, data = data$X, response = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0, lambda2 = 0, weight =  numeric(0))),
            tolerance = 1e-4
        )
        # dc
        dc <- data_container(X = data$X, Y = data$Y, epsilon = 0.03, beta = 3)
        expect_equal(
            c(loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0)),
            c(loss_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0)),
            c(loss_smooth_grad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        # ptr
        expect_equal(
            c(loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0)),
            c(lg_combined_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 3, lambda1 = 0)),
            c(lg_getgrad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-4
        )
    }
})


test_that("Compare owlqn_c to owlqn_r", {
    for (j in 1:5) {
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        alpha <- rnorm(ncol(data$X))
        dc <- data_container(X = data$X, Y = data$Y, epsilon = 0.1, beta = j)
        expect_equal(
            owlqn_c(alpha, dc, 0.0)$value,
            owlqn_r(alpha, data$X, data$Y, 0.1, j, lambda1 = 0)$value,
            tolerance = 1e-4
        )
        expect_equal(
            owlqn_c(alpha, dc, 2.0)$value,
            owlqn_r(alpha, data$X, data$Y, 0.1, j, lambda1 = 2)$value,
            tolerance = 1e-4
        )
    }
})

test_that("Smooth equals sharp at infinity", {
    for (j in 1:5) {
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        alpha <- rnorm(ncol(data$X))
        weight <- runif(100)
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 1e10),
            loss_sharp(alpha, X = data$X, Y = data$Y, epsilon = 0.1),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.1, beta = 1e10, weight = NULL, lambda1 = 1, lambda2 = 1),
            loss_sharp(alpha, X = data$X, Y = data$Y, epsilon = 0.1, weight = NULL, lambda1 = 1, lambda2 = 1),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 1e10, weight = NULL, lambda1 = 1, lambda2 = 1),
            loss_sharp(alpha, X = data$X, Y = data$Y, epsilon = 0.03, weight = NULL, lambda1 = 1, lambda2 = 1),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = 0.03, beta = 1e10, weight = weight, lambda1 = 1, lambda2 = 1),
            loss_sharp(alpha, X = data$X, Y = data$Y, epsilon = 0.03, weight = weight, lambda1 = 1, lambda2 = 1),
            tolerance = 1e-3
        )
    }
})

test_that("Residual versions are identical", {
    for (j in 1:5) {
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        alpha <- rnorm(ncol(data$X))
        res2 <- (data$X %*% alpha - data$Y)^2
        eps <- 0.1
        eps2 <- eps^2
        weight <- runif(100)
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = eps, beta = 3),
            loss_smooth_res(alpha, res2, epsilon2 = eps2, beta = 3),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = eps, beta = 3, lambda1 = 1, lambda2 = 1),
            loss_smooth_res(alpha, res2, epsilon2 = eps2, beta = 3, lambda1 = 1, lambda2 = 1),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_smooth(alpha, X = data$X, Y = data$Y, epsilon = eps, beta = 3, weight = weight, lambda1 = 1, lambda2 = 1),
            loss_smooth_res(alpha, res2, epsilon2 = eps2, beta = 3, weight = weight, lambda1 = 1, lambda2 = 1),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_sharp(alpha, X = data$X, Y = data$Y, epsilon = eps),
            loss_sharp_res(alpha, res2, epsilon2 = eps2),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_sharp(alpha, X = data$X, Y = data$Y, epsilon = eps, lambda1 = 1, lambda2 = 1),
            loss_sharp_res(alpha, res2, epsilon2 = eps2, lambda1 = 1, lambda2 = 1),
            tolerance = 1e-3
        )
        expect_equivalent(
            loss_sharp(alpha, X = data$X, Y = data$Y, epsilon = eps, weight = weight, lambda1 = 1, lambda2 = 1),
            loss_sharp_res(alpha, res2, epsilon2 = eps2, weight = weight, lambda1 = 1, lambda2 = 1),
            tolerance = 1e-3
        )
    }
})
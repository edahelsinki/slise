context("Tests for comparing R/C++ calculations")
source("setup.R")

test_that("Compare loss_smooth_grad to numerical gradient approximation", {
    if (require(numDeriv)) {
        for (j in 1:8) {
            data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
            alpha <- rnorm(ncol(data$X))
            expect_equal(
                c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.1, lambda=0)),
                grad(loss_smooth, alpha, X=data$X, Y=data$Y, epsilon=0.1, lambda=0),
                tolerance = 1e-6
            )
            expect_equal(
                c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.1, lambda=0.1)),
                grad(loss_smooth, alpha, X=data$X, Y=data$Y, epsilon=0.1, lambda=0.1),
                tolerance = 1e-6
            )
            i <- sample.int(length(data$Y), 1)
            data <- data_local(data$X, data$Y, data$X[i,], data$Y[[i]])
            alpha <- rnorm(ncol(data$X))
            expect_equal(
                c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.03, lambda=0)),
                grad(loss_smooth, alpha, X=data$X, Y=data$Y, epsilon=0.03, lambda=0),
                tolerance = 1e-6
            )
            expect_equal(
                c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.03, lambda=0.1)),
                grad(loss_smooth, alpha, X=data$X, Y=data$Y, epsilon=0.03, lambda=0.1),
                tolerance = 1e-6
            )
        }
    } else {
        skip("Package numDeriv required for comparing the manual gradients to numerical gradients")
    }
})


test_that("Compare loss_smooth and loss_smooth_grad to cpp", {
    for (j in 1:8) {
        # global
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        alpha <- rnorm(ncol(data$X))
        # fn
        expect_equal(
            c(loss_smooth(alpha, X=data$X, Y=data$Y, epsilon=0.1, beta=3, lambda=0)),
            c(loss_smooth_c(alpha, data=data$X, response=data$Y, beta=3, epsilon=0.1)),
            tolerance = 1e-6
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.1, beta=3, lambda=0)),
            c(loss_smooth_grad_c(alpha, data=data$X, response=data$Y, beta=3, epsilon=0.1)),
            tolerance = 1e-6
        )
        # dc
        dc <- new(DataContainer, data = data$X, response = data$Y, beta = 3, epsilon = 0.1)
        expect_equal(
            c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.1, beta=3, lambda=0)),
            c(loss_smooth_grad_c_dc(xs=alpha, dcptr = dc$.pointer)),
            tolerance = 1e-6
        )
        expect_equal(
            c(loss_smooth(alpha, X=data$X, Y=data$Y, epsilon=0.1, beta=3, lambda=0)),
            c(loss_smooth_c_dc(xs=alpha, dcptr = dc$.pointer)),
            tolerance = 1e-6
        )
        # ptr
        expect_equal(
            c(loss_smooth(alpha, X=data$X, Y=data$Y, epsilon=0.1, beta=3, lambda=0)),
            c(lg_combined_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-6
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.1, beta=3, lambda=0)),
            c(lg_getgrad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-6
        )
        # local
        i <- sample.int(length(data$Y), 1)
        data <- data_local(data$X, data$Y, data$X[i,], data$Y[[i]])
        alpha <- rnorm(ncol(data$X))
        # fn
        expect_equal(
            c(loss_smooth(alpha, X=data$X, Y=data$Y, epsilon=0.03, lambda=0)),
            c(loss_smooth_c(alpha, data=data$X, response=data$Y, beta=3, epsilon=0.03)),
            tolerance = 1e-6
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.03, lambda=0)),
            c(loss_smooth_grad_c(alpha, data=data$X, response=data$Y, beta=3, epsilon=0.03)),
            tolerance = 1e-6
        )
        # dc
        dc <- new(DataContainer, data = data$X, response = data$Y, beta = 3, epsilon = 0.03)
        expect_equal(
            c(loss_smooth(alpha, X=data$X, Y=data$Y, epsilon=0.03, beta=3, lambda=0)),
            c(loss_smooth_c_dc(xs=alpha, dcptr = dc$.pointer)),
            tolerance = 1e-6
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.03, beta=3, lambda=0)),
            c(loss_smooth_grad_c_dc(xs=alpha, dcptr = dc$.pointer)),
            tolerance = 1e-6
        )
        # ptr
        expect_equal(
            c(loss_smooth(alpha, X=data$X, Y=data$Y, epsilon=0.03, beta=3, lambda=0)),
            c(lg_combined_smooth_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-6
        )
        expect_equal(
            c(loss_smooth_grad(alpha, X=data$X, Y=data$Y, epsilon=0.03, beta=3, lambda=0)),
            c(lg_getgrad_c_dc(xs = alpha, dcptr = dc$.pointer)),
            tolerance = 1e-6
        )
    }
})


test_that("Compare owlqn_c to owlqn_r", {
    for (j in 1:5) {
        data <- data_create(100, 6, 2, 0.1, 0.3, 0.3)
        alpha <- rnorm(ncol(data$X))
        expect_equal(
            owlqn_c(alpha, data$X, data$Y, 0.1, 0, j)$value,
            owlqn_r(alpha, data$X, data$Y, 0.1, 0, j)$value,
            tolerance = 1e-6
        )
        expect_equal(
            owlqn_c(alpha, data$X, data$Y, 0.1, 2, j)$value,
            owlqn_r(alpha, data$X, data$Y, 0.1, 2, j)$value,
            tolerance = 1e-6
        )
    }
})



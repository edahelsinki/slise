# tests for the data functions


test_that("Check data_preprocess", {
    for (i in c(rep(c(4, 8), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        for(i in 0:7) {
            sc <- i%%2 > 0
            ic <- i%%4 < 2
            lg <- i >= 4
            Y <- if(lg) sigmoid(data$Y) else data$Y
            prep <- data_preprocess(data$X, Y, sc, ic, lg)
            if (ic) {
                expect_equal(prep$X[,-1], prep$scale_x(data$X))
            } else {
                expect_equal(prep$X, prep$scale_x(data$X))
            }
            expect_equal(prep$Y[1], prep$scale_y(Y[1]))
            expect_equal(Y[1], prep$unscale_y(prep$Y[1]))
            expect_equal(data$alpha, prep$unscale_alpha(prep$scale_alpha(data$alpha)))
        }
    }
})

test_that("Check data_local", {
    for (i in c(rep(c(4, 8), 2))) {
        data <- data_create(i * 30, i, floor(i * 0.5), 0.03, 0.3, 0.3)
        local <- data_local(data$X, data$Y, sample(2:30, 1))
        expect_equal(local$X[1,], local$scale_x(data$X[1,]))
        expect_equal(local$Y[1], local$scale_y(data$Y[1]))
    }
})
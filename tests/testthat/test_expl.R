context("Tests for plotting and printing")
source("setup.R")

.test_plot <- function(expr) {
    pdf(NULL)
    tryCatch(expr, finally = { dev.off() })
    expect_true(TRUE)
}

test_that("Check print", {
    data <- data_create(300, 5, 1)
    expl <- slise.fit(data$X, data$Y)
    cap <- capture.output(print(expl, title="TEST"))[[1]]
    expect_true(startsWith(cap, "TEST"))
    expl <- slise.explain(data$X, data$Y, 3)
    cap <- capture.output(print(expl, title="TEST"))[[1]]
    expect_true(startsWith(cap, "TEST"))
})

test_that("Check plot", {
    .test_plot({
        data <- data_create(300, 5, 1)
        expl <- slise.fit(data$X, data$Y)
        cap <- plot(expl, cols = 1)
        cap <- plot(expl, cols = 1, other=list(expl))
        cap <- plot(expl, cols = c(1, 2))
        cap <- plot(expl, cols = c(1, 2), threed=TRUE)
        expl <- slise.explain(data$X, data$Y, 3)
        cap <- plot(expl, cols = 1)
        cap <- plot(expl, cols = 1, other=list(expl))
        cap <- plot(expl, cols = c(1, 2))
        cap <- plot(expl, cols = c(1, 2), threed=TRUE)
    })
})

test_that("Check explain bar", {
    .test_plot({
        data <- data_create(300, 5, 1)
        expl <- slise.explain(data$X, data$Y, 3)
        explain(expl, "bar")
    })
})

test_that("Check explain dist", {
    .test_plot({
        data <- data_create(300, 5)
        expl <- slise.fit(data$X, data$Y)
        expect_error(explain(expl, "dist"))
        expl <- slise.explain(data$X, data$Y, 3)
        explain(expl, "distribution")
    })
})

test_that("Check explain image", {
    .test_plot({
        data <- data_create(300, 9, 1, 0.1, 0.3, 0.3)
        expl <- slise.explain(data$X, data$Y, 3)
        explain(expl, "image")
    })
})
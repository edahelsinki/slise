context("Tests for plotting and printing")
source("setup.R")

.test_plot <- function(expr, ...) {
    pdf(NULL)
    tryCatch(plot(expr, ...), finally = { dev.off() })
    expect_true(TRUE)
}

test_that("Check print", {
    for (i in c(5, 21)) {
        data <- data_create(100, i, 1)
        expl <- slise.fit(data$X, data$Y, 0.1)
        cap <- capture.output(print(expl))[[1]]
        expect_equal(cap, "SLISE Regression:")
        expl <- slise.fit(data$X, data$Y, 0.1, normalise = TRUE)
        cap <- capture.output(print(expl))[[1]]
        expect_equal(cap, "SLISE Regression:")
        expl <- slise.explain(data$X, data$Y, 0.2, 3)
        cap <- capture.output(print(expl))[[1]]
        expect_equal(cap, "SLISE Explanation:")
        expl <- slise.explain(data$X, data$Y, 0.2, 3, normalise = TRUE)
        cap <- capture.output(print(expl))[[1]]
        expect_equal(cap, "SLISE Explanation:")
    }
})

test_that("Check plot 2D", {
    x <- rnorm(50)
    y <- rnorm(50)
    expl <- slise.fit(x, y, 1)
    .test_plot(expl)
    expl <- slise.fit(x, y, 0.5, normalise = TRUE)
    .test_plot(expl)
    expl <- slise.explain(x, y, 1, 3)
    .test_plot(expl)
    expl <- slise.explain(x, y, 1, 3, normalise = TRUE)
    .test_plot(expl)
})

test_that("Check plot dist", {
    data <- data_create(100, 5, 1)
    expl <- slise.fit(data$X, data$Y, 0.1)
    .test_plot(expl, "dist")
    expl <- slise.fit(data$X, data$Y, 0.1, normalise = TRUE)
    .test_plot(expl, "dist")
    expl <- slise.explain(data$X, data$Y, 0.2, 3)
    .test_plot(expl, "dist")
    expl <- slise.explain(data$X, data$Y, 0.2, 3, normalise = TRUE)
    .test_plot(expl, "dist")
})

test_that("Check plot pred", {
    data <- data_create(100, 5, 1)
    expl <- slise.fit(data$X, data$Y, 0.1)
    .test_plot(expl, "prediction")
    expl <- slise.fit(data$X, data$Y, 0.1, normalise = TRUE)
    .test_plot(expl, "prediction")
    expl <- slise.explain(data$X, data$Y, 0.2, 3)
    .test_plot(expl, "prediction")
    expl <- slise.explain(data$X, data$Y, 0.2, 3, normalise = TRUE)
    .test_plot(expl, "prediction")
})

test_that("Check plot bar", {
    data <- data_create(100, 15, 2)
    expl <- slise.fit(data$X, data$Y, 0.1)
    .test_plot(expl, "bar")
    expl <- slise.fit(data$X, data$Y, 0.1, normalise = TRUE)
    .test_plot(expl, "bar")
    expl <- slise.explain(data$X, data$Y, 0.2, 3)
    .test_plot(expl, "bar")
    expl <- slise.explain(data$X, data$Y, 0.2, 3, normalise = TRUE)
    .test_plot(expl, "bar")
})

test_that("Check plot wordcloud", {
    data <- data_create(100, 15, 2)
    expl <- slise.fit(data$X, data$Y, 0.1)
    .test_plot(expl, "wordcloud", local = FALSE)
    .test_plot(expl, "wordcloud", local = TRUE)
    expl <- slise.fit(data$X, data$Y, 0.1, normalise = TRUE)
    .test_plot(expl, "wordcloud", local = FALSE)
    .test_plot(expl, "wordcloud", local = TRUE)
    expl <- slise.explain(data$X, data$Y, 0.2, 3)
    .test_plot(expl, "wordcloud", local = FALSE)
    .test_plot(expl, "wordcloud", local = TRUE)
    expl <- slise.explain(data$X, data$Y, 0.2, 3, normalise = TRUE)
    .test_plot(expl, "wordcloud", local = FALSE)
    .test_plot(expl, "wordcloud", local = TRUE)
})

test_that("Check plot image", {
    data <- data_create(100, 16, 2)
    data$X <- data$X[, c(sapply(1:4, function(i) rep((i - 1) * 4 + rep(1:4, each = 4), 4) ))]
    expl <- slise.explain(data$X, data$Y, 0.2, 3)
    .test_plot(plot(expl, type = "mnist", plots = 1))
    .test_plot(plot(expl, type = "mnist", plots = 2))
    .test_plot(plot(expl, type = "mnist", plots = 3))
})

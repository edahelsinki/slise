# This script contains the optimisations for SLISE (Graduated Optimisation and OWL-QN)

#' Smooth Loss
#' A loss function for when you want gradients
#'
#' @param alpha The vector to calculate loss for
#' @param X The data matrix
#' @param Y The response vector
#' @param epsilon The acceptable error
#' @param beta The steepness of the sigmoid
#' @param lambda1 The L1 regulariser (default: 0)
#' @param lambda2 The L2 regulariser (default: 0)
#' @param weight weight vector (default: NULL)
#'
#' @return The loss value
#'
loss_smooth <- function(alpha, X, Y, epsilon, beta, lambda1 = 0, lambda2 = 0, weight = NULL) {
    epsilon <- epsilon^2
    distances <- c(X %*% alpha - Y)^2
    subsize <- sigmoidc(beta * (epsilon - distances))
    if (is.null(weight)) {
        loss <- pmin(0, distances - epsilon * length(Y)) # phi(x) ~ pmin(0, x)
        loss <- sum(subsize * loss) / length(Y)
    } else {
        len <- sum(weight)
        loss <- pmin(0, distances - epsilon * len)
        loss <- sum(subsize * loss * weight) / len
    }
    if (lambda1 > 0) {
        loss <- loss + lambda1 * sum(abs(alpha))
    }
    if (lambda2 > 0) {
        loss <- loss + lambda2 * sum(alpha^2)
    }
    loss
}

#' Smooth Loss
#' A loss function for when you want gradients and the residuals are already calculated
#'
#' @param alpha The vector to calculate loss for
#' @param residuals2 Vector of squared residuals
#' @param epsilon2 The squared acceptable error
#' @param beta The steepness of the sigmoid
#' @param lambda1 The L1 regulariser (default: 0)
#' @param lambda2 The L2 regulariser (default: 0)
#' @param weight weight vector (default: NULL)
#'
#' @return The loss value
#'
loss_smooth_res <- function(alpha, residuals2, epsilon2, beta, lambda1 = 0, lambda2 = 0, weight = NULL) {
    subsize <- sigmoidc(beta * (epsilon2 - residuals2))
    if (is.null(weight)) {
        loss <- pmin(0, residuals2 - epsilon2 * length(residuals2)) # phi(x) ~ pmin(0, x)
        loss <- sum(subsize * loss) / length(residuals2)
    } else {
        len <- sum(weight)
        loss <- pmin(0, residuals2 - epsilon2 * len)
        loss <- sum(subsize * loss * weight) / len
    }
    if (lambda1 > 0) {
        loss <- loss + lambda1 * sum(abs(alpha))
    }
    if (lambda2 > 0) {
        loss <- loss + lambda2 * sum(alpha^2)
    }
    loss
}

#' Smooth Loss Gradient
#' Gradient for the smooth loss function
#'
#' @param alpha The vector to calculate loss-gradient for
#' @param X The data matrix
#' @param Y The response vector
#' @param epsilon The acceptable error
#' @param beta The steepness of the sigmoid
#' @param lambda1 The L1 regulariser (default: 0)
#' @param lambda2 The L2 regulariser (default: 0)
#' @param weight weight vector (default: NULL)
#'
#' @return The gradients for alpha
#'
loss_smooth_grad <- function(alpha, X, Y, epsilon, beta, lambda1 = 0, lambda2 = 0, weight = NULL) {
    epsilon <- epsilon^2
    distances <- c(X %*% alpha - Y)
    distances2 <- distances^2
    len <- if (is.null(weight)) {
        length(Y)
    } else {
        sum(weight)
    }

    f <- distances2 - epsilon * len
    s <- sigmoidc(beta * (epsilon - distances2))
    k1 <- 2 / len
    k2 <- (-2 * beta / len) * (s - s^2)
    distances <- ifelse(f < 0, distances, 0) # phi(x) ~ pmin(0, x)

    if (length(weight) > 1) {
        grad <- (t((distances * weight) * X) %*% ((s * k1) + (f * k2)))
    } else if (length(weight) == 1 && weight != 0) {
        grad <- (t(distances * X) %*% ((s * k1) + (f * k2))) * weight
    } else {
        grad <- (t(distances * X) %*% ((s * k1) + (f * k2)))
    }

    if (lambda1 > 0) {
        grad <- grad + lambda1 * sign(alpha)
    }
    if (lambda2 > 0) {
        grad <- grad + (lambda2 * 2) * alpha
    }
    grad
}


#' Sharp Loss Function
#' Exact loss function without gradients
#'
#' @param alpha The vector to calculate loss for
#' @param X The data matrix
#' @param Y The response vector
#' @param epsilon The acceptable error
#' @param lambda1 The L1 regulariser (default: 0)
#' @param lambda2 The L2 regulariser (default: 0)
#' @param weight weight vector (default: NULL)
#'
#' @return The loss value
#'
loss_sharp <- function(alpha, X, Y, epsilon, lambda1 = 0, lambda2 = 0, weight = NULL) {
    epsilon <- epsilon^2
    distances <- (X %*% alpha - Y)^2
    subsize <- distances <= epsilon
    if (is.null(weight)) {
        loss <- sum(subsize * (distances - epsilon * length(Y))) / length(Y)
    } else {
        len <- sum(weight)
        loss <- sum(subsize * (distances - epsilon * len) * weight) / len
    }
    if (lambda1 > 0) {
        loss <- loss + lambda1 * sum(abs(alpha))
    }
    if (lambda2 > 0) {
        loss <- loss + lambda2 * sum(alpha^2)
    }
    loss
}

#' Sharp Loss Function
#' Exact loss function without gradients for when the residuals are already calculated
#'
#' @param alpha The vector to calculate loss for
#' @param residuals2 The squared error vector: (X \%*\% alpha - Y)^2
#' @param epsilon2 The squared acceptable error
#' @param lambda1 The L1 regulariser (default: 0)
#' @param lambda2 The L2 regulariser (default: 0)
#' @param weight weight vector (default: NULL)
#'
#' @return The loss value
#'
loss_sharp_res <- function(alpha, residuals2, epsilon2, lambda1 = 0, lambda2 = 0, weight = NULL) {
    subsize <- residuals2 <= epsilon2
    if (is.null(weight)) {
        len <- length(residuals2)
        loss <- sum(subsize * (residuals2 - epsilon2 * len)) / len
    } else {
        len <- sum(weight)
        loss <- sum(subsize * (residuals2 - epsilon2 * len) * weight) / len
    }
    if (lambda1 > 0) {
        loss <- loss + lambda1 * sum(abs(alpha))
    }
    if (lambda2 > 0) {
        loss <- loss + lambda2 * sum(alpha^2)
    }
    loss
}

#' Wrapper for creating a C++ DataContainer that parses parameter names
#'
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param beta sigmoid steepness
#' @param weight weight vector (default: NULL)
#' @param lambda1 L1 regularisation (default: 0)
#' @param lambda2 L2 regularisation (default: 0)
#'
#' @return DataContainer
#'
data_container <- function(X, Y, epsilon, beta, lambda1 = 0.0, lambda2 = 0.0, weight = NULL) {
    if (is.null(weight)) {
        methods::new(DataContainer, data = X, response = Y, epsilon = epsilon, beta = beta, lambda1 = lambda1, lambda2 = lambda2)
    } else {
        methods::new(DataContainer, data = X, response = Y, epsilon = epsilon, beta = beta, lambda1 = lambda1, lambda2 = lambda2, weight = weight)
    }
}

#' OWL-QN for optimising loss_smooth
#' Cpp implementation
#'
#' @param alpha linear model to optimise
#' @param dc DataContainer containing the data and parameters
#' @param lambda1 L1 coefficient (default: 0)
#' @param max_iterations number of OWL-QN iterations (default: 300)
#' @param ... other parameters to OWL-QN
#' @param invisible no terminal output (default: TRUE)
#'
#' @return lbfgs object
#'
owlqn_c <- function(alpha, dc, lambda1 = 0, max_iterations = 300, ..., invisible = TRUE) {
    lbfgs::lbfgs(loss_smooth_c_ptr(), loss_smooth_grad_c_ptr(), alpha, dc$.pointer, ...,
        max_iterations = max_iterations, invisible = invisible, orthantwise_c = lambda1
    )
}

#' OWL-QN for optimising loss_smooth
#' R implementation
#'
#' @param alpha linear model to optimise
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param beta sigmoid steepness
#' @param lambda1 L1 coefficient (default: 0)
#' @param lambda2 L1 coefficient(default: 0)
#' @param weight weight vector (default: NULL)
#' @param max_iterations number of OWL-QN iterations (default: 300)
#' @param ... other parameters to OWL-QN
#' @param invisible no terminal output (default: TRUE)
#'
#' @return lbfgs object
#'
owlqn_r <- function(alpha, X, Y, epsilon, beta, lambda1 = 0, lambda2 = 0,
                    weight = NULL, max_iterations = 300, ..., invisible = TRUE) {
    fn <- function(alpha) loss_smooth(alpha, X, Y, epsilon, beta, 0, lambda2, weight)
    gd <- function(alpha) loss_smooth_grad(alpha, X, Y, epsilon, beta, 0, lambda2, weight)
    lbfgs::lbfgs(fn, gd, alpha, ..., max_iterations = max_iterations, invisible = invisible, orthantwise_c = lambda1)
}

#' Calculate the Logarithm of the approximation ratio
#' (logarithms are used for numerically stable calculations)
#' See Theorem 3 from the paper for more details
#'
#' @param residuals2 squared residuals
#' @param epsilon2 squared error tolerance
#' @param beta1 current sigmoid steepness
#' @param beta2 next sigmoid steepness
#' @param weight weight vector (default: NULL)
#'
#' @return log(approximation_ratio)
#'
#' @importFrom stats uniroot
#'
log_approximation_ratio <- function(residuals2, epsilon2, beta1, beta2, weight = NULL) {
    if (beta1 >= beta2) {
        return(0)
    }
    # phi = -pmin(0, r^2/n-e^2)
    phi <- if (is.null(weight)) {
        pmax(0, epsilon2 - residuals2 / length(residuals2))
    } else {
        pmax(0, epsilon2 - residuals2 / sum(weight)) * weight
    }
    ## log(f(r, beta)), assuming squared r, the phi is calculated separately
    lf <- function(r, beta) log_sigmoidc(beta * (epsilon2 - r))
    ## derivative of log(f(r, beta1)/f(r, beta2))
    lg <- function(r) -beta1 * dlog_sigmoid(beta1 * (epsilon2 - r)) + beta2 * dlog_sigmoid(beta2 * (epsilon2 - r))
    # Calculate log(k) (k = min_r f_1(r) / f_2(r), see Thm. 3)
    g.zero <- lg(0)
    if (g.zero < 0) {
        ## If derivative at r=0 is negative the minimum is within r>0 and r<epsilon2.
        ## (due to lg(0) < 0 and lg(epsilon2) = -beta1*0.5 + beta2*0.5 > 0)
        a <- uniroot(lg, lower = 0, upper = epsilon2, f.lower = g.zero)$root
        lk <- min(lf(0, beta1) - lf(0, beta2), lf(a, beta1) - lf(a, beta2))
    } else {
        ## If derivative at r=0 is positive the function has minimum at r<0 and hence the minimum
        ## can be found at r=0 (negative values of the squared residual are not allowed).
        lk <- lf(0, beta1) - lf(0, beta2)
    }
    # Calculate log(K) (K = G_1(a_1) / (G_2(a_1) k), see Thm. 3)
    log_sum_special(lf(residuals2, beta1), phi) - lk - log_sum_special(lf(residuals2, beta2), phi)
}

#' Find the matching *epsilon
#'
#' @param residuals2 squared residuals
#' @param epsilon2 squared error tolerance
#' @param beta sigmoid steepness
#' @param weight weight vector (default: NULL)
#'
#' @return *epsilon
#'
matching_epsilon <- function(residuals2, epsilon2, beta, weight = NULL) {
    if (is.null(weight)) {
        residuals2 <- sort(residuals2)
        loss <- sigmoid(beta * (epsilon2 - residuals2))
        i <- which.max(seq_along(residuals2) * loss)
    } else {
        ord <- order(residuals2)
        residuals2 <- residuals2[ord]
        weight <- weight[ord]
        loss <- sigmoid(beta * (epsilon2 - residuals2))
        i <- which.max(cumsum(weight) * loss)
    }
    sqrt(residuals2[i])
}

#' Print debug statement for how the graduated optimisation is going
#'
#' @param alpha linear model
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param beta current sigmoid steepness
#' @param lambda1 L1 coefficients
#' @param lambda2 L2 coefficients
#' @param weight weight vector
#' @param beta_max max sigmoid steepness
#'
grad_opt_debug <- function(alpha, X, Y, epsilon, beta, lambda1, lambda2, weight, beta_max) {
    residuals <- c(Y - X %*% alpha)^2
    approx <- exp(log_approximation_ratio(residuals, epsilon^2, beta, beta_max, weight))
    m_epsilon <- matching_epsilon(residuals, epsilon^2, beta, weight)
    loss_sm <- loss_smooth(alpha, X, Y, epsilon, beta, lambda1, lambda2, weight)
    loss_sh <- loss_smooth(alpha, X, Y, epsilon, beta_max, lambda1, lambda2, weight)
    loss_ha <- loss_sharp(alpha, X, Y, epsilon, lambda1, lambda2, weight)
    cat(sprintf(
        "Graduated Optimisation: beta = %6.3f eps* = %.3f approx = %5.3f Ls = %g Lh = %g L = %g\n",
        beta * epsilon^2, m_epsilon, approx, loss_sm, loss_sh, loss_ha
    ))
}


#' Find the next beta according to:
#'   ¤ approximation_ratio(alpha, beta_old, beta_new) == max_approx
#'   ¤ beta_new >= beta_old + min_increase
#'   ¤ beta_new <= beta_max
#'
#' @param residuals2 squared residuals
#' @param epsilon2 squared error tolerance
#' @param beta current sigmoid steepness
#' @param weight weight vector (default: NULL)
#' @param beta_max max sigmoid steepnsess
#' @param log_max_approx logarithm of the approximation ratio target for increasing beta
#' @param beta_min_increase minimum beta step
#'
#' @return beta_new
#'
#' @importFrom stats uniroot
#'
next_beta <- function(residuals2, epsilon2, beta = 0, weight = NULL,
                      beta_max = 20 / epsilon2, log_max_approx = log(1.15),
                      beta_min_increase = (beta_max + beta) * 0.0005) {
    if (beta >= beta_max) {
        return(beta)
    }
    log_approx <- log_approximation_ratio(residuals2, epsilon2, beta, beta_max, weight)
    if (log_approx <= log_max_approx) {
        beta_max
    } else {
        f <- function(b) log_approximation_ratio(residuals2, epsilon2, beta, b, weight) - log_max_approx
        beta_new <- uniroot(f,
            lower = beta, upper = beta_max, f.lower = -log_max_approx,
            f.upper = log_approx - log_max_approx
        )$root
        max(beta_new, beta + beta_min_increase)
    }
}

#' Graduated Optimisation to solve the SLISE problem
#'
#' @param alpha Initial linear model (if NULL then OLS)
#' @param X Data matrix
#' @param Y Response vector
#' @param epsilon Error tolerance
#' @param beta Starting sigmoid steepness (default: 0 == convex problem)
#' @param lambda1 L1 coefficient (default: 0)
#' @param lambda2 L1 coefficient (default: 0)
#' @param weight Weight vector (default: NULL == no weights)
#' @param beta_max Stopping sigmoid steepness (default: 20 / epsilon^2)
#' @param max_approx Approximation ratio when selecting the next beta (default: 1.15)
#' @param max_iterations Maximum number of OWL-QN iterations (default: 300)
#' @param debug Should debug statement be printed each iteration (default: FALSE)
#' @param beta_min_increase Minimum amount to increase beta (default: beta_max * 0.0005)
#' @param ... Additional parameters to OWL-QN
#'
#' @return lbfgs object with beta (max) and the number of iteration steps
#' @export
#'
graduated_optimisation <- function(alpha, X, Y, epsilon, beta = 0, lambda1 = 0, lambda2 = 0,
                                   weight = NULL, beta_max = 20 / epsilon^2, max_approx = 1.15,
                                   max_iterations = 300, beta_min_increase = beta_max * 0.0005,
                                   debug = FALSE, ...) {
    stopifnot(epsilon > 0)
    stopifnot(beta >= 0)
    stopifnot(lambda1 >= 0)
    stopifnot(lambda2 >= 0)
    stopifnot(beta_max > 0)
    stopifnot(max_approx > 1)
    stopifnot(max_iterations > 0)
    res <- list(par = if (is.null(alpha)) rep(0, ncol(X)) else alpha)
    max_approx <- log(max_approx)
    dc <- data_container(X = X, Y = Y, epsilon = epsilon, beta = beta, lambda1 = 0, lambda2 = lambda2, weight = weight)
    while (beta < beta_max) {
        res <- owlqn_c(res$par, dc, lambda1, max_iterations, ...)
        if (debug) grad_opt_debug(res$par, X, Y, epsilon, beta, lambda1, lambda2, weight, beta_max)
        beta <- next_beta((X %*% res$par - Y)^2, epsilon^2, beta, weight, beta_max, max_approx, beta_min_increase)
        dc$setBeta(beta)
    }
    res <- owlqn_c(res$par, dc, lambda1, max_iterations * 4, ...)
    if (debug) grad_opt_debug(res$par, X, Y, epsilon, beta, lambda1, lambda2, weight, beta_max)
    res
}
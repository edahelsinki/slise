# This script contains the optimisations for SLISE (Graduated Optimisation and OWL-QN)

#' Smooth Loss
#'
#' @param alpha The vector to calculate loss for
#' @param X The data matrix
#' @param Y The response vector
#' @param epsilon (Optional) The acceptable error
#' @param lambda (Optional) The sparsity reguraliser
#' @param beta (Optional) The steepness of the sigmoid (default: 3)
#'
#' @return The loss value
#'
loss_smooth <- function(alpha, X, Y, epsilon = 0.1, lambda = 0, beta = 3) {
    epsilon <- epsilon ^ 2
    distances <- c(X %*% alpha - Y) ^ 2
    subsize <- sigmoidc(beta / epsilon * (epsilon - distances))
    eps2 <- epsilon * length(Y)
    loss <- pmin(0, distances - eps2) #phi(x) ~ pmin(0, x)

    if (lambda > 0)
        sum(subsize * loss) / length(Y) + lambda * sum(abs(alpha))
    else
        sum(subsize * loss) / length(Y)
}

#' Smooth Loss Gradient
#'
#' @param alpha The vector to calculate loss-gradient for
#' @param X The data matrix
#' @param Y The response vector
#' @param epsilon (Optional) The acceptable error
#' @param lambda (Optional) The sparsity reguraliser
#' @param beta (Optional) The steepness of the sigmoid (default: 3)
#'
#' @return The gradients for alpha
#'
loss_smooth_grad <- function(alpha, X, Y, epsilon = 0.1, lambda = 0, beta = 3) {
    epsilon <- epsilon ^ 2
    beta <- beta / epsilon
    distances <- c(X %*% alpha - Y)
    distances2 <- distances ^ 2

    f <- distances2 / nrow(X) - epsilon
    s <- sigmoidc(beta * (epsilon - distances2))
    k1 <- 2 / nrow(X)
    k2 <- -2 * beta * (s - s ^ 2)

    distances <- ifelse(f < 0, distances, 0) #phi(x) ~ pmin(0, x)

    if (lambda > 0)
        (t(distances * X) %*% ((s * k1) + (f * k2))) + lambda * sign(alpha)
    else
        (t(distances * X) %*% ((s * k1) + (f * k2)))
}


#' Sharp Loss Function
#'
#' @param alpha The vector to calculate loss for
#' @param X The data matrix
#' @param Y The response vector
#' @param epsilon (Optional) The acceptable error
#' @param lambda (Optional) The sparsity reguraliser
#'
#' @return The loss value
#'
loss_sharp <- function(alpha, X, Y, epsilon = 0.1, lambda = 0) {
    epsilon <- epsilon ^ 2
    distances <- (X %*% alpha - Y) ^ 2
    mask <- distances <= epsilon
    subsize_loss <- sum(mask) * epsilon
    regression_loss <- sum(distances[mask]) / length(Y)

    if (lambda > 0)
        -subsize_loss + regression_loss + lambda * sum(abs(alpha))
    else
        -subsize_loss + regression_loss
}

#' OWL-QN for optimising loss_smooth (Cpp implementation)
#'
#' @param alpha linear model to optimise
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param lambda L1 coefficient
#' @param beta sigmoid steepness
#' @param max_iterations number of OWL-QN iterations
#' @param ... other parameters to OWL-QN
#'
#' @return lbfgs object
#'
owlqn_c <- function(alpha, X, Y, epsilon = 0.1, lambda = 0, beta = 3, max_iterations = 250, ...) {
    dc <- methods::new(DataContainer, data = X, response = Y, beta = beta, epsilon = epsilon, lambda = 0)
    lbfgs::lbfgs(loss_smooth_c_ptr(), loss_smooth_grad_c_ptr(), alpha, dc$.pointer, ...,
        max_iterations = max_iterations, invisible = TRUE, orthantwise_c = lambda)
}

#' OWL-QN for optimising loss_smooth (R implementation)
#'
#' @param alpha linear model to optimise
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param lambda L1 coefficient
#' @param beta sigmoid steepness
#' @param max_iterations number of OWL-QN iterations
#' @param ... other parameters to OWL-QN
#'
#' @return lbfgs object
#'
owlqn_r <- function(alpha, X, Y, epsilon = 0.1, lambda = 0, beta = 3, max_iterations = 250, ...) {
    fn <- function(alpha) loss_smooth(alpha, X, Y, epsilon, 0, beta)
    gd <- function(alpha) loss_smooth_grad(alpha, X, Y, epsilon, 0, beta)
    lbfgs::lbfgs(fn, gd, alpha, ..., max_iterations = max_iterations, invisible = TRUE, orthantwise_c = lambda)
}

#' Calculate the Logarithm of the approximation ratio
#' (logarithms are used for numerically stable calculations)
#' See Theorem 3 from the paper for more details
#'
#' @param residuals squared residuals
#' @param epsilon error tolerance
#' @param beta1 current sigmoid steepness
#' @param beta2 next sigmoid steepness
#'
#' @return log(approximation_ratio)
#'
log_approximation_ratio <- function(residuals, epsilon, beta1, beta2) {
    if (beta1 >= beta2) return(0)
    epsilon <- epsilon ^ 2
    beta1 <- beta1 / epsilon
    beta2 <- beta2 / epsilon
    ## log(f(r, beta)), assuming squared r, the phi is calculated separately                    
    lf <- function(r, beta) log_sigmoid(beta * (epsilon - r))
    phi <- pmax(0, epsilon - residuals / length(residuals)) # = -pmin(0, r^2/n-e^2)
    ## derivative of log(f(r, beta1)/f(r, beta2))
    lg <- function(r) - beta1 * dlog_sigmoid(beta1 * (epsilon - r)) + beta2 * dlog_sigmoid(beta2 * (epsilon - r))
    # Calculate log(k) (see Thm. 3)
    g.zero <- lg(0)
    if (g.zero < 0) {
        ## If derivative at r=0 is negative the minimum is within r>0 and r<epsilon.
        ## (due to lg(0) < 0 and lg(epsilon) = -beta1*0.5 + beta2*0.5 > 0)
        a <- stats::uniroot(lg, lower = 0, upper = epsilon, f.lower = g.zero)$root
        lK <- min(lf(0, beta1) - lf(0, beta2), lf(a, beta1) - lf(a, beta2))
    } else {
        ## If derivative at r=0 is positive the function has minimum at r<0 and hence the minimum
        ## can be found at r=0 (negative values of the squared residual are not allowed).
        lK <- lf(0, beta1) - lf(0, beta2)
    }
    # Calculate log(K) (see Thm. 3)
    if (sum(phi) < 0) {
        lphi <- log(phi)
        log_sum(lf(residuals, beta1) + lphi) - lK - log_sum(lf(residuals, beta2) + lphi)
    } else {
        # phi is constant (0) and can be removed from the division
        log_sum(lf(residuals, beta1)) - lK - log_sum(lf(residuals, beta2))
    }
}

#' Find the matching *epsilon
#'
#' @param residuals squared residuals
#' @param epsilon error tolerance
#' @param beta sigmoid steepness
#'
#' @return *epsilon
#'
matching_epsilon <- function(residuals, epsilon, beta) {
    epsilon <- epsilon ^ 2
    residuals <- sort(residuals)
    loss <- sigmoid(beta / epsilon * (epsilon - residuals))
    i <- which.max(seq_along(residuals) * loss)
    sqrt(residuals[i])
}

#' Print debug statement for how the graduated optimisation is going
#'
#' @param alpha linear model
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param lambda L1 weight
#' @param beta current sigmoid steepness
#' @param beta_max max sigmoid steepness
#'
grad_opt_debug <- function(alpha, X, Y, epsilon, lambda, beta, beta_max) {
    residuals <- c(Y - X %*% alpha)^2
    approx <- exp(log_approximation_ratio(residuals, epsilon, beta, beta_max))
    m_epsilon <- matching_epsilon(residuals, epsilon, beta)
    loss_sm <- loss_smooth(alpha, X, Y, epsilon, lambda, beta)
    loss_sh <- loss_smooth(alpha, X, Y, epsilon, lambda, beta_max)
    loss_ha <- loss_sharp(alpha, X, Y, epsilon, lambda)
    cat(sprintf("Graduated Optimisation: beta = %6.3f eps* = %.3f approx = %5.3f Ls = %g Lh = %g L = %g\n",
        beta, m_epsilon, approx, loss_sm, loss_sh, loss_ha))
}


#' Find the next beta according to:
#'   ¤ approximation_ratio(alpha, beta_old, beta_new) == max_approx
#'   ¤ beta_new >= beta_old + min_increase
#'
#' @param alpha linear model
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param beta current sigmoid steepness
#' @param beta_max max sigmoid steepnsess
#' @param max_approx approximation ratio target for increasing beta
#' @param beta_min_increase minimum beta step
#'
#' @return beta_new
#'
next_beta <- function(alpha, X, Y, epsilon = 0.1, beta = 0, beta_max = 25,
        max_approx = 1.2, beta_min_increase = beta_max * 0.0005) {
    if (beta >= beta_max) return(beta)
    residuals <- c(Y - X %*% alpha)^2
    max_approx <- log(max_approx)
    log_approx <- log_approximation_ratio(residuals, epsilon, beta, beta_max)
    if (log_approx <= max_approx) {
        beta_max
    } else {
        f <- function(b) log_approximation_ratio(residuals, epsilon, beta, b) - max_approx
        beta_new <- stats::uniroot(f, lower = beta, upper = beta_max, f.lower = -max_approx,
                f.upper = log_approx - max_approx)$root
        max(beta_new, beta + beta_min_increase)
    }
}

#' Graduated Optimisation to solve the SLISE problem
#'
#' @param alpha initial linear model (if NULL then OLS)
#' @param X data matrix
#' @param Y response vector
#' @param epsilon error tolerance
#' @param lambda L1 coefficient (0)
#' @param beta starting sigmoid steepness (0 => convex problem)
#' @param beta_max stopping sigmoid steepness (25)
#' @param max_approx approximation ratio when selecting the next beta (1.2)
#' @param max_iterations maximum number of OWL-QN iterations (100)
#' @param debug should debug statement be printed each iteration (FALSE)
#' @param ... Additional parameters to OWL-QN
#' @param beta_min_increase the minimum increase of beta each iteration (beta_max * 0.0005)
#' @param beta_start_max Ignored
#'
#' @return lbfgs object with beta (max) and the number of iteration steps
#'
graduated_optimisation <- function(alpha, X, Y, epsilon = 0.1, lambda = 0, beta = 0, beta_max = 25,
        max_approx = 1.2, max_iterations = 100, debug = FALSE,
        ..., beta_min_increase = beta_max * 0.0005, beta_start_max = NULL) {
    res <- list(par = if (is.null(alpha)) stats::.lm.fit(X, Y)$coefficients else alpha)
    while (beta < beta_max) {
        res <- owlqn_c(res$par, X, Y, epsilon, lambda, beta, max_iterations, ...)
        if (debug) grad_opt_debug(res$par, X, Y, epsilon, lambda, beta, beta_max)
        beta <- next_beta(res$par, X, Y, epsilon, beta, beta_max, max_approx, beta_min_increase)
    }
    res <- owlqn_c(res$par, X, Y, epsilon, lambda, beta, max_iterations * 2, ...)
    if (debug) grad_opt_debug(res$par, X, Y, epsilon, lambda, beta, beta_max)
    res
}

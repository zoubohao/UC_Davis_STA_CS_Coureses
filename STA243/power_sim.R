power_iteration = function(A, v0, eps = 1e-6, maxiter=100) {
  # Please implement the function power_iteration that takes in the matrix X and initial vector v0 and returns the eigenvector.
}

set.seed(5)
E = matrix(rnorm(100), 10, 10)
v = c(1, rep(0, 9))
lams = 1:10
prods = c()
for (lambda in lams) {
  X = lambda*outer(v, v) + E
  v0 = rep(1, nrow(E))
  v0 = v0/sqrt(sum(v0^2))
  vv = power_iteration(X, v0)
  prods = c(prods, abs(v %*% vv))
}
plot(lams, prods, "b")

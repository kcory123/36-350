generate_data = function(n,p) {
  covariates= matrix( rnorm(n*p,0,1), nrow = n, ncol = p)
  responses = vector(length = n)
  for (i in 1:n) {
    responses[i] = rnorm(1,0,1)
  }
  return(list(covariates = covariates, responses = responses))
}
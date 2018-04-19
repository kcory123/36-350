
generate_data = function(n,p) {
  covariates= matrix( rnorm(n*p,0,1), nrow = n, ncol = p)
  responses = vector(length = n)
  for (i in 1:n) {
    responses[i] = rnorm(1,0,1)
  }
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) {
  regression = lm(responses~ covariates)
  retained.covariates = which(regression$p.value <= cutoff)
  regression2 = lm(responses ~ covariates[,retained.covariates])
  return(regression2$p.value)
  
}
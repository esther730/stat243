## Initialization

init_pop = function(dataset, numsample) {
  ## check number of rows of the dataset
  numrows = dim(dataset)[2]
  ## create the intial population as a dataframe
  pop = as.data.frame(matrix(sample(x=c(0,1),
        numrows*numsample,replace = T),nrow = numsample))
  colnames(pop) = colnames(dataset)
  return (pop)
}

## Stop the iteration

check_criteria = function(maxiteration = 10000, mincrit = NULL) {
  if (!is.null(mincrit)) {
    ## create an empty vector for the AIC value in each loop
    AICvalues = c()
    ## create an empty vector for the best model
    bestmodel = c()
    ## evaluate the intial population
    ## "some function created by Kesin"
    
    AICvalues[1] = "AIC value of the initial population"
    ## create a for loop for evaluatiing the criteria
    for (i in 1:maxiteration) {
      ## functions created by Amanda for genetic offspring, crossover and mutation
      ## then, function created by Kesin for evaluating AIC
      AICvalues[i+1] = "the evaluated AIC value"
      bestmodel = "the best model in the iteration"
      if (AICvalues[i+1] - AICvalues[i] <= mincrit & AICvalues[i+1] - AICvalues[i]>0) {
        break
      }
    }
    ## Return the best model and AIC
    return (c(bestmodel, tail(AICvalues, n=1)))
  } if (is.null(mincrit)) {
    ## create an empty vector for the AIC value in each loop
    AICvalues = c()
    ## create an empty vector for the best model
    bestmodel = c()
    ## evaluate the intial population
    ## "some function created by Kesin"
    
    AICvalues[1] = "AIC value of the initial population"
    ## create a for loop for evaluatiing the criteria
    for (i in 1:maxiteration) {
      ## functions created by Amanda for genetic offspring, crossover and mutation
      ## then, function created by Kesin for evaluating AIC
      AICvalues[i+1] = "the evaluated AIC value"
      bestmodel = "the best model in the iteration"
    }
    ## Return the best model and AIC
    return (c(bestmodel, tail(AICvalues, n=1)))
  }
}


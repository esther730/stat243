#'nextGen
#'
#'Make the next generation
#'@param pop list, population
#'@param pSelect float, the worse part to remove from the population(default=0.2)
#'@param pMutate float, the probability used to decide number of mutation(default=0.01)
#'@param fitfunc method, model selection method(default is AIC)
#'@param family family,for linear regression model the family should be a continuous probability density function (default is gaussian family)
#'@param end_iter_cond logical, the index to decide whether to stop iteration (default=F)
#'@return list, population
#'@export

nextGen = function(pop, pSelect=0.2, pMutate=0.01, fitfunc="AIC", family="gaussian", end_iter_cond = F) {
  ## 1. remove lowest pSelect*100% of chromosomes from population
  ## 2. repopulate with offspring
  ## 2.a. select parents with probability proportional to fitness
  ## 2.b. perform crossover between parent chromosomes
  ## 3. generate random mutations in population
  ## output: object of class "population"
  # pop: object of class "population"
  # pSelect: proportion of population to select out

  # 1. remove lowest pSelect*100% of chromosomes from population
  numRemove = floor(pSelect*length(pop$genomes))
  fitness = getFitness(pop$genomes)


  ####################################
  oldGenomes = pop$genomes[order(fitness, decreasing=F)[1:(length(fitness) - numRemove)]]
  ####################################


  # 2. repopulate with offspring
  nChrom = length(oldGenomes)
  weights = 2*order(getFitness(oldGenomes), decreasing=T)/(nChrom*(nChrom+1))
  if(end_iter_cond) {
    if(length(unique(getFitness(oldGenomes))) == 1)  return(pop)
  }

  newGenomes = lapply(1:numRemove,
                      function(x) {
                        crossover(sample(oldGenomes, size=2, prob=weights),
                                  dat=pop$data, fitfunc=fitfunc, family=family)
                      })
  genomes = unlist(list(oldGenomes, newGenomes), recursive=F)
  newPop = initPop(dat=pop$data, popSize=NA, genomes=genomes,
                   fitfunc=fitfunc, family=family)

  # 3. generate random mutations in population
  nMutations = rbinom(n = length(genomes), size=(ncol(pop$data)-1), prob=pMutate)
  newPop = mutatePop(newPop, nMutations)

  return(newPop)
}

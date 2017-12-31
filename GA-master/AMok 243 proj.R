library(testthat)

initChrom = function(dat, chrom=NULL, fitfunc="AIC", family="gaussian") {
  ## initializes new objects of class "chromosome"
  ## output: object of class "chromosome"
  # dat: data in data frame, with first column as outcome variable
  # chrom: numeric vector of 0/1 for variable inclusion in model
  # fitfunc: function name to evaluate fitness on glm object
  # family: family argument for glm function
  if(is.null(chrom)) {
    chrom = sample(c(0,1), size = ncol(dat)-1, replace=T)
  }
  fitness = do.call(evalFitness, list(fitfunc, chrom, dat, family))
  obj = list(chrom, fitness)
  names(obj) = c("chrom", "fitness")
  class(obj) = "chromosome"
  return(obj)
}

convertFormula = function(dat, chrom) {
  ## creates formula for glm() function
  ## output: object of class "formula"
  # dat: data in data frame, with first column as outcome variables
  # chrom: numeric vector of 0/1 for variable inclusion in model
  varNames = colnames(dat)
  varInclude = paste(varNames[2:ncol(dat)][chrom==1],collapse="+")
  return(as.formula(paste0(varNames[1], " ~ ", varInclude)))
}

evalFitness = function(fitfunc, chrom, dat, family) {
  ## evaluates AIC of linear model corresponding to chromosome
  ## output: numeric
  # chrom: numeric vector of 0/1 for variable inclusion in model
  # dat: data in data frame, with first column as outcome variable
  # returns AIC associated with corresponding linear model
  form = convertFormula(dat, chrom)
  mod = glm(form, family=family, data=dat)
  fitness = do.call(fitfunc, list(mod))
  return(fitness)
}

initPop = function(dat, popSize=30, genomes=NULL, fitfunc="AIC", family="gaussian") {
  ## initializes new objects of class "population"
  ## output: object of class "population"
  # dat: data in data frame, with first column as outcome variable
  # popSize: number of chromosomes in population
  if(is.null(genomes)) {
    genomes = lapply(1:popSize, 
                     function(x) initChrom(dat, fitfunc=fitfunc, family=family))
  }
  fitness = getFitness(genomes)
  bestChrom = genomes[[which.max(fitness)]]
  obj = list(dat, genomes, bestChrom)
  names(obj) = c("data","genomes", "bestChrom")
  class(obj) = "population"
  return(obj)
}

getFitness = function(genomes) {
  ## retrieves AICs from list of chromosomes
  ## output: numeric vector
  # genomes: list of "chromosome" objects
  sapply(genomes, function(obj) obj$fitness)
}

nextGen = function(pop, pSelect=0.2, pMutate=0.01, fitfunc="AIC", family="gaussian") {
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
  oldGenomes = pop$genomes[order(fitness, decreasing=F)>numRemove]
  
  # 2. repopulate with offspring
  weights = getFitness(oldGenomes)/sum(getFitness(oldGenomes))
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

crossover = function(parents, dat, fitfunc="AIC", family="gaussian") {
  ## performs crossover between 2 parent chromosomes
  ## output: object of class "chromosome"
  # parents: list of 2 "chromosome" objects
  chromA = parents[[1]]
  chromB = parents[[2]]
  nVars = length(chromA$chrom)
  pos = sample.int(nVars, size=1)
  chrom = c(chromA$chrom[1:pos], chromB$chrom[(pos+1):nVars])
  obj = initChrom(dat=dat, chrom=chrom, fitfunc=fitfunc, family=family)
  return(obj)
}

mutateChrom = function(chrom, nMutate, dat, fitfunc="AIC", family="gaussian") {
  ## performs mutation on single chromosomes
  ## output: object of class "chromosome"
  # chrom: object of class "chromosome"
  # nMutate: number of mutations to perform on chromosome
  nVars = length(chrom$chrom)
  posMutate = sample.int(nVars, size=nMutate)
  newChrom = chrom$chrom
  newChrom[posMutate] = abs(newChrom[posMutate]-1)
  obj = initChrom(dat=dat, chrom=newChrom, fitfunc=fitfunc, family=family)
  return(obj)
}

mutatePop = function(pop, nMutations, fitfunc="AIC", family="gaussian") {
  ## performs mutations on population
  ## output: object of class "population"
  # pop: object of class "population"
  # nMutations: number of mutations to perform on each chromosome in pop
  toMutate = which(nMutations > 0)
  for(i in toMutate) {
    pop$genomes[[i]] = mutateChrom(pop$genomes[[i]], nMutations[i], pop$data,
                                   fitfunc=fitfunc, family=family)
  }
  return(pop)
}
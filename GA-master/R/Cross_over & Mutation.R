#'Cross over
#'
#'Generate offsprings by randomly pick the position of chromosomes
#'@param parents list, parents to generate offsprings,length=2
#'@param dat dataframe, data to generate offspring(default sets the dependent variable in first column and independent varialbes is other columns)
#'@param fitfunc method, model selection method(default is AIC)
#'@param family family,for linear regression model the family should be a continuous probability density function (default is gaussian family)
#'@export

crossover = function(parents, dat, fitfunc="AIC", family="gaussian") {
  ## performs crossover between 2 parent chromosomes
  ## output: object of class "chromosome"
  # parents: list of 2 "chromosome" objects
  chromA = parents[[1]]
  chromB = parents[[2]]
  nVars = length(chromA$chrom)
  pos = sample.int(nVars-1, size=1)
  chrom = c(chromA$chrom[1:pos], chromB$chrom[(pos+1):nVars])
  obj = initChrom(dat=dat, chrom=chrom, fitfunc=fitfunc, family=family)
  return(obj)
}

#'Muatation
#'
#'Performs mutation on single chromosomes
#'@param chrom vector, a chromsome which a mutation is desired
#'@param nMutate integer, indicating how many mutations to perform on chromosome
#'@param dat dataframe, data to do mutation(default sets the dependent variable in first column and independent varialbes is other columns)
#'@param fitfunc method, model selection method(default is AIC)
#'@param family family,for linear regression model the family should be a continuous probability density function (default is gaussian family)
#'@export

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


#'Muatation
#'
#'Perform mutations on population
#'@param pop list, a population which mutation is desired
#'@param nMutate integer, indicating how many mutations to perform on chromosome
#'@param dat dataframe, data to do mutation(default sets the dependent variable in first column and independent varialbes is other columns) )
#'@param fitfunc method, model selection method(default is AIC)
#'@param family family,for linear regression model the family should be a continuous probability density function (default is gaussian family)
#'@export

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

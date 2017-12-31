#'initChrom
#'
#'Initializes chromosome
#'@param dat dataframe, data to operation(default sets dependent variable in first column and independent varialbes is other columns)
#'@param chrom vector, chromesome which is composed of binary data(default is NULL)
#'@param fitfunc method, model selection method(default is AIC)
#'@param family family,for linear regression model the family should be a continuous probability density function (default is gaussian family)
#'@export

initChrom = function(dat, chrom=NULL, fitfunc="AIC", family="gaussian") {
  ## initializes new objects of class "chromosome"
  ## output: object of class "chromosome"
  # dat: data in data frame, with first column as outcome variable
  # chrom: numeric vector of 0/1 for variable inclusion in model
  # fitfunc: function name to evaluate fitness on glm object
  # family: family argument for glm function
  if(is.null(chrom)) {
    chrom = sample(c(0,1), size = ncol(dat) - 1, replace=T)
  }
  fitness = do.call(evalFitness, list(fitfunc, chrom, dat, family))
  obj = list(chrom, fitness)
  names(obj) = c("chrom", "fitness")
  class(obj) = "chromosome"
  return(obj)
}


#'initPop
#'
#'Initializes population
#'@param dat dataframe, data to operation(default sets dependent variable in first column and independent varialbes is other columns)
#'@param popSize integer, population size(default=30)
#'@param genomes list, composed of chromosomes and fitness value with lenght equal to population size(default=NULL)
#'@param fitfunc method, model selection method(default is AIC)
#'@param family family,for linear regression model the family should be a continuous probability density function (default is gaussian family)
#'@export

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
  ##########################################
  bestChrom = genomes[[which.min(fitness)]]
  ##########################################
  obj = list(dat, genomes, bestChrom)
  names(obj) = c("data","genomes", "bestChrom")
  class(obj) = "population"
  return(obj)
}

#'convertFormula
#'
#'Creates formula for glm() function
#'@param dat dataframe, data to generate offspring(default sets the dependent variable in first column and independent varialbes is other columns)
#'@param chrom vector, a chromsome which composed of the binary dataset, 1 means to include in the model
#'@return strings of the regression formula
#'@details Transfer the input chromesome into a model that composed of independent variable and selected dependent variables in the chromosome
#'@export

convertFormula = function(dat, chrom) {
  ## creates formula for glm() function
  ## output: object of class "formula"
  # dat: data in data frame, with first column as outcome variables
  # chrom: numeric vector of 0/1 for variable inclusion in model
  varNames = colnames(dat)
  varInclude = ifelse(sum(chrom)==0,
                      "0",
                      paste(varNames[2:ncol(dat)][chrom==1],collapse="+"))
  return(as.formula(paste0(varNames[1], " ~ ", varInclude)))
}

#'evalFitness
#'
#'Evaluates AIC of linear model corresponding to chromosome
#'@param fitfunc method, model selection method(default is AIC)
#'@param chrom vector, a chromsome which composed of the binary dataset, 1 means to include in the model
#'@param dat dataframe, data to do mutation(default sets the dependent variable in first column and independent varialbes is other columns)
#'@param family family,for linear regression model the family should be a continuous probability density function (default is gaussian family)
#'@export


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

#'getFitness
#'
#'Retrieves AICs from list of chromosomes
#'@param genomes list, composed of chromosomes and fitness value with lenght equal to population size(default=NULL)
#'@return vector, composed of AIC of each chromsome
#'@export

getFitness = function(genomes) {
  ## retrieves AICs from list of chromosomes
  ## output: numeric vector
  # genomes: list of "chromosome" objects
  sapply(genomes, function(obj) obj$fitness)
}

#'swapCol
#'
#'Swap the dependent variable into first column
#'@param dat dataframe, data to generate offspring(default sets the dependent variable in first column and independent varialbes is other columns)
#'@param y_name string, column of dependent variable
#'@return dataframe
#'@details swaps the columns of the data set so that the first column corresponding to the dependent variable
#'@export

swapCol = function(dat, y_name) {
  # swaps the columns of the data set so that the first column corresponding to the dependent variable
  #output: data.frame
  #dat: the original data
  #y_name: string of the name of the dependent variable
  # returns a new data set with the first columns as the dependent variable.
  col_names <- colnames(dat)
  y_ind <- which(col_names == y_name)
  new_dat <- cbind(dat[y_ind], dat[, -y_ind])
  return(new_dat)
}


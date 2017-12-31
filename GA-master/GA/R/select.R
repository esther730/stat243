#'select
#'
#'Select the best model
#'@param dat dataframe, data to operation(default sets dependent variable in first column and independent varialbes is other columns)
#'@param y_name string, column of dependent variable
#'@param popSize integer, population size(default=30)
#'@param pSelect float, the worse part to remove from the population(default=0.2)
#'@param pMutate float, the probability used to decide number of mutation(default=0.01)
#'@param max_iter integer, the maximum number of iteration
#'@param fitfunc method, model selection method(default is AIC)
#'@param family family,for linear regression model the family should be a continuous probability density function (default is gaussian family)
#'@param end_iter_cond logical, the index to decide whether to stop iteration (default=F)
#'@return list, return the best model that composed of chromsome and corresponding fitness
#'@export


select <- function(dat, y_name, popSize=30, pSelect=0.2, pMutate=0.01, max_iter=1000,
                   fitfunc="AIC", family="gaussian", end_iter_cond = F) {

  #Create a new data set with the dependent variable as the first column from the original data set
  new_dat = swapCol(dat = dat, y_name = y_name)

  init_pop = initPop(dat = new_dat, popSize = popSize, genomes=NULL,
                     fitfunc = fitfunc, family = family)

  #Create two vectors to store the best and the average fitness value for each generation (iteration)
  best_fitness = rep(NA, max_iter+1)
  best_fitness[1] = init_pop$bestChrom$fitness
  avg_fitness = rep(NA, max_iter+1)
  avg_fitness[1] = mean(getFitness(init_pop$genomes))

  next_gen = nextGen(init_pop, pSelect=pSelect, pMutate=pMutate, fitfunc=fitfunc, family=family)
  best_gen = next_gen
  for(i in 1:max_iter) {
    best_fitness[i+1] = next_gen$bestChrom$fitness
    avg_fitness[i+1] = mean(getFitness(next_gen$genomes))
    updated_gen = nextGen(next_gen, pSelect=pSelect,
                          pMutate=pMutate, fitfunc=fitfunc, family=family)

    #The iteration will stop when members of the next generation are exactly the same as the previous one
    #if the end_iter_cond is set to be true
    if(end_iter_cond) {
      set1 <-c(next_gen, NA)
      set2 <-c(updated_gen, NA)
      if(setequal(set1, set2)) {
        print(paste("The best model selected by ", fitfunc, " using ",
                    family, " distribution is generated at the ",
                    i, "th iteration.", "The fitness value for the model is ",
                    best_gen$bestChrom$fitness, ".", sep = ""))
        break
      }

    }

    next_gen = updated_gen

    #Updated the best model selected by the fitness function
    if(min(best_fitness, na.rm=T) >= updated_gen$bestChrom$fitness) {
      best_gen = updated_gen
    }

    #Generate a notification every 100 iterations
    if (i %% 100 == 0) print(paste("Finished the ", i, "th iteration.", sep = ""))
    if (i == max_iter) print(paste("This select function reaches the number of maximum iterations.",
                                   "The best model selected by ", fitfunc, " using ",
                                   family, " distribution within the maximum iterations",
                                   " has the fitness value as ", best_gen$bestChrom$fitness,
                                   ".", sep = ""))
  }

  #Output two plots corresponding to best fitness value per generation and average fitness value per generation
  par(mfrow=c(1,2))
  plot(best_fitness, main="best fitness per generation", xlab="iteration")
  plot(avg_fitness, main="average fitness per generation", xlab="iteration")

  #Generate the formula for the best model selected by the fitness function
  selected_var_ind <- which(best_gen$bestChrom$chrom == 1)
  selected_var <- paste0(colnames(new_dat)[selected_var_ind + 1], collapse = " + ")
  best_model <- paste(y_name, " ~ ", selected_var)

  result <- list(best_model,
                 best_gen$bestChrom$chrom,
                 best_gen$bestChrom$fitness)
  names(result) <- c("model", "chrom", "fitness")
  return(result)
}

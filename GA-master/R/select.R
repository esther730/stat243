#' Implementing Variable Selection in Regression Problems
#' @description select is used to implement a genetic algorithm for variable selection in regression problems, including both linear regression and GLMs.
#' @usage select(dat, y_name, fitfunc="AIC", family="gaussian",
#'        popSize=30, pSelect=0.2, pMutate=0.01,
#'        max_iter=1000, end_iter_cond = F)
#' @param dat dataframe, data to operation
#' @param y_name character string, name of dependent variable
#' @param popSize integer, population size (different models) for one generation (default=30)
#' @param pSelect float, the worse part to remove from the population (default=0.2)
#' @param pMutate float, the probability used to decide number of mutation (default=0.01)
#' @param max_iter integer, the maximum number of iterations (generations) that will be considered (default=1000)
#' @param fitfunc character string, the fitness function that evaluate the regression models. (default="AIC")
#' @param family character string, the distribution that will be used in glm function. (default="gaussian")
#' @param end_iter_cond logical, the logical value that will end the iterations based on specific conditions. When it is true, the function will stop generate new generations when the next generation is exactly the same as the previous one. (default=FALSE)
#' @details This function first uses the swapCol function to create a new data set with the dependent variable as the first column from the original data set, and substitutes the original one with this new one.\cr\cr Then initPop function will be used to generate the first generation, and nexGen function will be used within the for-loop iterations to generate subsequent generations. if end_iter_cond is true, the iteration will be terminated when the next generation is exactly the same as the previous one. Meanwhile, the function will keep track of the model with the least fitness value during each iteration.\cr\cr This function will give a notifiction every 100 iterations, and the iterations will be terminated after reaching the moximum number of iterations.
#' @return select returns a nested list with three sublists:\cr\cr$model contains the formula expression for the best model selected by the function.\cr\cr$chrom contains the chrom object corresponding to this model.\cr\cr$fitness contains the fitness value corresponding to this model.\cr\cr This function will also print two plots corresponding to best fitness value per generation and average fitness value per generation
#' @author Dongyu Lang, Amanda Mok, Kehsin Su, and Junyi Tang
#' @references Givens, G. H. and Hoeting J. A. (2012) \emph{Combinatorial Optimization}. Chapter 3 of \emph{Computational Statistics} in G. H. Givens and J. A. Hoeting, Wiley.
#' @examples require(GA)
#' select_var <- select(dat = mtcars, y_name = "mpg", fitfunc = "AIC",
#'                      pMutate = 0.0001, max_iter = 100, end_iter_cond = T)
#' select_var
#' @export


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

## test initChrom
test_that("Initialize new object correctly", {
   initial <- initChrom(mtcars)

   ## Check the class of the output
   expect_that( initial, is_a("chromosome") )
   expect_that( initial$chrom, is_a("numeric") )
   expect_that( initial$fitness, is_a("numeric") )

   ## Check for wrong inputs
   expect_error(initial_error <- initChrom(nodata))
   expect_error(initial_error <- initChrom(mtcars, fitfunc="CIC"))
})


## test convertFormula
test_that("convert formula correctly", {
  initial <- initChrom(mtcars)
  formula <- convertFormula(mtcars, initial$chrom)

  ## Check the class of the output
  expect_that( formula, is_a("formula") )

  ## Check the result of output
  initialchrom <- c(0,0,1,0,1,0,0,1,1,1)
  formula_initial <- convertFormula(mtcars, initialchrom)
  expectedresult <- "mpg ~ hp + wt + am + gear + carb"
  expect_equal(formula_initial, as.formula(expectedresult))

  ## Check for wrong inputs
  expect_error(convertFormula(mtcars, chrom = "not chrom"))
})



## test evalFitness
test_that("evaluate fitness correctly", {
  initial <- initChrom(mtcars)
  fitness <- evalFitness("AIC", initial$chrom, mtcars, "gaussian")

  ## Check the class of the output
  expect_that( fitness, is_a("numeric") )

  ## Check if the result is correct
  initialchrom <- c(0,0,1,0,1,0,0,1,1,1)
  formula_initial <- convertFormula(mtcars, initialchrom)
  lm_initial = lm(formula_initial,mtcars)
  expect_equal(evalFitness("AIC", initialchrom, mtcars, "gaussian"),AIC(lm_initial))

  ## Check for wrong inputs
  expect_error(evalFitness("CIC", initial$chrom, mtcars, "gaussian"))
  ## Check for longer chrom
  expect_error(evalFitness("AIC", chrom =rep(1,20), mtcars, "gaussian"))
  ## Check for wrong data
  expect_error(evalFitness("AIC", initial$chrom, c(1,0,1), "gaussian"))
})

## test initPop
test_that("Initialize population correctly", {
  initial <- initPop(dat = mtcars, popSize=50, fitfunc="AIC", family="gaussian")

  ## Check the class of the output
  expect_that( initial, is_a("population") )
  expect_that( initial$genomes, is_a("list") )
  expect_that( initial$genomes[[1]], is_a("chromosome"))
  expect_that( initial$genomes[[1]]$chrom, is_a("numeric"))
  expect_that( initial$genomes[[1]]$fitness, is_a("numeric"))

  ## Check the result
  expect_equal( length(initial$genomes), 50 )
  expect_equal( length(initial$genomes[[1]]$chrom), dim(mtcars)[2] - 1 )


  ## Check for wrong inputs
  expect_error(initPop(dat = nodata, popSize= 30, fitfunc="AIC", family="gaussian"))
})

## test getFitness
test_that("Fit the list correctly", {
  initial <- initPop(dat = mtcars, popSize=50, fitfunc="AIC", family="gaussian")
  fitness <- getFitness(initial$genomes)

  ## Check the class of the output
  expect_that( fitness, is_a("numeric"))

  ## Check the result
  expect_equal( length(fitness), 50 )


  ## Check for wrong inputs
  expect_error(getFitness(unlist(initial$genomes)))
})



## test crossover
test_that("Crossover correctly", {
  initial <- initPop(dat = mtcars, popSize=50, fitfunc="AIC", family="gaussian")
  crossover_test <- crossover(initial$genomes,mtcars)

  ## Check the class of the output
  expect_that( crossover_test$chrom, is_a("numeric"))
  expect_that( crossover_test, is_a("chromosome"))

  ## Check the result
  expect_equal( length(crossover_test$chrom), dim(mtcars)[2] - 1 )


  ## Check for wrong inputs
  expect_error(crossover(list(c(1,0,1),c(1,1,1)), mtcars))
})

## test mutateChrom

test_that("Mutation on single chromosomes correctly", {
  initial <- initPop(dat = mtcars, popSize=50, fitfunc="AIC", family="gaussian")
  mutatechrom_test <- mutateChrom(initial$genomes[[1]],3,mtcars)

  ## Check the class of the output
  expect_that( mutatechrom_test$chrom, is_a("numeric"))
  expect_that( mutatechrom_test, is_a("chromosome"))

  ## Check the result
  expect_equal(sum(mutatechrom_test$chrom != initial$genomes[[1]]$chrom), 3)


  ## Check for larger number of mutation
  expect_error(mutateChrom(initial$genomes[[1]],11,mtcars))
})

## test mutatePop
test_that("Mutation on population correctly", {
  initial <- initPop(dat = mtcars, popSize=50, fitfunc="AIC", family="gaussian")
  mutatepop_test <- mutatePop(initial,3)

  ## Check the class of the output
  expect_that( mutatepop_test$genomes, is_a("list"))
  expect_that( mutatepop_test, is_a("population"))

  ## Check the result
  expect_equal(sum(mutatepop_test$genomes[[1]]$chrom != initial$genomes[[1]]$chrom), 3)


  ## Check for larger number of mutation
  expect_error(mutatePop(initial,11))
})

## test nextGen

test_that("creating next generation correctly", {
  initial <- initPop(dat = mtcars, popSize=50, fitfunc="AIC", family="gaussian")
  nextgen_test <- nextGen(initial)

  ## Check the class of the output
  expect_that( nextgen_test$genomes, is_a("list"))
  expect_that( nextgen_test, is_a("population"))
})

## test for the overall functions

test_that("testing the overall", {
  ## Call the select function in our package
  selectit = select(dat = mtcars, y_name = "cyl", fitfunc = "AIC")
  ## We use stepwise regression to test if this result is the same as our result
  library(MASS)
  fit <- lm(cyl ~ mpg + disp + hp + drat + wt + qsec + vs + am + gear + carb,data=mtcars)
  step <- stepAIC(fit, direction="both",trace = 0)
  ## Now, we get the formula for the best result using stepwise regression
  colname <- colnames(step$model)
  step_best <- paste(colname[1]," ~ ",paste(colname[-1],collapse = " + "),sep = " ")
  ## The result for our genetic model
  gene_best = selectit$model
  ## Check if the results are the same
  expect_equal(step_best, gene_best)

})


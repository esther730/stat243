fit <- function(pop,data){
  y <- data[,1] #indep variable
  x <- data[,-1] #dep variable
  smod <- as.matrix(pop) 
  lgab <- function(mod,gl,abic){abic(gl(y~x[,which(mod==1)]))}
  #return(best model)
  b1 <- which.min(apply(smod,1,lgab,gl=lm,abic=AIC)) 
  b2 <- which.min(apply(smod,1,lgab,gl=lm,abic=BIC))
  b3 <- which.min(apply(smod,1,lgab,gl=glm,abic=AIC))
  b4 <- which.min(apply(smod,1,lgab,gl=glm,abic=BIC))
  return(c(b1,b2,b3,b4))
}


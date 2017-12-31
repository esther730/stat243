rm(list = ls())
library(simcausal)

random_selection <- function(num_of_gene, data_name) {
  result = read.table(text = "", col.names = names(data_name))
  for(i in 1:num_of_gene) {
    random <-rbern(ncol(data_name), 0.5)
    result[i,] = random
  }
  return(result)
}

Find_AIC <- function(num_of_gene, data_name, y_name) {
  df <- random_selection(num_of_gene, data_name)
  df_remove_y <- df[,-which(names(df) == y_name)]
  AIC_result <- c()
  y_ind = which(names(df) == y_name)
  for(i in 1:num_of_gene) {
    random_var <- which(df[i,] == 1) 
    selected_var_ind <- c(random_var[random_var != y_ind], y_ind)
    y_formula <- formula(paste(y_name, "~ .", sep = ""))
    mod <- lm(y_formula, data = data_name[,selected_var_ind])
    AIC_value <- AIC(mod)
    AIC_result <- c(AIC_result, AIC_value)
  }
  df[,"AIC"] <- AIC_result
  return(df)
}

result <- Find_AIC(10, mtcars, "mpg")
result_updated <- result[-which(names(result) == "AIC")]
vec1 <- as.numeric(result_updated[1,])
vec2 <- as.numeric(result_updated[2,])

mating <- function(vec1, vec2, split_point) {
  father1 <- vec1[1:split_point]
  mother1 <- vec1[(split_point + 1):length(vec1)]
  father2 <- vec2[1:split_point]
  mother2 <- vec2[(split_point + 1):length(vec2)]
  offspring1 <- c(father1, mother2)
  offspring2 <- c(father2, mother1)
  return(rbind(offspring1, offspring2))
}

mating(vec1, vec2, 6)[1,]


mutation_for_one <- function(vec, prob) {
  mutated <- rbern(length(vec), prob)
  mutated_ind <- which(mutated == 1)
  vec[mutated_ind] = abs(vec[mutated_ind] - 1)
  return(vec)
}

vec = c(0,0,0,0,1)
mutation_for_one(vec, 0.42)
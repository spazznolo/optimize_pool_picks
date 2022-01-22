
library(openxlsx)
library(tidyverse)
library(janitor)
library(GA)
library(beepr)

salary_cap <- 85
results_frame <- data.frame(matrix(NA, 0, 0))

parameter_frame <-
  expand_grid(
    pop_size = c(200, 400, 600),
    p_mutation = c(0.1, 0.25, 0.5),
    p_crossover = c(0.25, 0.5, 0.75)
  )

read_player_salaries <- function(sheet_) {

  read.xlsx('sets/player_salaries.xlsx', sheet = sheet_) %>%
    clean_names %>% 
    as_tibble %>%
    select(name, sal) %>%
    mutate(
      name = sub('(.*)\\,\\s+(.*)','\\2 \\1', toupper(name)),
      pos = sheet_)
  
}

#Define fitness function to ensure that solutions meet criteria
optimization_function <- function(x) {
  
  x = as.vector(x)
  current_solution_salary <- x %*% player_base$sal
  current_solution_pool_points <- x %*% player_base$pool_points
  
  # current_solution_pos <- player_base$pos[x==1]
  # 
  # idx_remove <- c(max(which(current_solution_pos == 'FWD')), 
  #                 max(which(current_solution_pos == 'DEF')), 
  #                 max(which(current_solution_pos == 'GLT')))
  # 
  # x[idx_remove] = 0
  # current_solution_pool_points <- x %*% player_base$pool_points
  
  if(  
    sum(player_base$pos[x==1] == "FWD")  != 11 |
    sum(player_base$pos[x==1] == "DEF")  != 6 |
    sum(player_base$pos[x==1] == "GLT")  != 3 |
    sum(initial_population != x)  > 4 |
    current_solution_salary > salary_cap
  )
    return(0) 
  
  else return(current_solution_pool_points)
  
}

find_optimal_ga_parameters <- function(i) {
  
  set.seed(33)
  
  pop_size_ <- parameter_frame[i,] %>% pull(pop_size)
  p_mutation_ <- parameter_frame[i,] %>% pull(p_mutation)
  p_crossover_ <- parameter_frame[i,] %>% pull(p_crossover)
  
  # run genetic algorithm
  ga_model <- ga(type="binary", nBits = nrow(player_base), fitness = optimization_function,
                 suggestions = initial_population_ga, popSize = pop_size_, monitor = TRUE,
                 pmutation = p_mutation_, pcrossover = p_crossover_,
                 maxiter = 5000, run = 2000, names = player_base$name)
  
  # save the solution
  solution <- as.vector(summary(ga_model)$solution)
  
  # view optimal picks
  iteration_result <- 
    player_base[solution == 1,] %>% 
    summarize(pool_points = sum(pool_points)) %>%
    mutate(
      iteration = i,
      pop_size = pop_size_,
      p_mutation = p_mutation_,
      p_crossover = p_crossover_
    ) 
  
  return(iteration_result)
  
}

current_solution_pos <- c('FWD', 'FWD', 'DEF', 'FWD', 'DEF')


c(0, 2, 3) %*% c(0, 2, 3)



library(openxlsx)
library(tidyverse)
library(janitor)
library(GA)

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
  
  current_solution_salary <- x %*% player_base$sal
  current_solution_pool_points <- x %*% player_base$pool_points
  
  if(  
    sum(player_base$pos[x==1] == "FWD")  != 11 |
    sum(player_base$pos[x==1] == "DEF")  != 6 |
    sum(player_base$pos[x==1] == "GLT")  != 3 |
    current_solution_salary > salary_cap
  )
    return(0) 
  
  else return(current_solution_pool_points)
  
}
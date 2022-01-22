
results_frame %>%
  count(iteration, wt = pool_points) %>%
  arrange(desc(n))

# find iteration with max points
pop_size_ = parameter_frame[3,] %>% pull(pop_size)
p_mutation_ = parameter_frame[3,] %>% pull(p_mutation)
p_crossover_ = parameter_frame[3,] %>% pull(p_crossover)

set.seed(33)
# run genetic algorithm
ga_model <- ga(type="binary", nBits = nrow(player_base), fitness=optimization_function,
              suggestions = initial_population_ga, popSize = pop_size_, monitor = TRUE,
              pmutation = p_mutation_, pcrossover = p_crossover_,
              maxiter=5000, run = 2000, names = player_base$name)

# save the solution
solution <- as.vector(summary(ga_model)$solution)

# plot of solution progress
plot(GAmodel)

# view optimal picks
iteration_result <- 
  player_base[solution == 1,] %>% 
  select(name, sal, pool_points)

iteration_result %>%
  anti_join(initial_picks_1, by = 'name')

initial_picks_1 %>%
  anti_join(iteration_result, by = 'name')




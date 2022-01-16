
# set salary cap
salary_cap <- 85

# save initial picks
initial_picks <-
  player_base %>% 
  mutate(idx = 1:n()) %>% 
  filter(pos == 'DEF') %>% 
  pull(idx)

# set initial population as initial picks
initial_population <- rep(0,nrow(player_base))
initial_population[initial_picks] = 1

# run genetic algorithm
GAmodel <-ga(type="binary",nBits = nrow(player_base), fitness=optimization_function,
             suggestions = initial_population, popSize=100, monitor = TRUE,
             pmutation = .3, pcrossover = .75,
             maxiter=10000, names = player_base$name)

# save the solution
solution <-summary(GAmodel)$solution

# view optimal picks
player_base[as.vector(solution)==1,]

# plot of solution progress
plot(GAmodel)




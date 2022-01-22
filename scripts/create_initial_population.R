
# set salary cap

initial_picks_1 <- 
  read.xlsx('sets/initial_picks_1.xlsx', startRow = 5) %>%
  clean_names %>%
  as_tibble %>%
  mutate(
    name = sub('(.*)\\,\\s+(.*)','\\2 \\1', toupper(player)),
    name = ifelse(name == 'ELIAS PETERSSON', 'ELIAS PETTERSSON', name),
    name = ifelse(name == 'JOSH NORRI', 'JOSH NORRIS', name),
    status = 1) %>%
  select(name)

# save initial picks
initial_picks <-
  player_base %>% 
  mutate(idx = 1:n()) %>% 
  inner_join(initial_picks_1, by = 'name') %>%
  pull(idx)

initial_picks_ga <-
  player_base %>% 
  mutate(idx = 1:n()) %>% 
  inner_join(initial_picks_1 %>%
               mutate(
                 name = sub('CONNOR MCDAVID', 'AUSTON MATTHEWS', name),
                 name = sub('LEON DRAISAITL', 'MIKKO RANTANEN', name)
                 ), by = 'name') %>%
  pull(idx)

# set initial population as initial picks
initial_population <- rep(0,nrow(player_base))
initial_population[initial_picks] = 1

initial_population_ga <- rep(0,nrow(player_base))
initial_population_ga[initial_picks_ga] = 1




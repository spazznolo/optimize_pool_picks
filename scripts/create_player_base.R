

## load data

skater_points <- 
  read.xlsx('sets/rest_of_season_projections.xlsx', sheet = 'Skaters') %>%
  clean_names %>% 
  as_tibble %>%
  mutate(
    pool_points = (3*g) + (2*a) + (2*gwg),
    name = toupper(player)
  ) %>%
  select(name, pool_points) %>%
  drop_na %>%
  arrange(desc(pool_points))

goalie_points <- 
  read.xlsx('sets/rest_of_season_projections.xlsx', sheet = 'Goalies') %>%
  clean_names %>% 
  as_tibble %>%
  mutate(
    pool_points = (4*w) - l + (3*so) + otl - (0.25*ga) + (0.1*sv),
    name = toupper(player)
  ) %>%
  select(name, pool_points) %>%
  drop_na %>%
  arrange(desc(pool_points))

player_salaries <-
  map_dfr(c('FWD', 'DEF', 'GLT'), read_player_salaries) %>%
  filter(!(name == 'SEBASTIAN AHO' & pos == 'DEF'))

player_base <-
  bind_rows(skater_points, goalie_points) %>%
  full_join(player_salaries, by = 'name') %>%
  drop_na





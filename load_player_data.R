

## load data

skater_points <- 
  read.xlsx('rest_of_season_projections.xlsx', sheet = 'Skaters') %>%
  clean_names %>% 
  as_tibble %>%
  mutate(
    pool_points = (3*g) + (2*a) + (2*gwg) + sog,
    name = toupper(player)
  ) %>%
  select(name, pool_points) %>%
  drop_na %>%
  arrange(desc(pool_points))

player_salaries <-
  map_dfr(c('FWD', 'DEF'), read_player_salaries)

player_base <-
  skater_points %>%
  full_join(player_salaries, by = 'name') %>%
  drop_na

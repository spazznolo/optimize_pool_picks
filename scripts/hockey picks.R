library(openxlsx)
library(tidyverse)
library(janitor)
library(GA)
library(readxl)

#skater points table

skater_points <- read.xlsx('rosproj.xlsx', sheet='Skaters') %>% 
  mutate(
    pool_points=(3*g)+(2*a) + (2*gwg), 
    name=toupper(player)) %>%
  select(name, pool_points) %>% 
  drop_na %>% 
  arrange(desc(pool_points)) %>% view()

#goalie points table

goalie_points <-read.xlsx('rosproj.xlsx', sheet='Goalies') %>% 
  mutate(
    pool_points=(4*w)-l + otl -(0.25*ga)+(0.1*sv), 
    name=toupper(player)) %>%
  select(name, pool_points) %>% 
  drop_na %>% 
  arrange(desc(pool_points))

#combine goalies and players 

player_points <- bind_rows(goalie_points, skater_points) 



#player salary table and modify "name"

player_salary<-excel_sheets("picklist.xlsx") %>% map_df(~read_xlsx("picklist.xlsx",.)) %>% 
  select(SAL, Name, Pos) %>% 
  clean_names()%>% 
  separate("name", c('Name', 'Surname')) %>% 
  unite("lowername",'Surname':'Name', sep = " ") %>% 
  mutate("name"=toupper(lowername)) %>% 
  select(-lowername) %>% view()


#combine salary and points table
merge(player_points, player_salary, by='name') %>% view()


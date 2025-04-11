library(rvest)
library(tidyverse)


getText <- function(x) {
  page %>% 
    html_nodes(x) %>% 
    html_text()
}


# How teams are named in dataset
key <- mmsubset %>% 
  select(Team, Season)

wins <- tibble()

# Go through all years
for (year in c(2002:2019, 2021:2025)) {
  link <- paste0("https://www.sports-reference.com/cbb/postseason/men/", year, "-ncaa.html")
  page <- read_html(link)
  
  zerowins <- getText(".round:nth-child(1) span+ a")[1:64]
  onewin <- getText(".round:nth-child(2) span+ a")[1:32]
  twowins <- getText(".round:nth-child(3) span+ a")[1:16]
  threewins <- getText(".round:nth-child(4) span+ a")
  fourwins <- getText(".round:nth-child(5) span+ a")
  fivewins <- getText(".round:nth-child(2) span+ a")[33:34]
  sixwins <- getText(".round:nth-child(3) span+ a")[17]
  
  temp <- tibble(Team = zerowins, 
                 Season = year,
                 wins = 0)
  
  temp <- temp %>% 
    mutate(wins = case_when(Team %in% sixwins ~ 6,
                            Team %in% fivewins ~ 5,
                            Team %in% fourwins ~ 4,
                            Team %in% threewins ~ 3,
                            Team %in% twowins ~ 2,
                            Team %in% onewin ~ 1,
                            .default = 0))
  
  
  wins <- wins %>% 
    rbind(temp)
  Sys.sleep(5)
  
}

# Names that need to change
adjust <- wins %>% 
  select(Team) %>% 
  distinct() %>% 
  anti_join(key) %>% 
  as.vector()

# Change names
wins <- wins %>%
  mutate(Team = case_when(
    Team == "St. John's (NY)" ~ "St. John's",
    Team == "McNeese State" ~ "McNeese",
    Team == "Penn" ~ "Pennsylvania",
    Team == "Pitt" ~ "Pittsburgh",
    Team == "Miami (FL)" ~ "Miami",
    Team == "UCSB" ~ "UC Santa Barbara",
    Team == "St. Joseph's" ~ "Saint Joseph's",
    Team == "ETSU" ~ "East Tennessee State",
    Team == "UNC" ~ "North Carolina",
    Team == "FDU" ~ "Fairleigh Dickinson",
    Team == "Southeastern Louisiana" ~ "SE Louisiana",
    Team == "Albany (NY)" ~ "UAlbany",
    Team == "American" ~ "American University",
    Team == "LIU" ~ "Long Island University",
    Team == "St. Peter's" ~ "Saint Peter's",
    Team == "Loyola (MD)" ~ "Loyola Maryland",
    Team == "UMass" ~ "Massachusetts",
    Team == "UC-Irvine" ~ "UC Irvine",
    Team == "UC-Davis" ~ "UC Davis",
    Team == "College of Charleston" ~ "Charleston",
    Team == "Loyola (IL)" ~ "Loyola Chicago",
    Team == "SIU-Edwardsville" ~ "SIU Edwardsville",
    Team == "UC-San Diego" ~ "UC San Diego",
    .default = Team
  ))

saveRDS(wins, "wins.rds")

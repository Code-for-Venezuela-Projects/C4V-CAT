library(googlesheets4)
library(tidyverse)

covid <- "https://docs.google.com/spreadsheets/d/1pPcOeGV5iIAwS2-3OFoQXnx0yr_PiXmKjf_3oZIDl7s/edit?usp=sharing"

covid_table <- range_read(covid) %>% 
  janitor::clean_names() %>% 
  as_tibble() 



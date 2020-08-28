#### Venezuela Funcitions Luis ------------------------------

library(lubridate)
library(tidyverse)
library(glue)
library(plotly)
library(zoo)


# Notes 

# Not able to find the pct of available beds or er beds. Should be in a different fucntion in previous project


# Manipulation Functions

vzla_cases_by_state <- function(dataset){
  
  dataset %>%
    group_by(federal_entity, report_day) %>%  
    summarise(state_cases = sum(covid_cases_confirmed_rapid_test_hospitalized_count)) %>%
    mutate(cumulative_cases = cumsum(state_cases), # Cumulative cases measure
           daily_cases = state_cases, # Daily cases renamed
           roll_7days = round(replace_na(rollmean(state_cases, 7, na.pad = T),0))) %>% 
    mutate_if(is.integer, as.numeric) %>% # Converting all integers to numeric
    ungroup() %>%
    select(federal_entity, report_day, cumulative_cases, daily_cases, roll_7days) %>%
    arrange(report_day)
  
}

# Working - Cumulative, Daily and 7 day Rolling Average per State
#vzla_cases_by_state(covid_table) 


vzla_services_by_hospital <- function(dataset){
  
  dataset  %>% # Daily cases renamed
    mutate_if(is.integer, as.numeric) %>% # Converting all integers to numeric
    distinct(hospital_code, report_day, .keep_all = TRUE) %>%  # Repeated variables found, keeping unique observations
    na_if(., 0)  # Replace 0 with Nulls
    
}



loess_one_state_visz <- function(dataset, state){
  
  # Data Transformation 
  dataset <- vzla_cases_by_state(dataset)
  
  # Data Visualization
  plot <- dataset %>%
    filter(federal_entity == state) %>%
    ggplot(aes(report_day, cumulative_cases)) +
    geom_line() +
    labs(title = paste0('Cumulative Covid Cases in: ', state),
         y = 'Cumulative Cases',
         x = 'Date') +
    geom_smooth(method = 'loess') +
    theme_minimal()
  
  plot
}

#loess_one_state_visz(covid_table, "Distrito Capital")


# Trend Line, Bar graph and Loess per State

bar_one_state_visz <- function(dataset, state){
  
  dataset <- vzla_cases_by_state(dataset)
  
  plot <- dataset %>%
    filter(federal_entity == state) %>%
    ggplot(aes(x = report_day, y = cumulative_cases)) +
    geom_line(aes(y = cumulative_cases)) +
    geom_line(aes(y = roll_7days), color = '#2d3252') +
    geom_col(aes(y = daily_cases)) +
    labs(title = paste0('Cumulative Covid Cases in: ', state),
         y = 'Cumulative Cases',
         x = 'Date') +
  geom_smooth(method = 'loess') +
    theme(axis.text.x = element_text(angle = 90))
  
  
  ggplotly(plot) 
}

# Testing
#bar_one_state_visz(covid_table, "Nueva Esparta")


# Numerical Variables Service Plot

## Work-in-Progress

### Figure out how to make select a specific type of service, currently only works with icu_beds_count
### Figure out how to select date, we do not want to plot the whole data once is not static.


services_icu_beds <- function(dataset, estado){ 
  
  
  plot_icu <- vzla_services_by_hospital(dataset) %>%
    filter(federal_entity == estado) %>% # Selecting desired States 
    mutate(hospital_code = str_remove(hospital_code, '. Dtto'),
           pct_icu_available_cat = case_when(pct_icu_available < 0.25  ~ "Capacidad Baja < 25%",
                                             pct_icu_available < 0.5 ~ "Capacidad Media < 50%",
                                             TRUE ~ "Capacidad Alta > 51%")) %>% # Removing ugly hospital name
    ggplot(aes(report_day, 
               hospital_code,
               color = pct_icu_available_cat,
               text = glue('Fecha: {report_day}\n Numero de UCI: {icu_beds_count}\n UCI Disponible: {pct_text_icu}'))) +
    geom_point(alpha = 0.8, show.legend = F, size = 3) +
    scale_color_manual(values = c("Capacidad Media < 50%" = 'Yellow',  # Proper colors for status
                                  "Capacidad Alta > 51%"= '#2d3252', 
                                  "Capacidad Baja < 25%" = 'Dark Red')) +
    labs(title = paste0("Disponibilidad Unidades Cuidados Intensivos en: ", estado),
         y = NULL,
         x = NULL) + 
    theme_c4v()
  
  ggplotly(plot_icu, tooltip = 'text')
  
}

# Testing Function - It's Working 
#services_icu_beds(covid_table, "Distrito Capital")


services_er_beds <- function(dataset, estado){ 
  
  plot_er <- vzla_services_by_hospital(dataset) %>%
    filter(federal_entity == estado) %>% # Selecting desired States 
    mutate(hospital_code = str_remove(hospital_code, '. Dtto'),
           pct_er_available_cat = case_when(pct_er_available < 0.25  ~ "Capacidad Baja < 25%",
                                            pct_er_available < 0.5 ~ "Capacidad Media < 50%",
                                            TRUE ~ "Capacidad Alta > 51%")) %>% # Removing ugly hospital name
    ggplot(aes(report_day, 
               hospital_code,
               color = pct_er_available_cat,
               text = glue('Fecha: {report_day}\n Numero de camas ER: {er_beds_count}\n ER camas Disponible: {pct_text_er}'))) +
    geom_point(alpha = 0.8, show.legend = F, size = 3) +
    scale_color_manual(values = c("Capacidad Media < 50%" = 'Yellow',  # Proper colors for status
                                  "Capacidad Alta > 51%"= '#2d3252', 
                                  "Capacidad Baja < 25%" = 'Dark Red')) +
    labs(title = paste0("Disponibilidad Camas de Emergencia en: ", estado),
         y = NULL,
         x = NULL) + theme_c4v()
  
  ggplotly(plot_er, tooltip = 'text')
  
}


#services_er_beds(covid_table, 'Apure')




# Categorical Variables Service Plot

services_cat_func <- function(dataset, estado, servicio){ 
  
  
  dataset <- vzla_services_by_hospital(dataset) # Manipulation of dataset  
  
  service_plot_cat <- dataset %>%
    filter(federal_entity == estado) %>% # Selecting desired States
    mutate(hospital_code = str_remove(hospital_code, '. Dtto')) %>% # Removing ugly hospital names
    ggplot(aes_string('report_day', 
               'hospital_code',
               color = servicio)) +
    geom_point(alpha = 0.8, show.legend = F, size = 3) +
    scale_color_manual(values = c('Hay de manera intermitente' = 'Yellow',  # Proper colors for status
                                  'Sí hay' = '#2d3252', 
                                  'No hay' = 'Dark Red')) +
    labs(title = paste0("Disponibilidad de: ", servicio, " en ", estado),
         y = NULL,
         x = NULL) +
    theme_c4v() 
  
  ggplotly(service_plot_cat)
  
}

#vzla_services_by_hospital(covid_table)
#services_cat_func(covid_table, 'Distrito Capital', 'power_availability')





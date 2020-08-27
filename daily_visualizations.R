# Venezuela functions Samir  -----------------------------------------------------

### Vzla functions 

plot_daily_covid <- function(vzla_data, estado){
  
  if(estado=='Todos los estados'){
    subset_vzla_data = vzla_data
  } else{
    subset_vzla_data = vzla_data[vzla_data$federal_entity==estado,]
  }
  
  covid_vzla_data <- subset_vzla_data[, c('covid_cases_confirmed_test_hospitalized_count',
                                          'report_day','ira_cases_in_respiratory_count',
                                          'covid_cases_confirmed_rapid_test_hospitalized_count')]
  
  colnames(covid_vzla_data) <- c('Covid Confirmado','fecha','IRA', 'Covid Prueba Rapida')
  
  
  if(estado=='Todos los estados'){
    title_name = paste('Casos Diarios Sospechados de Covid: Venezuela')
  } else{
    title_name = paste('Casos Diarios Sospechados de Covid:', estado, sep=' ')
  }
  
  
  df = melt(covid_vzla_data, id.vars = 'fecha') %>% mutate(fecha = mdy(fecha))
  
  p = ggplot(data=df, aes(x=fecha, y=value, fill=variable)) + geom_bar(stat='identity') + 
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
    theme_c4v() + 
    labs(title=title_name, x='',y='# Casos', fill='Tipo de Caso') +
    theme(
      plot.title = element_text(color="black", size=24, face="bold", hjust = 0.5),
      axis.title.x = element_text(color="black", size=20, face="bold"),
      axis.title.y = element_text(color="black", size=20, face="bold"),
      axis.text.x = element_text(color="black", size=10, face="bold", angle=90),
      axis.text.y = element_text(color="black", size=10, face="bold"),
      legend.text = element_text(color="black", size=12, face="bold"),
      legend.title = element_text(color="black", size=20, face="bold"),
      legend.position = 'bottom')
  
  ggplotly(p)
  
}




plot_daily_resources <- function(vzla_data,estado){
  
  if(estado=='Todos los estados'){
    subset_vzla_data = vzla_data
  } else{
    subset_vzla_data = vzla_data[vzla_data$federal_entity==estado,]
  }
  
  res_vzla_data <- subset_vzla_data[, c('icu_beds_count',
                                        'report_day','er_beds_count',
                                        'respiratory_units_count')]
  
  colnames(res_vzla_data) <- c('Camas: Cuidados Intensivos','fecha','Camas: Sala de Emergencia', 'Unidades Respiratorias')
  
  df = melt(res_vzla_data, id.vars = 'fecha') %>% mutate(fecha = mdy(fecha))
  
  
  if(estado=='Todos los estados'){
    title_name = paste('Recursos Medicos Disponibles: Venezuela')
  } else{
    title_name = paste('Recursos Medicos Disponibles:', estado, sep=' ')
  }
  
  p = ggplot(data=df, aes(x=fecha, y=value, fill=variable)) + 
    geom_bar(stat='identity') + 
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme_c4v() + 
    labs(title=title_name, x='',y='# Unidades', fill='Tipo de Recursos') +
    theme(
      plot.title = element_text(color="black", size=24, face="bold", hjust = 0.5),
      axis.title.x = element_text(color="black", size=20, face="bold"),
      axis.title.y = element_text(color="black", size=20, face="bold"),
      axis.text.x = element_text(color="black", size=10, face="bold", angle = 90),
      axis.text.y = element_text(color="black", size=10, face="bold"),
      legend.text = element_text(color="black", size=12, face="bold"),
      legend.title = element_text(color="black", size=20, face="bold"),
      legend.position = 'bottom') 
  
  ggplotly(p)
}







